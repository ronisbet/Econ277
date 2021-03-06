##############################
# Econ 277: Problemset #1
# Aurthor: Robert Nisbet
# Date Created: 2/12/19
# Date Last Modified: 2/12/19
# Data File Location: /Users/robertnisbet/Desktop/Econ 277 Data
# Shortened -> ~/Desktop/Econ 277 Data
# Notes:
##############################
# Set Directories
##############################
# Set working Directory 
setwd("~/Desktop/Econ277")
# Raw Data Directory 
rawdata_dir<-"~/Econ277/RawData"
# Raw Data Directory 
images_dir<-"~/Econ277/Images"
##############################  
# Load Packages
##############################
install.packages("tidyverse")
library(tidyverse)
library(readr)
##############################
# Load Data - Dataset #1.1
##############################
# Read CSV Data
data1 <- read.csv("~/Desktop/Econ277/Data/PSET1/Dataset_1.1.csv")
##############################
# Load Data - Dataset #1.2
##############################
# Read CSV Data
data2 <- read.csv("~/Desktop/Econ277/Data/PSET1/Dataset_1.2.csv")
##############################
# Load Data - CountyRRData
##############################
# Read CSV Data
DataRR <- read.csv("~/Desktop/Econ277/Data/PSET1/CountyRRData.csv")
#############################

# Part 1: Dataset Construction

##############################
# Adjust Farm Data for inflation
##############################

data3 <-merge(data1, data2, by="YEAR")

data3 <- transform(data3, adjfarmval = (FAVAL/(Annual.Average/100)))
##############################
# Create a merged dataset with Data3 and DataRR by County
##############################
countydata <-merge(data3, DataRR, by="FIPS")
##############################

# Question 1 - Question 1: What is a unit of observation in the data?

# Answer 1: A unit of measure of "countydata" is kilometers. Specificly, the variable "RRinitialtotaldist" is measured in kilometers (see Donaldson and Hornbeck, 2016).

##############################
# Create dummy variable for railroad implementation
##############################
countydata$RR_Dummy = ifelse(
  (countydata[ ,"RRinitialtotaldist"] == 0 ) ,
  0 ,
  1) 
# Where NO = 0 and YES = 1 
###############################
# Create a subset of the data for obs. after 1930
###############################
Countydata_P1930 <- subset(countydata,YEAR <= 1930)

# "P1930" is shorthand for "Pre 1930"

# Question 2: How many observations are in your dataset after this adjustment? 

# Answer 2: We have 30770 ovservations of 7 variables  

# Question 3: How many missing land values do you have?

# Answer 3: There are 5521 missing land variables 
#############################

#Part 2: Summary Statistics

##############################
summary(Countydata_P1930, na.rm = TRUE)

# Question 4: What is the mean inflation adjusted farm value?

# Answer 4: The mean inflation adjusted farm value is 104.771.

# Question 5: What percent of counties did not receive any railroad?

print(sum(1,-0.9532))
# Answer 5: 4.68 percent of counties did not revieve any railroad.

# Question 6: What is the average kms of railroad?

# Answer 6: 126.35 kms.

# Question 7: What is the standard deviation of the railroad kms?

sapply(Countydata_P1930, sd)

# Answer 7: 1.016180e+02 kms.

#############################

# Part 3: Scatterplots with Binary RR Treatment

#############################
# Create Subsets of data for RR implementation
#############################
Countydata_P1930$LNadjfarmval = log(Countydata_P1930$adjfarmval) # create new collum for log of adjusted farm values

Countydata_P1930_NORR <- subset(Countydata_P1930, RR_Dummy == 0)
Countydata_P1930_YESRR <- subset(Countydata_P1930, RR_Dummy == 1)
#############################
# 3.1. Create a scatterplot with a local polynomial line for the ever RR group.
#############################

install.packages("ggplot2")

p1 <- ggplot(data = Countydata_P1930_YESRR, mapping = aes(x = YEAR, y = LNadjfarmval)) + geom_point() + geom_smooth()
p1
#############################

# 3.2. Create a scatterplot with a local polynomial line for the never RR group.

#############################

p2 <- ggplot(data = Countydata_P1930_NORR, mapping = aes(x = YEAR, y = LNadjfarmval)) + geom_point() + geom_smooth()
p2
#############################

# 3.3. Create a scatterplot with the entire dataset in one graph. This should have the points
# plotted in different colors and the lines in different colors. 
# This is so we can start to see how different the two areas are.

#############################
Countydata_P1930$Railraod_Implementation <- as.factor(Countydata_P1930$RR_Dummy) # convert RR dummy to factor variable
head(Countydata_P1930)

p3 <- ggplot(data=Countydata_P1930, aes(x=YEAR, y=LNadjfarmval, color=Railraod_Implementation, shape=Railraod_Implementation)) +
  geom_point() + 
  geom_smooth()+
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  theme(legend.position="top")

p3

# Part 4: Basic Difference – in – Difference (DID) Models

#############################

# 4.1. Generate a new variable, after, which is equal to 1 if the year>1885 and 0 if year<=1885.

#############################

Countydata_P1930[ , "1885_Marker"] <- ifelse(
  (Countydata_P1930[ ,"YEAR"] > 1885 ) ,
  1 ,
  0) 
# Where NO = 0 and YES = 1

#############################

# 4.2. Generate the interaction term for the DID

# Generate a term that captures the difference between the treatment and control group after treatment
# Such that, Variable = (Treatment X Post 1885)

#############################

# Define Variables
#############################

RR <- Countydata_P1930$RR_Dummy
TP <- Countydata_P1930$'1885_Marker'

#############################
# Create Interaction Variable
#############################
Countydata_P1930$RRxTP <- RR * TP
#############################

# 4.3 Run a standard DID model using adjusted farm values as the outcome

#############################

DID_1 = lm(adjfarmval ~ RR_Dummy + `1885_Marker` + RRxTP, data = Countydata_P1930)
summary(DID_1)

#############################

# 4.3 Run a standard DID model using adjusted farm values as the outcome

#############################

Countydata_P1930$LNadjfarmval = log(Countydata_P1930$adjfarmval) # create new output collum for LN 

DID_2 = lm(LNadjfarmval ~ RR_Dummy + `1885_Marker` + RRxTP, data = Countydata_P1930)
summary(DID_2)

#############################

# Question 8: Interpret the four coefficients of interest in this regression.

# Answer 8: The four coeficients are: the intercept, The rail road implementation dummy (RR_Dummy), the time period dummy (1885_Marker), and the interaction coeficient (RRxTP).
# The intercept of 3.06 extimates 3.06 units of the natural log of farm productivity in the absense of railroads and prior to 1850 (significant at less than 1%). The "RR_Dummy" coefiencient of 0.47 estimates a gain of 0.47 unites of the natural log of farm productivity when railroads are implemented prior to 1850 (significant at less than 1%)
# The "1885_Marker" coeficient of 0.60 extimates a gain of 0.81 unites of the natural log of farm productivity after 1885 between 1850 and 1930 (significant at less than 1%).
# The RRxTP coeficient estimates a gain of 0.22 unites of the natural log of farm productivity when railroads are implemented after 1885 (significant at less than 1%).

# Question 9: Discuss the merits of this model. Do you think that this is a reasonable DID model based on the fundamental assumptions?

# Answer 9: Probably the most important assumption of the DID model to ensure its interal validity is the parallel trend assumtion. 
# That is, in the absence of treatment (eg railroads) the difference between the treatmet and control group is constant over time. 
# Naturally, the smaller the time period tested, the more likley this assumption is to hold (reduced risk of omited variabe bias).
# Because we our data is focused on the 80 year period between 1850 and 1930, it is reasionalbe to assume that, in the grand scheme of time,
# this is a small enough time frame to allow the parallel trend assumption to hold. 
# Another assumption is that we do not have any spillover effects between our time period and railroad variables (Stabel united treatment value assumption).
# This assumption holds because the decision to implement railroads took place prior to 1885 (for our data).
# Additionally, when we regress DID model with the natural log of farm productivity as our output, all of our coefiecients are siginiifcant at less than 1%.
# It may be the case that a unit change in our independent variables leads to a percent change in our dependent variable. 

#############################

# Part 5: Generate New KM Based Graphs

#############################

# 5.1. Generate a new measure of railroad km. 
# This new measure should equal the Ln(RRinitialtotaldist + 1)

#############################

Countydata_P1930$NewRR = log(Countydata_P1930$RRinitialtotaldist + 1)

#############################

# 5.2 Create two new scatterplots with polynomial lines. 
# For the first plot let Y = Ln(Adjusted Farm Values) & X = RRinitialtotaldist. 
# For the second plot let Y = Ln(Adjusted Farm Values) , X = your new log measure.

#############################

P4 <- ggplot(data = Countydata_P1930_YESRR, aes(x= RRinitialtotaldist, y= LNadjfarmval)) + 
  geom_point()+
  geom_smooth() # origional RR distance measure

P4

Countydata_P1930_YESRR <- subset(Countydata_P1930, RR_Dummy == 1) # add "NEW RR" column to Railroad group

P5 <- ggplot(data = Countydata_P1930_YESRR, aes(x= NewRR, y= LNadjfarmval)) + 
  geom_point()+
  geom_smooth() # New RR distance measure

P5
#############################

# Question 10: Describe the patterns in the two figures above.

# Answer 10: The first measure of railroad distance shows large returns to farm productivity in the intitial levels of railroad implementation (distance between 0 and 250 Km). 
# Ther is a significant amount of error in the later portions of the scatter plot (distances greater than 750 Km).
# The second measure of railroad distance, Ln(RRinitialtotaldist + 1), shows moderate returns to farm productivity initialy, but show a sharp increase just after the 5 mark. 
# There is a significtant amount of error in the early portions of the scatter plot (2 x units and earilier).

#############################

# Part 6: Run Simple Bivariate Regressions

#############################

# 6.1 For the first regression, let Y = Ln(Adjusted Farm Values) & X = RRinitialtotaldist.

#############################

Reg_1 = lm(LNadjfarmval ~ RRinitialtotaldist , data = Countydata_P1930)
summary(Reg_1)

#############################

# 6.1 For the first regression, let Y = Ln(Adjusted Farm Values) & X = RRinitialtotaldist.

#############################

Reg_1 = lm(LNadjfarmval ~ RRinitialtotaldist , data = Countydata_P1930)
summary(Reg_1)

#############################

# 6.2 For the second regression, let Y = Ln(Adjusted Farm Values) , X = your new log measure.

#############################

Reg_2 = lm(LNadjfarmval ~ NewRR , data = Countydata_P1930)

summary(Reg_2)

############################

# Question 11: Interpret the coefficients of interest from both models.

# Answer 11: In model 1, an intercept of 3.74 estimates 3.74 units of the natural log of farm productivity in the absense of railroad implementation (significant at less than 1%).
  # A coeficient of 2.62e-03 for initial railroad distance (RRinitialtotaldist) estimates a gain of 2.62e-03 in the natural log of farm productivity with each km increace in railroad for a county (significant at less than 1%).
  # In model 2, an intercept of 3.10 estimates 3.10 units of the natural log of farm productivity in the absense of railroad implementation (significant at less than 1%).
  # A coeficient of 0.21 for our new rail road measure (NewRR) estimates a gain of 0.21 in the natural log of farm productivity with each unit incease in the new railroad measure  (significant at less than 1%).

# Question 12: Discuss the type of endogeneity concerns you would have regarding these models.

# Answer 12: Becasue farm productivity relies on a variety of factors outside of railroads, and railroad development and implementation have its own exogenious promoters, there are certinly some exogeneity concerns with this model.
  # First, railway development is more likely in areas that are well inhabited and popular. It is reasonable to assume that these polular areas may have more suiltible land for farming, and therefore will see higher levels of farm productivity.
  # Our model, would atribute this productivity gain to railroad implementation, were the gain should actually be atributed to more suitable land. Additionaly, railways being tourists and more citizens, opening up local markets, this will raise the value the county land.

