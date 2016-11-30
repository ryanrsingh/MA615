library(ggplot2)
library(tidyr)
library(dtplyr)

#Code for tidying data
vehicles = read.csv("vehicles.csv")
#The goal of tidying this dataset is to analyze in more detail, more about the Single Fuel Vehicles.

#Preliminary observation of column headers identify 5 unnecessary columns of data, 
#(createdon; modifiedon;startStop; phevCity; phevHwy; phevComb).
#The following code in the subsection removes those 5 columns by setting them to NULL.
base1 <- vehicles[c(1:77,81:83)]

#Filter 1 removes datalines that have any annual fuel consumption in terms of the second fuel type.
filter1 <- subset(base1, barrelsA08 < 0.0001)

#Filter 2 & 3 ensure that datalines for both cityA08 and cityA08U, and for both co2A and co2TailpipeAGpm, there is no consumption of the fuel type 2.
filter2 <- subset(filter1, cityA08 < 0.0001| cityA08U < 0.0001)

filter3<- subset(filter2, co2A <0 | co2TailpipeAGpm < 0.0001)

#Removing other unnecessary columns (7,8,13,14,18,19,30,37,38,55,57,60,62)
base2 <- filter3[,-c(7,8,13:14,18:19,30,37:38,55,57,60,62)]

#Creating a regression for Annual Fuel cost(fuelCost08) based on a number of factors:
#barrels08, city08, city08U, comb08, comb08U, cylinders, displ, drive, make, trany, year, co2,
#co2TailpipeGPM, highway08, highway08U, UCity, UHighway

#Split data set into 2, one for formulation of regression, the other to test.
## 50% of the sample size
smp_size <- floor(0.50 * nrow(base2))

## set the seed to make your partition reproductible
set.seed(123)
ind <- sample(seq_len(nrow(base2)), size = smp_size)

train <- base2[ind, ]
test <- base2[-ind, ]

#regress the training set
LM1 <- lm(fuelCost08 ~ barrels08+city08+city08U+comb08+comb08U+cylinders
          +displ+drive+make+trany+year+co2+co2TailpipeGpm+highway08+highway08U
          +UCity+UHighway,data = train)
ss <- coef(summary(LM1))
ss_sign <- ss[ss[,"Pr(>|t|)"]<0.05,]
printCoefmat(ss_sign)

#From the summary, we can see that the significant variables in the multiple linear regression are:
#barrels08, city08, city08U, comb08, comb08U, cylinders, displ, drive, make, year, 
#co2TailpipeGpm, highway08, highway08U, UCity.
LM2 <- lm(fuelCost08 ~ barrels08+city08+city08U+comb08+comb08U+cylinders+displ+drive
+make+year+co2TailpipeGpm+highway08+highway08U+UCity, data= train)
predict(LM2, test)
#(HELPPP)

#Creating a regression based on the whole dataset
LMfull <- lm(fuelCost08 ~ barrels08+city08+city08U+comb08+comb08U+cylinders+displ+drive+make
             +trany+year+co2+co2TailpipeGpm+highway08+highway08U+UCity+UHighway,data = base2)
ssF <- coef(summary(LMfull))
ssF_sign <- ssF[ssF[,"Pr(>|t|)"]<0.05,]
printCoefmat(ssF_sign)

#This offers a different model based on the entirety of the dataset produced, 
#as compared to taking a sample of the original data, running a regression and testing it with another 
#version.To note, catergorical variables in the regression model will have one or more levels 
#less than the dataset, as the regression method utilizes one of the levels as a reference point.

#Looking at relationships between variables and annual fuel costs

#1. Fuel cost vs cylinders
#base2$cylinders <- as.factor(base2$cylinders)
base2$fuelCost08 <- as.integer(base2$fuelCost08)
CYL <- base2$cylinders
AFC <- base2$fuelCost08
plot.1 <- qplot(CYL, AFC, 
     main = "Relationship between Cylinders and Annual Fuel Cost", 
     xlab ="Cylinders", ylab = "Annual Fuel Cost")
model.1 <- lm(AFC ~ CYL)
coef(model.1)
plot.1 + geom_abline(intercept=coef(model.1)[1],
                     slope=coef(model.1)[2])
#2. Relationship between Make and Annual Fuel Cost
newbase$Make <- as.character(newbase$fuelCost08)
MAKE <- newbase$make
AFC <- newbase$fuelCost08
plot.2 <- qplot(MAKE, AFC, 
                main = "Relationship between Make and Annual Fuel Cost", 
                xlab ="Make", ylab = "Annual Fuel Cost")
model.2 <- lm(MAKE ~ CYL)
coef(model.2)
plot.3 <- plot.2 + geom_abline(intercept=coef(model.2)[1],
                               slope=coef(model.2)[2])
suppressWarnings(print(plot.3))
#From this plot, we can see that it is a really dense plot of all the cars recorded in the dataset.
#We can reduce the number of Makes represented in the plot, to really analyze specifically a brand of car, as well as further break it down into the individual models of each brand.
#These analyses can be done simply by creating smaller datasets based on the make, and then using q plot to plot the graphs again.
