#4.
install.packages('MASS')
library(MASS)
install.packages('dplyr')
library(dplyr)
install.packages('tidyr')
library(tidyr)
library(ggplot2)
install.packages('ascii')
library(ascii)
install.packages('lubridate')
library(lubridate)
install.packages('splines')
library(splines)
install.packages('nlme')
library(nlme)
library(mgcv)

PSDS_PATH=file.path('/Users/joohyunyoon/workspace/Loan Prediction/')
zhvi <- read.csv(file.path(PSDS_PATH, 'data','County_Zhvi_AllHomes.csv'))
house <- read.csv(file.path(PSDS_PATH, 'data', 'house_sales.csv'), sep='\t')

#Multiple Linear Regression
View(house)
head(house[,c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
              "Bedrooms", "BldgGrade")])
house_lm=lm(AdjSalePrice~SqFtTotLiving+SqFtLot+Bathrooms+Bedrooms+BldgGrade,
            data = house, na.action = na.omit)
house_lm 
##Intercept is estimator. If add 1square feet to Living, estimated price is $229
##If add 1,000square feet to Living, estimated price is $228,800

#Evaluate Model
summary(house_lm)

#Select Model and Stepwise Regression
house_full=lm(AdjSalePrice~SqFtTotLiving+SqFtLot+Bathrooms
              +Bedrooms+BldgGrade+ PropertyType + NbrLivingUnits + 
                  SqFtFinBasement + YrBuilt + YrRenovated + 
                  NewConstruction,
              data=house, na.action=na.omit)
house_full

step_lm=stepAIC(house_full,direction = 'both')

#Weighted Regression
house$Year=year(house$DocumentDate)
house$Year
house$Weight=house$Year-2005
house$Weight

house_wt=lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                Bedrooms + BldgGrade,
            data=house, weight=Weight, na.action=na.omit)
round(cbind(house_lm=house_lm$coefficients, 
            house_wt=house_wt$coefficients),digits = 3)

#Factor Variable
head(house[,'PropertyType'])
prop_type_dummies=model.matrix(~PropertyType -1, data=house)
##It change to binary with model.matrix 
head(prop_type_dummies)
##It has 3columns, it called one-hot encoding
##First factor is reference, the rest of factors can be interpretated coompare to reference factor relatively 

lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
       Bedrooms +  BldgGrade + PropertyType, data=house)
##Multiplex is reference, Single Family and Townhouse is related values.
##If PropertyTypeSingle Family==0 && PropertyTypeTownhouse==0, 
##Single Family is $85,000 cheaper, Townhouse is $150,000 cheaper than Multiplex

#Multi-Factor Variables
table(house$ZipCode)

zip_groups=house %>%
    mutate(resid=residuals(house_lm))  %>%   
    group_by(ZipCode) %>%
    summarize(med_resid=median(resid),cnt=n()) %>%
    arrange(med_resid) %>%
    mutate(cum_cnt=cumsum(cnt), 
           ZipGroup=factor(ntile(cum_cnt, 5))) ##Divide median ordered ZipCode to 5

house=house  %>%
    left_join(select(zip_groups,ZipCode,ZipGroup),by='ZipCode')
