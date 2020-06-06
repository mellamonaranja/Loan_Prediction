#5.
#Regression Factor Variable
head(house[,'PropertyType'])
prop_type_dummies=model.matrix(~PropertyType -1, data=house)
##It change to binary with model.matrix.
head(prop_type_dummies)
##It has 3columns, it called one-hot encoding.
##First factor is reference, the rest of factors can be interpretated coompare to reference factor relatively.

lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
       Bedrooms +  BldgGrade + PropertyType, data=house)
##Multiplex is reference, Single Family and Townhouse is related values.
##If PropertyTypeSingle Family==0 && PropertyTypeTownhouse==0, it doesn't need to set Multiplex.
##Single Family is $85,000 cheaper, Townhouse is $150,000 cheaper than Multiplex.

#Multi-Factor Variables
table(house$ZipCode)

zip_groups=house %>%
    mutate(resid=residuals(house_lm))  %>%   
    group_by(ZipCode) %>%
    summarize(med_resid=median(resid),cnt=n()) %>%
    ##Get median of each zip codes residual
    arrange(med_resid) %>%
    mutate(cum_cnt=cumsum(cnt), 
           ZipGroup=factor(ntile(cum_cnt, 5))) 
            ##Divide Divide 82 zip codes to 5 integrated groups with ntile
##Divide 82 zip codes to 5 integrated groups basis on residual median of house_lm regression.

house=house  %>%
    left_join(select(zip_groups,ZipCode,ZipGroup),by='ZipCode')


