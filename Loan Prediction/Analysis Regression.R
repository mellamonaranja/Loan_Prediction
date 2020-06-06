#6.
#Correlated Variables
step_lm$coefficients
##More bedrooms, less value because coefficient of bedrooms is negative.
##With the same size of house, house size is more effect to house price rather than bedrooms number.

update(step_lm,.~.-SqFtTotLiving - SqFtFinBasement - Bathrooms)
##Available to add or remove model variable with update.
##Now bedrooms is positive because it removed.

#Confounding Variables
lm(AdjSalePrice~SqFtTotLiving + SqFtLot + 
       Bathrooms + Bedrooms + 
       BldgGrade + PropertyType + ZipGroup,
   data=house, na.action=na.omit)
##The group5 which is most expensive group's house price is $340,000 more expensive.
##When one bathroom adds, $5,537 increases.

#Interaction and Main Effect
lm(AdjSalePrice~SqFtTotLiving*ZipGroup + SqFtLot + 
       Bathrooms + Bedrooms + 
       BldgGrade + PropertyType,
   data=house, na.action=na.omit)
##There is strong interaction between house location and size.
##House lean is $118 by square fit.
##The most expensive coefficient is SqFtTotLiving+SqFtTotLiving:ZipGroup5=$348
##In other words, when 1 square fit house size increases, it has 3times to cheapest price.

