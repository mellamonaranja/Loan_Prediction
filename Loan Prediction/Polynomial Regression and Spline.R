#8.
#Polynomial Regression
lm(AdjSalePrice~poly(SqFtTotLiving,2)+SqFtLot
   +BldgGrade + Bathrooms +  Bedrooms, 
   data=house_98105)
##There are 2 SqFtTotLiving coefficients 

#Spline
knots=quantile(house_98105$SqFtTotLiving, p=c(.25, .5, .75))
lm_spline=lm(AdjSalePrice~bs(SqFtTotLiving,knots=knots,degree=3)+
                 SqFtLot +Bathrooms + Bedrooms + BldgGrade,  data=house_98105)
##Applied predict variable SqFtTotLiving to 3id spline with degree=3
##Default spline setup is knots on the edge such as quantile.

terms1=predict(lm_spline,type='terms')
partial_resid1=resid(lm_spline)+terms
df1=data.frame(SqFtTotLiving=house_98105[,'SqFtTotLiving'],
               Terms=terms1[,1],
               PartialResid=partial_resid[,1])
ggplot(df1,aes(SqFtTotLiving,PartialResid))+
    geom_point(shape=1)+
    scale_shape(solid = F)+
    geom_smooth(linetype=2)+
    geom_line(aes(SqFtTotLiving, Terms))+
    theme_bw()
##Regression line is coloser to data.

#Generalized Additive Model(GAM)
lm_gam=gam(AdjSalePrice~s(SqFtTotLiving)+
               SqFtLot+Bathrooms+Bedrooms+
               BldgGrade, data=house_98105)
##It search optimized knot on spline with s(SqFtTotLiving)

terms=predict.gam(lm_gam, type='terms')
partial_resid=resid(lm_gam)+terms
df=data.frame(SqFtTotLiving=house_98105[,'SqFtTotLiving'],
              Terms=terms[,5],
              PartialResid=partial_resid[,5])
ggplot(df,aes(SqFtTotLiving,PartialResid))+
    geom_point(shape=1)+
    scale_shape(solid=F)+
    geom_smooth(linetype=2)+
    geom_line(aes(SqFtTotLiving,Terms))+
    theme_bw()
