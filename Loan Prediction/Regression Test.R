#7.
#Outlier Analysis
house_98105=house[house$ZipCode==98105,]
lm_98105=lm(AdjSalePrice~SqFtTotLiving + SqFtLot + Bathrooms + 
                Bedrooms + BldgGrade, data=house_98105)

#Risidual Standard
sresid=rstandard(lm_98105)

#Minimum Residual
idx=order(sresid,decreasing=F)
sresid[idx[1]]

house_98105[idx[1],c('AdjSalePrice', 'SqFtTotLiving', 'SqFtLot',
                     'Bathrooms', 'Bedrooms', 'BldgGrade')]
##Wrong record occurred. It should be more expensive than $119,748

#Influence Plot(Bubble Plot)
std_resid=rstandard(lm_98105)
##Residual

cooks_D=cooks.distance(lm_98105)
##Influence by single record. Sum of leverage and residual.

hat_values=hatvalues(lm_98105)
##scale to measure leverage

plot(hat_values,std_resid,cex=10*sqrt(cooks_D))
##x=hat_values, y=Residual value, circle size=cooks_D
##Some points has significant enfluences.
abline(h=c(-2.5, 2.5), lty=2)

#Heteroscedasticity, Non-normal, Coefficient between errors
df=data.frame(resid=residuals(lm_98105),
              pred=predict(lm_98105))
ggplot(df,aes(pred,abs(resid)))+
    geom_point()+geom_smooth()
##THe reabsolute residuals and predict
##Linked the absolute residuals smoothly with geom_smooth()
##The variance of residual tends to increase when the house price is high.
##Vice versa. Hence, regression model lm_98105 has heteroscedasticity errors.
##Meaning this model isn't complete. It cannot explain about extreme house prices.

#Partial Residuals Plot
terms=predict(lm_98105, type = 'terms')
partial_resid=resid(lm_98105)+terms
df=data.frame(SqFtTotLiving=house_98105[,'SqFtTotLiving'],
              Terms=terms[,'SqFtTotLiving'],
              PartialResid=partial_resid[,'SqFtTotLiving'])
ggplot(df,aes(SqFtTotLiving,PartialResid))+
    geom_point(shape=1)+
    scale_shape(solid = F)+
    geom_smooth(linetype=2)+
    geom_line(aes(SqFtTotLiving, Terms))
##How SqFtTotLiving affect to house price.
##The relationship between SqFtTotLiving and house price is non-linear.
##According to linear line, house smaller than 1,000square fit extimates low price than origin price.
##House between 2,000-3,000square fit extimates high price than origin price.
##About over 4,000square fit, cannot analysis with small datas.
