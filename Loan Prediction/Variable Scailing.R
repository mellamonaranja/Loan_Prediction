#17.
#Variable Scailing
defaults=loan_data[loan_data$outcome=='default',]
df=defaults[, c('loan_amnt', 'annual_inc', 'revol_bal', 'open_acc', 'dti', 'revol_util')]
km=kmeans(df,centers = 4, nstart = 10)
centers=data.frame(size=km$size, km$centers)
round(centers, digits=2)
##annual_inc and revol_bal influence.
##There are only 55persons who relatively high income and high revolving balance in cluster 1.

#Normalization
df0=scale(df)
##Normalize with scale.
km0=kmeans(df0,centers = 4, nstart = 10)
centers0=scale(km0$centers, center=FALSE, scale=1/attr(df0, 'scaled:scale'))
centers0=scale(centers0,center=-attr(df0,'scaled:center'), scale=FALSE)
centers0=data.frame(size=km0$size, centers0)
round(centers0,digits = 2)
##Every size of cluster is a little bit equality rather than before with normalization.
##annual_inc and revol_bal don't influence much.
