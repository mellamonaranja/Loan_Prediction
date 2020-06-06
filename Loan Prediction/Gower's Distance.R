#18.
#Gower's Distance
x=loan_data[1:5, c('dti', 'payment_inc_ratio', 'home_', 'purpose_')]
x

daisy(x,metric = 'gower')
##Compute gower's distance with daisy
##Every distances is between 0 and 1.

df=loan_data[sample(nrow(loan_data), 250),
             c('dti', 'payment_inc_ratio', 'home_', 'purpose_')]
d=daisy(df,metric = 'gower')    
hcl=hclust(d)    
##Apply hierachical Clustering
dnd=as.dendrogram(hcl)
plot(dnd,leaflab = 'none',ylab='distance')
##It's hard to discriminate.

dnd_cut=cut(dnd,h=.5)
df[labels(dnd_cut$lower[[1]]),]
##The purpose of loan is credit card, home type is rent.
##It tends to create grouping by clustering.
