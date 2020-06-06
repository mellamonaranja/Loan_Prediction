#2.
library(ggplot2)

PSDS_PATH=file.path('/Users/joohyunyoon/workspace/Loan Prediction/')
loans_income=read.csv(file.path(PSDS_PATH, 'data', 'loans_income.csv'))[,1]
loans_income <- read.csv(file.path('/Users/joohyunyoon/workspace/Practical_Statistics', 'data', 'loans_income.csv'))[,1]

#Standard Distribution
samp_data=data.frame(income=sample(loans_income,1000),
                     type='data_dist')
samp_mean_05=data.frame(income=tapply(sample(loans_income,1000*5),
                                      rep(1:1000, rep(5,1000)),
                                      FUN = mean),
                        type='mean_of_5')

samp_mean_20=data.frame(income=tapply(sample(loans_income,1000*20),
                                      rep(1:1000, rep(20,1000)),
                                      FUN = mean),
                        type='mean_of_20') 
##Bigger sampling size, smaller sampling error

income=rbind(samp_data, samp_mean_05, samp_mean_20)
str(income)
View(income)
income$type=factor(income$type,
                   levels = c('data_dist', 'mean_of_5', 'mean_of_20'),
                   labels=c('Data', 'Mean of 5', 'Mean of 20'))
ggplot(income, aes(x=income))+
    geom_histogram(bins = 40)+
    facet_grid(type~.)
