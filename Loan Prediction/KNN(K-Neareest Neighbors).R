#13.
library(MASS)
loan200=read.csv(file.path(PSDS_PATH, 'data', 'loan200.csv'))
loan200$outcome=ordered(loan200$outcome, levels=c('paid off', 'default'))

loan3000=read.csv(file.path(PSDS_PATH, 'data', 'loan3000.csv'))
loan3000$outcome=ordered(loan3000$outcome, levels=c('paid off', 'default'))

loan_data=read.csv(file.path(PSDS_PATH, 'data', 'loan_data.csv'))
loan_data=select(loan_data, -X, -status)

head(loan_data)
##payment_inc_ratio is pay off ratio by income.
##dti is default ratio by income.
head(loan200)
##loan200 has 200datas which has payment_inc_ratio and dti.
##First row is new loan. Row2 to row 201 has result.
newloan=loan200[1,2:3,drop=F]
head(newloan)


#KNN(K-Nearest Neighbors)
knn_pred=knn(train=loan200[-1, 2:3],test=newloan,cl=loan200[-1,1],k=20)
##If k=20(the number of neighbors), get new prediction about first row which payment_inc_ratio=9, dti=22.5
knn_pred=='paid off'
##Result is True, predictable to pay off.

loan200[attr(knn_pred, 'nn.index')-1, ]
dist=attr(knn_pred, 'nn.dist')

circleFun=function(center=c(0,0), r=1, npoints=100){
    tt=seq(0, 2*pi, length.out=npoints-1)
    xx=center[1]+r * cos(tt)
    yy=center[2]+r * sin(tt)
    return(data.frame(x=c(xx, xx[1]), y=c(yy, yy[1])))
}

circle_df=circleFun(center=unlist(newloan), r=max(dist), npoints=201)
loan200_df=bind_cols(loan200, circle_df)

ggplot(data=loan200_df, aes(x=payment_inc_ratio, dti, color=outcome, shape=outcome))+
    geom_point(size=2)+
    scale_shape_manual(values=c(1, 4, 15))+
    geom_path(aes(x=x, y=y), color='black')+
    xlim(3, 15)+
    ylim(17, 29)+
    theme_bw()

#Standardization
loan_df=model.matrix(~-1+payment_inc_ratio+dti+revol_bal+revol_util, data=loan_data)
##revol_bal is total revolution balance, revol_util is utilizable revolution.
newloan=loan_df[1,,drop=F]
loan_df=loan_df[-1,]
outcome=loan_data[-1,1]
knn_pred=knn(train=loan_df,test=newloan,cl=outcome,k=5)
knn_pred
loan_df[attr(knn_pred,"nn.index"),]
##Get 5 toppest rows.
##Those revol_bal are similar with newloan. However other predict variables are spread out from newloan.
##Therefore when decide the neighbor, it isn't important to other variables.

#Z-score
loan_df=model.matrix(~-1+payment_inc_ratio+dti+revol_bal
                     +revol_util, data=loan_data)
loan_std=scale(loan_df)
##Standardize the loan_df with scale

target_std=loan_data[1,,drop=F]
str(target_std)
View(target_std)
loan_std=loan_std[-1,]
str(loan_std)
outcome=loan_data[-1,1]
knn_pred=knn(train=loan_std, test=target_std,cl=outcome,k=5)
loan_df[attr(knn_pred,'nn.index'),]

#Create a feature engineering for borrowers
borrow_df=model.matrix(~-1+dti + revol_bal + revol_util + open_acc +
                           delinq_2yrs_zero + pub_rec_zero, data=loan_data)
borrow_knn=knn(borrow_df,test=borrow_df,cl=loan_data[,'outcome'],
               prob=T, k=20)
prob=attr(borrow_knn,'prob')
borrow_feature=ifelse(borrow_knn=='default',1-prob, prob)
summary(borrow_feature)
##Default predictable data has been predicted.
