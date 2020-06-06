#15.
#Random Forest
rf=randomForest(outcome~borrower_score + payment_inc_ratio,
                data=loan3000)
rf
##Basically create 500trees with training.
##Algorithm selects one of two variables randomly(borrower_score, payment_inc_ratio).
##The size of bootstrap sample is 1

#OOB(Out Of Bag)
error_df=data.frame(error_rate=rf$err.rate[,'OOB'],num_trees=1:rf$ntree)
ggplot(error_df,aes(x=num_trees,y=error_rate))+
    geom_line()  +
    theme_bw()
##The error rate is remarkably decrease from 0.44 to 0.385 and keeps plain.
##More tree adds, improve the accuracy.

pred=predict(rf,prob=T)
rf_df=cbind(loan3000,pred=pred)
ggplot(data=rf_df,aes(x=borrower_score,y=payment_inc_ratio, 
                      shape=pred, color=pred)) +
    geom_point(alpha=.6, size=2) +
    scale_shape_manual( values=c( 46, 4)) +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0), lim=c(0, 20)) + 
    theme_bw()
##Although high credit rating predictable default. This is including exception training.

#Variable Importance
rf_all=randomForest(outcome~.,data=loan_data,importance=T)
##Request additional importance of other variable's information with importance.
rf_all

varImpPlot(rf_all,type=1)
##It shows importance by accuracy decrease and gini decrease each.
##It placed ranking by accuracy decrease.
##There is difference in two plots by variable importance.
