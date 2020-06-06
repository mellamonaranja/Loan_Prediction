#11.
#Confusion Matrix
pred=predict(logistic_gam,newdata=loan_data)
pred_y=as.numeric(pred>0)
true_y=as.numeric(loan_data$outcome=='default')    
true_pos=(true_y==1)&(pred_y==1)    
true_neg=(true_y==0)&(pred_y==0)    
false_pos=(true_y==0)&(pred_y==1)    
false_neg=(true_y==1)&(pred_y==0)
conf_mat=matrix(c(sum(true_pos), sum(false_pos),
                  sum(false_neg), sum(true_neg)),2,2)    
colnames(conf_mat)=c('Yhat=1','Yhat=0')
rownames(conf_mat)=c('Y=1','Y=0')
conf_mat
##Column is predict value, row is actual result.
##A diagonal is number of precise datas, non-diagonal is  number of inaccurate datas.

#Precision
conf_mat[1,1]/sum(conf_mat[,1])

#Recall(Sensitivity)
conf_mat[1,1]/sum(conf_mat[1,])

#Specificity
conf_mat[2,2]/sum(conf_mat[2,])

#ROC(Receiver Operating Characteristic) Curve
idx=order(-pred)
recall=cumsum(true_y[idx]==1)/sum(true_y==1)
specificity=(sum(true_y==0)-cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df=data.frame(recall=recall, specificity=specificity)
ggplot(roc_df,aes(x=specificity, y=recall))+
    geom_line(color='blue') + 
    scale_x_reverse(expand=c(0,0))+
    scale_y_continuous(expand=c(0, 0)) +
    geom_line(data=data.frame(x=(0:100)/100),
              aes(x=x, y=1-x), linetype='dotted',color='orange')+
    theme_bw()
##Dotted is prediction result.
##If ORC curve is close to upper left side, in other words, it will predict precisely 1.

#AUC(Area Underneath the Curve)
ggplot(roc_df,aes(specificity))+
    geom_ribbon(aes(ymin=0, ymax=recall),fill='orange',
                alpha=.3)+
    scale_x_reverse(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    labs(y='recall') +
    theme_bw()

#AUC(Area Underneath the Curve) Calculation
sum(roc_df$recall[-1]*diff(1-roc_df$specificity))
##Higer the AUC value, better.
##If classifier predicts 1 precisely, meaning flawless classifier.
##The worst classifier is 0.5
##The AUC value is 0.69, therefore relatively weak classifier.

head(roc_df)
