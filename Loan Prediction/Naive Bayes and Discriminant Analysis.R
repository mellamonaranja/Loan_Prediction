#9.
loan3000=read.csv(file.path(PSDS_PATH, 'data', 'loan3000.csv'))
loan_data=read.csv(file.path(PSDS_PATH, 'data', 'loan_data.csv'))
loan_data$outcome=ordered(loan_data$outcome, levels=c('paid off', 'default'))
full_train_set=read.csv(file.path(PSDS_PATH, 'data', 'full_train_set.csv'))
full_train_set$outcome=ordered(full_train_set$outcome, levels=c('paid off', 'default'))

#Naive Bayes
naive_model=NaiveBayes(outcome~purpose_+home_+emp_len_,
                       data=na.omit(loan_data))
naive_model$table

new_loan=loan_data[147,c('purpose_', 'home_', 'emp_len_')]
row.names(new_loan)=NULL
new_loan
predict(naive_model,new_loan)
##This model can be predicted default.

#Discriminant Analysis
loaa_lda=lda(outcome~borrower_score+payment_inc_ratio,
             data=loan3000)
loaa_lda$scaling

pred=predict(loaa_lda)
head(pred$posterior)
##This model can be predicted default.

lad_df=cbind(loan3000,prob_default=pred$posterior[,'default'])

x=seq(from=.33,to=.73, length=100)
y=seq(from=0,to=20,length=100)
newdata=data.frame(borrower_score=x,payment_inc_ratio=y)
pred=predict(loaa_lda,newdata=newdata)
lda_df0=cbind(newdata,outcome=pred$class)

ggplot(data=lad_df, aes(x=borrower_score, y=payment_inc_ratio, color=prob_default))+
    geom_point(alpha=.6)+
    scale_color_gradient2(low='white',high='blue')+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0),lim=c(0,20))+
    geom_line(data=lda_df0,col='orange',size=2, alpha=.8)+
    theme_bw()
##LDA(Linear Discriminant Analysis) divides predict variable range to 2 parts.
##Further from linear, higher confidence. In other words, the probability is further from 0.5.

