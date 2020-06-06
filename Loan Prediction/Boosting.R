#16.
#XGBoost
predictors=data.matrix(loan3000[,c('borrower_score', 'payment_inc_ratio')])
label=as.numeric(loan3000[,'outcome'])-1
xgb=xgboost(data=predictors,label=label, objective='binary:logistic',
            params=list(subsample=.63,eta=0.1),
            nrounds=100)
##The ratio of input data with subsample
##Scale down ratio with eta, which has effect in overfitting prevention.

pred=predict(xgb,newdata=predictors)
xgb_df=cbind(loan3000,pred_default=pred>.5,prob_default=pred)
ggplot(data=xgb_df,aes(x=borrower_score, y=payment_inc_ratio, 
                       color=pred_default, shape=pred_default)) +
    geom_point(alpha=.6, size=2) +
    scale_shape_manual(values=c(46,4))+
    scale_x_continuous(expand=c(.03,0))+
    scale_y_continuous(expand=c(0,0),lim=c(0,20))+
    theme_bw()

#Regularization
seed=400820
predictors=data.matrix(loan_data[,-which(names(loan_data) %in% 'outcome')])
label=as.numeric(loan_data$outcome)-1
test_idx=sample(nrow(loan_data), 10000)

xgb_default=xgboost(data=predictors[-test_idx,], label=label[-test_idx], 
                    objective="binary:logistic", nrounds=250, verbose=0)
pred_default=predict(xgb_default, predictors[test_idx,])
error_default=abs(label[test_idx] - pred_default) > 0.5
xgb_default$evaluation_log[250,]
mean(error_default)

xgb_penalty=xgboost(data=predictors[-test_idx,], 
                    label=label[-test_idx], 
                    params=list(eta=.1, subsample=.63, lambda=1000),
                    objective="binary:logistic", nrounds=250, verbose=0)
pred_penalty=predict(xgb_penalty, predictors[test_idx,])
error_penalty=abs(label[test_idx] - pred_penalty) > 0.5
xgb_penalty$evaluation_log[250,]
mean(error_penalty)

error_default=rep(0, 250)
error_penalty=rep(0, 250)
for(i in 1:250)
{
    pred_default=predict(xgb_default, predictors[test_idx,], ntreelimit=i)
    error_default[i]=mean(abs(label[test_idx] - pred_default) > 0.5)
    pred_penalty=predict(xgb_penalty, predictors[test_idx,], ntreelimit=i)
    error_penalty[i]=mean(abs(label[test_idx] - pred_penalty) > 0.5)
}

errors=rbind(xgb_default$evaluation_log,
             xgb_penalty$evaluation_log,
             data.frame(iter=1:250, train_error=error_default),
             data.frame(iter=1:250, train_error=error_penalty))
errors$type=rep(c('default train', 'penalty train', 
                  'default test', 'penalty test'), rep(250, 4))

ggplot(errors, aes(x=iter, y=train_error, group=type)) +
    geom_line(aes(linetype=type, color=type), size=1) +
    scale_linetype_manual(values=c('solid', 'dashed', 'dotted', 'longdash')) +
    theme_bw() +
    theme(legend.key.width=unit(1.5,"cm")) +
    labs(x="Iterations", y="Error") +
    guides(colour=guide_legend(override.aes=list(size=1))) 
##Default model steadily improve the accuracy, however test data is opposite.

#Hyperparameters and Cross Validation
N=nrow(loan_data)
fold_number=sample(1:5, N, replace = T)
params=data.frame(eta=rep(c(.1,.5,.9),3),
                  max_depth=rep(c(3, 6, 12),
                                rep(3,3)))
rf_list=vector('list',9)
error=matrix(0,nrow = 9, ncol = 5)
for (i in nrow(params)) {
    for (k in 1:5) {
        cat('Fold',k,'for model',i,'\n')
        fold_idx=(1:N)[fold_number==k]
        xgb=xgboost(data=predictors[-fold_idx,],
                    label=label[-fold_idx],
                    params = list(eta=params[i,'eta'],
                                  max_depth=params[i,'max_depth']),
                    ##max_depth means maximum tree depth from root to leaf node, default is 6
                    objective='binary:logistic',
                    nrounds=100, verbose = 0)
        pred=predict(xgb, predictors[fold_idx,])
        error[i,k]=mean(abs(label[fold_idx]-pred)>=0.5)
    }
}

avg_error=100*rowMeans(error)
##Compare error rate to other parameter with rowMeans function.
cbind(params,avg_error)
##Using less eta, shallow tree presents more accurate.
##This model is more stable, optimized parameter is eta=0.1, max_depth=3 or 6
