#12.
#Undersampling
mean(full_train_set$outcome=='default')

full_model=glm(outcome~payment_inc_ratio+purpose_ + 
                   home_ + emp_len_+ dti + revol_bal + revol_util,
               data=full_train_set, family='binomial')
pred=predict(full_model)
mean(pred>0)
##It predicts about 39% is default from all.

#Updersampling and Up Weight
wt=ifelse(full_train_set$outcome=='default',
          1/mean(full_train_set$outcome=='default'),1)
full_model=glm(outcome~payment_inc_ratio + purpose_ + 
                   home_ + emp_len_+ dti + revol_bal + revol_util,
               data=full_train_set, weight=wt, family = 'quasibinomial')
pred=predict(full_model)
mean(pred>0)
##Weight for default is 1/mean(full_train_set$outcome=='default') which is default probability.
##Weight for pay off is 1
##Sum of Weight for default and Weight for pay off is almost similar.
