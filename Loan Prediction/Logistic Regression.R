#10.
#Logistic Regression and Generalized Linear Model
logistic_model=glm(outcome~payment_inc_ratio+
                       purpose_+home_+emp_len_ + borrower_score,
                   data=loan_data, family='binomial')
logistic_model
##outcome is responsive variable.
##Pay off=0, default=1
##Variables start with purpose_, home_ are factor variable represent purpose of loan, status house possess.
##The column is P-1 from factor variables P.
##The standard to each factor variables is credit_card and MORTGAGE.
##borrower_score is the credit rating from 0(pay off) to 1(default).
##The worst deteriorate of credit rating is 100times risk than good of credit rating.

#Prediction of Logistic Regression
pred=predict(logistic_model)
summary(pred)
prob=1/(1+exp(-pred))
summary(prob)
##Transform to probability value
##However, still unclear whether this prediction is default or pay off, it only shows between 0 and 1.

#Evaluate Classification Model
summary(logistic_model)

logistic_gam=gam(outcome~s(payment_inc_ratio)+purpose_+
                     home_+emp_len_+s(borrower_score), 
                 data=loan_data, family='binomial')
##It search optimized knot on spline with s()

logistic_gam

terms=predict(logistic_gam,type = 'terms')
partial_resid=resid(logistic_gam)+terms
df=data.frame(payment_inc_ratio = loan_data[, 'payment_inc_ratio'],
              terms=terms[,'s(payment_inc_ratio)'],
              partial_resid=partial_resid[,'s(payment_inc_ratio)'])

ggplot(df,aes(x=payment_inc_ratio,y=partial_resid,solid=F))+
    geom_point(shape=46,alpha=.4)+
    geom_line(aes(x=payment_inc_ratio, y=terms),
              color='orange',alpha=.5, size=1.5)+
    labs(y='Partial Residual')+
    xlim(0, 25)+
    theme_bw()
##Upside is default(1), downside is pay off(0).
##Result variable is binomial.

