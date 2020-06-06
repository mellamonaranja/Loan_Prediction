#3.
library(boot)

#Bootstrap
stat_fun=function(x,idx) median(x[idx])
boot_obj=boot(loans_income, R=1000, statistic = stat_fun)
boot_obj 
##Original estimated mean is 62,000$
##There is around -$70 bias, $218 standard error.
