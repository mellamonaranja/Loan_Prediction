#1.
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages('vioplot')
library(vioplot)
install.packages('ascii')
library(ascii)
install.packages('corrplot')
library(corrplot)
install.packages('descr')
library(descr)
install.packages('stat_binhex')
library(stat_binhex)

PSDS_PATH=file.path('/Users/joohyunyoon/workspace/Loan Prediction/')
kc_tax=read.csv(file.path(PSDS_PATH, 'data', 'kc_tax.csv'))
lc_loans=read.csv(file.path(PSDS_PATH, 'data', 'lc_loans.csv'))

#Hexagonal binning and Contour plot
kc_tax0=subset(kc_tax,TaxAssessedValue<750000&SqFtTotLiving>100 &
                   SqFtTotLiving<3500) 
##Remove too much expensive and too much cheaper

nrow(kc_tax0)
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue)))+ stat_binhex(color='white')+
    theme_bw()+ scale_fill_gradient(low='white',high='orange')+
    labs(x="Finished Square Feet", y="Tax Assessed Value")

ggplot(kc_tax0,aes(SqFtTotLiving, TaxAssessedValue))+
    theme_bw()+
    geom_point(alpha=0.1)+
    geom_density2d(color='white')+
    labs(x="Finished Square Feet", y="Tax Assessed Value")

#Crosstable
library(descr)
x_tab=CrossTable(lc_loans$grade, lc_loans$status,
                 prop.c = F, prop.chisq = F, prop.t = F)
tots=cbind(row.names(x_tab$tab),
           format(cbind(x_tab$tab, x_tab$rs)))

props=cbind("", format(cbind(x_tab$prop.row, x_tab$rs/x_tab$gt), digits=1))
c_tot=c("Total", format(c(x_tab$cs, x_tab$gt)))

asc_tab=matrix(nrow=nrow(tots)*2+1, ncol=ncol(tots))
colnames(asc_tab)=c("Grade", colnames(x_tab$tab), "Total")
idx=seq(1, nrow(asc_tab)-1, by=2)
asc_tab[idx,]=tots
asc_tab[idx+1,]=props
asc_tab[nrow(asc_tab), ]=c_tot
ascii(asc_tab,  align=c("l", "r", "r", "r", "r"), include.rownames = FALSE, include.colnames = TRUE)

#Visualize Multivariable
kc_tax0=subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving>100 &
                      SqFtTotLiving<3500)
nrow(kc_tax0)

ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
    stat_binhex(colour="white") + 
    theme_bw() + 
    scale_fill_gradient(low="white", high="black") +
    labs(x="Finished Square Feet", y="Tax Assessed Value")

