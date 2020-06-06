#14.
#Tree Model
loan_tree=rpart(outcome~borrower_score + payment_inc_ratio,
                data=loan3000, control=rpart.control(cp=.005))

plot(loan_tree,uniform=T, margin=.05)
text(loan_tree, cex=.75)

#Recursive Partitioning Algorithm

r_tree=data_frame(x1=c(0.575, 0.375, 0.375, 0.375, 0.475),
                  x2=c(0.575, 0.375, 0.575, 0.575, 0.475),
                  y1=c(0,0, 10.42, 4.426, 4.426),
                  y2=c(25, 25, 10.42, 4.426, 10.42),
                  rule_number=factor(c(1, 2, 3, 4, 5)))
r_tree=as.data.frame(r_tree)

labs=data.frame(x=c(.575 + (1-.575)/2, 
                    .375/2, 
                    (.375 + .575)/2,
                    (.375 + .575)/2, 
                    (.475 + .575)/2, 
                    (.375 + .475)/2
),
y=c(12.5, 
    12.5,
    10.42 + (25-10.42)/2,
    4.426/2, 
    4.426 + (10.42-4.426)/2,
    4.426 + (10.42-4.426)/2
),
decision=factor(c('paid off', 'default', 'default', 'paid off', 'paid off', 'default')))


ggplot(data=loan3000, aes(x=borrower_score, y=payment_inc_ratio)) +
    geom_point( aes(color=outcome, shape=outcome), alpha=.5) +
    scale_color_manual(values=c('blue', 'red')) +
    scale_shape_manual(values=c(1, 46)) +
    ##scale_shape_discrete(solid=FALSE) +
    geom_segment(data=r_tree, aes(x=x1, y=y1, xend=x2, yend=y2, linetype=rule_number), size=1.5, alpha=.7) +
    guides(colour=guide_legend(override.aes=list(size=1.5)),
           linetype=guide_legend(keywidth=3, override.aes=list(size=1))) +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0), limits=c(0, 25)) + 
    geom_label(data=labs, aes(x=x, y=y, label=decision)) +
    #theme(legend.position='bottom') +
    theme_bw()
##Partitioning repeatedly by prediction. Classify relatively same classes.
##First rule is borrower_score>=0.525(line 1)
##Second rule is payment_inc_ratio<9.732, divides two sections with line 2.
