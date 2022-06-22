


## Bulb Lifetime

# 1

s = 3.007117
r = 1002.372


s_prior_2<- (0.003/(0.00173^2))*0.003
r_prior_2<- 0.003/(0.00173^2)

curve(dgamma(x,shape=s_prior_2,rate=r_prior_2),xlim=c(0,0.01),main="Prior Distribution", xlab=expression(theta), ylab="Density", col="#1874CD")




# 2

#We have vague knowledge as we have not yet observed data, in fact the prior distribution as said before represent only our belief about the parameter.


y_obs<-c(1, 13, 27, 43, 73, 75, 154, 196, 220, 297,
         344, 610, 734, 783, 796, 845, 859, 992, 1066, 1471)
y_mean<-mean(y_obs)
y_mean

#In addition, the mean of the prior distribution ($\frac{1}{0.003} = 333.3$) is quite distant from the sample one (479.95).



alpha_conf<-0.05

q_2_2 <- qgamma(alpha_conf / 2, s_prior_2, r_prior_2)
q_1_2 <- qgamma(1 - alpha_conf / 2, s_prior_2, r_prior_2)
c(1/q_1_2, 1/q_2_2)
diff(c(1/q_1_2, 1/q_2_2))

#The idea that we are giving a very vague guess about the average bulb life value is also largely supported by the results of the calculated confidence interval:
#in 95% of cases the value of $\frac{1}{\theta}$ will be between 138.5 and 1612.2, with a range of 1473.7.
#This information does not contribute much to the estimation of the true value.





# 3




par(mfrow=c(1,2))
hist(y_obs,xlim=c(0,2000),xlab="Hours", col="#008B00", main="Histogram of the bulb's life time")
abline(v=y_mean,lwd=2, lty=2)
boxplot(y_obs,col="#008B00",xlab="Hours",horizontal = T)

y_mean



#Analyzing the data it is immediately possible to notice the great variability of the life time of the bulbs: it goes from one hour to 1471 hours, that is about 61 days.
#The average lifetime of a bulb of the data is equal to 479.95.
#We can see from the graphs that the distribution is not symmetrical.




s_post_2<- s_prior_2 + as.numeric(length(y_obs))
r_post_2<- r_prior_2 + as.numeric(sum(y_obs))
cbind(s_post_2, r_post_2)
 


#After observing the data, the parameters were updated according to the rule given in the fourth point.


  
Mode_Post_2<-(s_post_2-1)/r_post_2
Mean_Post_2<-s_post_2/r_post_2
Median_Post_2<-qgamma(0.5, s_post_2, r_post_2)

cbind(Mode_Post_2,Median_Post_2,Mean_Post_2)

posterior_qf2 <- function(x){
  qgamma(x,shape=s_post_2,rate=r_post_2)
}

hpd(posterior.icdf=posterior_qf2, conf=0.95, tol=0.00000001)
diff(hpd(posterior.icdf=posterior_qf2, conf=0.95, tol=0.00000001)) # the width

 

#We have learnt the three suitable estimates of $\theta$ and also the highest posterior density interval.
#To make inference about $\psi= \frac{1}{\theta}$, instead it is necessary to use the Inverse-gamma distribution.
#In fact if $\theta \sim Gamma(\alpha, \beta) \Longrightarrow \frac{1}{\theta} = \psi \sim InvGamma(\alpha, \beta)$.


  
library(invgamma)

Mode_Post_Inv<-r_post_2/(s_post_2+1)

Mean_Post_Inv<-r_post_2/(s_post_2-1)
Median_Post_Inv<-qinvgamma(0.5, s_post_2, r_post_2)

cbind(Mode_Post_Inv,Median_Post_Inv,Mean_Post_Inv)

posterior_qf3 <- function(x){
  qinvgamma(x,shape=s_post_2,rate=r_post_2)
}

hpd(posterior.icdf=posterior_qf3, conf=0.95, tol=0.00000001)
diff(hpd(posterior.icdf=posterior_qf3, conf=0.95, tol=0.00000001)) # the width

 

#Using the posterior distribution we obtain that the average life time is 481.7 hours, the median is 467.5 and finally the mode (the peak of the distribution) is equal to 441.6.

#In addition, through the HPD interval with the alpha set in the code, we can say that 95% of the time the life time value of innovative bulbs is between 299 and 692 hours.


polygon(curve(dinvgamma(x,shape= s_prior_2, rate= r_prior_2),from=0,to=1000,xlab=expression(psi),cex.main=0.5,1000,ylim=c(0,0.006), col="#63B8FF", lwd=2, ylab="Density"),col=c("#63B8FF",alpha=0.2))

polygon(curve(dinvgamma(x,shape=s_post_2,rate=r_post_2),from=0,to=1000,xlab=expression(theta),main="",cex.main=0.5,1000,add=TRUE, col = 'red', lwd=2), col = "red")
legend("topright",legend=c("Prior", "Posterior"), fill=c("#63B8FF", "red"),bg="#CAFF70")
 

# 4

To answer the question i have to use the CDF of the posterior distribution: $P(\psi > 550 | y_1, ...,y_n) = 1- P(\psi \leq 550 | y_1, ...,y_n) = 1- F(550)$.




pa<-curve(pinvgamma(x,s_post_2,r_post_2),xlim=c(0,1000),main="CDF of the Posterior Distribution",ylab="Cumulative Density",xlab=expression(psi))
polygon(c(pa$x[pa$x>=550], max(pa$x), 550 ),  c( pa$y[pa$x>=550],0.77,0.77 ), col="red")
abline(h=min(pa$y[pa$x>=550]),lwd=2,lty=2)
abline(v=550,lwd=2)

 


  
1-pinvgamma(550,s_post_2,r_post_2)
 

#So the probability that  the average bulb lifetime exceeds 550 hours is 22.54%

#Analyzing only the data we have that 9 bulbs out of 20 last more than 550 hours, so the probability should be close to 50%, the big difference that we have with the value obtained from the Cumulative density function depends on the role that had the prior distribution in influencing the posterior one. Because a low number of data was observed and at the same time the prior gamma distribution has a remarkably high rate value, the likelihood did not have much power in influencing the posterior distribution.


