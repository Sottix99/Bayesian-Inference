## Fully Bayesian conjugate analysis of Rome car accidents
# 1

# First we need to upload the data.


load(file= "homework_1.RData")
mydata <- subset(roma,subset=sign_up_number==104)
str(mydata)

y_sum<-as.numeric(sum(mydata$car_accidents))
n<-as.numeric(length(mydata$car_accidents))
y_bar<-y_sum/n
y_bar


plot( mydata$week ,mydata$car_accidents , col="red", pch=16, xaxt="n",xlab="Weeks", ylab="Number of accidents", main="Distribution of the accidents in the considered days")
lines( mydata$week ,mydata$car_accidents , col="blue",  xaxt="n",xlab="Weeks", ylab="Number of accidents")
axis(1, at = seq(round(min(mydata$week)), round(max(mydata$week)), by = 1), labels = 1:19)


# This plot represent the number of car accidents in each considered day.


plot(table(mydata$car_accidents),col="red",  xaxt="n", xlab="Accidents in a day", ylab="Frequency", main="Frequencies of car accidents")
axis(1, at = seq(round(min(mydata$car_accidents)), round(max(mydata$car_accidents)), by = 1), labels = 1:8)


#Analyzing the plots reported we realize that the distribution is not symmetrical, most of the data tends to be distributed between 2 and 5. Moreover in three weekdays there were 8 accidents, this value is quite extreme compared to the others. 

#In addition, knowing that the average number of car accidents in Rome is 3.22, we can state that in the 19 Saturdays present in the dataset, the average value is higher (3.89).

# 2

#To perform Bayesian inference, we need a prior distribution for the unknown rate $\theta$ .
#For mathematical convenience we use a gamma distribution, which is conjugate to the Poisson.
\

#We assume that the mean is equal to the variance, because the accidents of the machines re-enter in the category of the rare events, which distributing itself like a poisson, having mean and variance both equal to the unknown parameter.

#So we have to built a system of equations to find the proper parameters for $s$ and $r$ of Gamma distribution.


s_pre<-3.22
r_pre<-1



s_post<-s_pre + y_sum
r_post<-r_pre + n





# 3


# a)

#The three alternatives point estimates are the Mean, the Mode and the Median.


Mode_Post<- (s_post-1)/r_post
Mean_Post<-s_post/r_post
Median_Post<-qgamma(0.5, s_post, r_post)

cbind(Mode_Post,Median_Post,Mean_Post)

#The three values are extremely similar to each other.
#It is known that in the Gaussian mean, mode and median coincide at the theoretical level, so it is possible to argue that this posterior distribution can be approximated to a Gaussian. 

#It is possible to perform the Shapiro-Wilk test to analyze this assumption.

P_vector<-rep(0,1000)
for (i in 1:1000){
  sample<-rgamma(200,s_post,r_post)
  a<-shapiro.test(sample)
  P_vector[i]<-a$p.value
}
length(P_vector[P_vector>0.05])/length(P_vector)


#By running the test 1000 times the normality of the sample generated from the Posterior distribution is accepted about 80% of the time.

# b)


Mean_pre<-3.22
polygon(curve(dgamma(x,shape= 3.22, rate= 1),from=0,to=10,xlab=expression(theta),main="Uncertainity",cex.main=0.5,1000,ylim=c(0,2), col="orange", lwd=2, ylab="Density"),col=c("orange",alpha=0.2))

polygon(curve(dgamma(x,shape=s_post,rate=r_post),from=0,to=10,xlab=expression(theta),main="",cex.main=0.5,1000,add=TRUE, col = '#528B8B', lwd=2), col = "#528B8B")
legend("topright",legend=c("Prior", "Posterior"), fill=c("orange", "#528B8B"),bg="#CAFF70")

abline(v=Mean_Post,lty=2,lwd=2)
abline(v=Mean_pre,lty=2,lwd=2)
text(x=2.5,y=0.5,"Prior Mean \n 3.22",col="orange")
text(x=5,y=1,"Posterior Mean \n 3.8",col="#528B8B")



#To evaluate the uncertainty of posterior distribution we can use the posterior variance. 

Variance_Post<- s_post/(r_post)^2
Variance_Post




# c)
#Equal Tailed Interval (ETI)


alpha_conf<-0.05 # set the value of Alpha

q_lower <- qgamma(alpha_conf / 2, s_post, r_post)
q_upper <- qgamma(1 - alpha_conf / 2, s_post, r_post)
c(q_lower, q_upper) # 95% equal-tailed CI for the model



theta <- seq(0,7, by = 0.001) # set up grid for plotting
plot(theta, dgamma(theta, s_post, r_post), type = 'l', lwd = 2, col = '#528B8B',
     ylim = c(0, 1.5), xlab = expression(theta),main="Equal-tailed Interval",ylab="")
y_val <- dgamma(theta, s_post, r_post)
x_coord <- c(q_lower, theta[theta >= q_lower & theta <= q_upper], q_upper)
y_coord <- c(0, y_val[theta >= q_lower & theta <= q_upper], 0)
polygon(x_coord, y_coord, col = '#79CDCD', lwd = 2,  border = '#528B8B')
abline(v=Mean_Post,lwd=2,col="#CD4F39",lty=2)




#Highest Posterior Density (HPD)


posterior_qf <- function(x){
  qgamma(x,shape=s_post,rate=r_post)
}

library(TeachingDemos)
hpd(posterior.icdf=posterior_qf, conf=0.95, tol=0.00000001)


W_hpd<-diff(hpd(posterior.icdf=posterior_qf, conf=0.95, tol=0.00000001)) # HPD method
W_eti<-diff(c(q_lower, q_upper)) # ETI
cbind(W_hpd,W_eti)


#Generally, it is better to use HPD as the interval instead of Equal Tailed (ETI) for the posterior distribution.
#The reason for using the HPD is that all the values inside the interval have higher probability density (i.e., credibility) than any value outside. The HPD therefore includes the most credible values of $\theta$.
#Instead a 95% ETI has 2.5% of the distribution on either side of its limits. It indicates the 2.5th percentile and the 97.5th percentile.

#However in symmetric distributions, the ETI and HPD are Identical, in fact in our case The width and the extremes of the two intervals are approximately the same. 

#So from the results  we can say that in 95% of cases the value of the parameter is between 3.02 and 4.73.

# d)


{par(mfrow=c(2,1))
  
  plot(theta, dgamma(theta,3.22 , 1), type = 'l', lwd = 2, col = 'orange',
       ylim = c(0, 1.2), xlab = expression(theta),main="Prior Distribution",ylab="")
  
  
  
  plot(theta, dgamma(theta, s_post, r_post), type = 'l', lwd = 2, col = '#528B8B',
       ylim = c(0, 1.2), xlab = expression(theta),main="Posterior Distribution",ylab="")}



#Analyzing the differences between the two distributions, it is possible to notice that the Prior one has a much greater variability concerning the estimate of $\theta$: in fact there is no evident peak, moreover many values of $\theta$ have a similar probability value, this is not at all useful in estimating the value of the parameter.

#After observing the data, the posterior distribution shows a much more accurate and centered estimate by assigning high probabilities to a few values of $\theta$.


cbind(Mean_pre,y_bar,Mean_Post)


#Furthermore, it is possible to comment on the weight had by the prior distribution on the posterior one by analyzing the three averages reported: the mean of the posterior is much closer to the sample one than the one of the prior.
#This makes us understand that the influence of likelihood was greater than the prior distribution. 



# e)

#The posterior predictive distribution is the distribution of future observable data ($Y_{Next}$) , based on the posterior distribution.


y_grid <- 0:10


plot(y_grid, dnbinom(y_grid, size = s_post, prob = r_post / (1 + r_post)),
     type = 'h', lwd = 3, col = 'slateblue1', xlab = expression(y_Next),
     ylab = 'Probability',ylim=c(0,0.40))
lines(y_grid, dnbinom(y_grid, size = s_post, prob = r_post / (1 + r_post)),
      type = 'p', lwd = 3, col = 'slateblue1')

lines(table(mydata$car_accidents)/19, lwd = 3, col = 'mediumseagreen')
lines(table(mydata$car_accidents)/19,type='p', lwd = 3, col = 'mediumseagreen')
legend('topright', inset = .02, 
       legend = c('posterior predictive', 'observed data'),
       col = c('slateblue1', 'mediumseagreen'), lwd = 3)



#Observing the graph we realize how the two distributions are more or less similar in shape, even if the biggest problem between the two is the value 8.

#First we generate 1000 samples by the negative binomial of the same size as the one provided at the beginning.



num.sim<-19
Means_sim<-rep(0,1000)
Matr_sim<-matrix(0,1000,num.sim)

for(i in 1:1000){
  Matr_sim[i,]<-rnbinom(num.sim, size = s_post, prob = r_post / (1 + r_post))
}

for(i in 1:1000){
  Means_sim[i]<-sum(Matr_sim[i,])/19
}
y_bar_sim<-sum(Means_sim)/1000


#Through this vector we can compare the average of the means of the simulations to the sample one of the dataset:


cbind(y_bar_sim,y_bar)


#The two values turn out to be very similar to each other, so it is possible to say that on average the mean number of observed car accidents is close to those predicted by the posterior predictive distribution.
