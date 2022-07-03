

suppressWarnings(suppressMessages(library("R2jags", quietly = T)))
suppressWarnings(suppressMessages(library("LaplacesDemon", quietly = T)))
suppressWarnings(suppressMessages(library("psych", quietly = T)))


## 1

# 1a

set.seed(1234)
my_data <- read.table("dugong-data.txt",header=T)
my_data <- my_data[,-1]



hist(my_data$Length,breaks = "Freedman-Diaconis", col = "Orange", main="Histogram of the Length",ylim=c(0,10),xlab="Length")



round(mean(my_data$Length),2)



#In the small data sample, the distribution of heights seems to take with high probability the values between 2.2 and 2.6, in fact the sample mean is 2.334



hist(my_data$Age, breaks = "Freedman-Diaconis" , col = 4, main="Histogram of the Age",ylim=c(0,10),xlab="Age")




summary(my_data)


 #From the age histogram we can see that as the age increases, the number of individual decreases.
#For example, only 3 out of 27 individuals are over 20 years of age.
#The average Age is 10.94.

#The average Length is 2.334




plot(my_data$Age, my_data$Length,ylab="Lenght", xlab="Age",col="#2F4F4F", pch=16,main="Plot of Length and Age")


#It's possible to notice that there is a relationship between the Age and the Length of Dugongs:


cor(my_data$Length, my_data$Age)


#The two variables are very strong correlated, that connection was to be expected.

# 1b

#We start from the likelihood function of the normal distribution:
#$$\prod_{i=1}^{n} \frac{1}{\sqrt{2 \pi \tau^2}} exp \{-\frac{(y_i - \mu_i#)^2}{2\tau^2}\} $$
#Now we have to substitute $\mu_i$ with $\alpha - \beta \gamma^{x_i}$

#$$\prod_{i=1}^{n} \frac{1}{\sqrt{2 \pi \tau^2}} exp \{-\frac{(y_i - (\alpha - \beta \gamma^{x_i}))^2}{2\tau^2}\} $$
#$\Longrightarrow$

#$$ (2 \pi \tau^2)^{-\frac{n}{2}} * exp \{- \sum_{i=1}^{n}\frac{(y_i - (\alpha - \beta #\gamma^{x_i}))^2}{2\tau^2}\} $$


# 1c


#Under the condition of indipendence of the four parameters $\alpha ,\beta, \gamma, \tau^2$, the joint prior distribution can be obtained with the product of the priors

#$\pi(\alpha ,\beta, \gamma, \tau^2) = \pi(\alpha)\pi(\beta)\pi(\gamma)\pi(\tau^2)$, where:

#* $\pi(\alpha) \sim N(0,\sigma^2_{\alpha}) \propto \exp\{\frac{- \alpha^2}{2\sigma_{\alpha}^2}\}$
#* $\pi(\beta)  \sim  N(0,\sigma^2_{\beta}) \propto \exp\{\frac{- \beta^2}{2\sigma_{\beta}^2}\}$
#* $\pi(\gamma) \sim  Unif(0,1) = 1$
#* $\pi(\tau^2) \sim  IG(a,b)) \propto {\tau^2}^{(-a-1)}exp\{\frac{-b}{\tau^2}\}$

#$\alpha \in (1, \infty)$,
#$\beta \in (1, \infty)$,
#$\gamma \in (0,1)$,
#$\tau^2 \in (0,\infty)$


#$\Longrightarrow$

#$\pi(\alpha ,\beta, \gamma, \tau^2) = \exp\{\frac{- \alpha^2}{2\sigma_{\alpha}^2}\}#*\exp\{\frac{- \beta^2}{2\sigma_{\beta}^2}\}*{\tau^2}^{(-a-1)}*exp\{\frac{#-b}{\tau^2}\}$
#$\Longrightarrow$
#$\pi(\alpha ,\beta, \gamma, \tau^2) =exp\{-(\frac{\alpha^2}{2\sigma_{\alpha}^2}#+\frac{\beta^2}{2\sigma_{\beta}^2}+\frac{b}{\tau^2})\}*{\tau^2}^{(-a-1)}*1_{\alpha}(1, #\infty)*1_{\beta}(1, \infty)*1_{\gamma}(0,1)*1_{\tau^2}(0, \infty)$

#The value of the hyperparameters are:

#* $\sigma_a = 1$

#* $\sigma_b = 1$

#* $a = 0.5$

#* $b = 0.05$

#I chose those parameters taking into account the condition that the variances of the $#\alpha$ and $\beta$ distributions must not be negative.

# 1d

#The full conditionals are:

#* $P(\alpha|\beta,\gamma,\mu_i,\tau^2,y) \propto p(\alpha)p(\mu_i|\alpha,\beta,\gamma) \propto (2 \pi \tau^2)^{-\frac{n}{2}}  exp \{- \sum_{i=1}^{n}\frac{(y_i - (\alpha - \beta \gamma^{x_i}))^2}{2\tau^2}\}* exp\{ \frac{-\alpha^2}{2\sigma_\alpha^2}\} \propto$

#$\propto (2 \tau^2)^{-\frac{n}{2}}* exp \{- \sum_{i=1}^{n}\frac{(y_i - (\alpha - \beta \gamma^{x_i}))^2- \frac{-\alpha^2}{2\sigma_\alpha^2}}{2\tau^2}\}$

#* $P(\beta|\alpha,\gamma,\mu_i,\tau^2,y)\propto p(\beta)p(\mu_i|\alpha,\beta,\gamma)\propto(2 \pi \tau^2)^{-\frac{n}{2}}  exp \{- \sum_{i=1}^{n}\frac{(y_i - (\alpha - \beta \gamma^{x_i}))^2}{2\tau^2}\}* exp\{ \frac{-\beta^2}{2\sigma_\beta^2}\} \propto$

#$\propto (2 \tau^2)^{-\frac{n}{2}}* exp \{- \sum_{i=1}^{n}\frac{(y_i - (\alpha - \beta \gamma^{x_i}))^2- \frac{-\beta^2}{2\sigma_\beta^2}}{2\tau^2}\}$

#* $P(\gamma |\beta,\alpha,\mu_i,\tau^2,y)\propto p(\gamma)p(\mu_i|\alpha,\beta,\gamma) \propto exp \{- \sum_{i=1}^{n}\frac{(y_i - (\alpha - \beta \gamma^{x_i}))^2}{2\tau^2}\}#*1_\gamma(0,1)$

#* $P(\tau^2|\beta,\gamma,\mu_i,\tau^2,y)\propto p(\tau^2)p(y|\mu_i,\tau^2) \propto (\tau^2)^{(-a-1)}exp\{ \frac{-b}{\tau^2}\} *(2 \pi \tau^2)^{-\frac{n}{2}}  exp \{- \sum_{i=1}^{n}\frac{(y_i - (\alpha - \beta \gamma^{x_i}))^2}{2\tau^2}\} \propto$

#$\propto exp \{- \sum_{i=1}^{n}\frac{(y_i - (\alpha - \beta \gamma^{x_i}))^2 - \frac{b}{\tau^2}}{2\tau^2}\}* (\tau^2)^{(-\frac{n}{2}-a-1)}$


# 1e

#$\alpha \sim N(\mu_\alpha1,\sigma^2_\alpha1)$

#Where:

#*$\mu_\alpha1 = \frac{\sum_{i=1}^n(y_i + \beta\gamma^{x_i})}{\tau^2(\frac{1}{\sigma^2_\alpha}+\frac{n}{\tau^2})}$

#*$\sigma^2_\alpha1 = \frac{1}{\frac{1}{\sigma^2_\alpha}+\frac{n}{\tau^2}}$


#$\beta \sim N(\mu_\beta1,\sigma^2_\beta1)$

#Where:

#*$\mu_\beta1 = \frac{\sum_{i=1}^n(-y_i\gamma^{x_i} + \alpha\gamma^{x_i})}{\tau^2(\frac{1}{\sigma^2_\beta}+\frac{\sum_{i=1}^n\gamma^{2x_i}}{\tau^2})}$

#*$\sigma^2_\beta1 = \frac{1}{\frac{1}{\sigma^2_\beta}+\frac{\sum_{i=1}^n\gamma^{2x_i}}{\tau^2}}$

#$\tau^2 \sim IG(a1= a + \frac{n}{2},b1= b + \frac{1}{2}\sum_{i=1}^n(y_i-\alpha+\beta\gamma^{x_i})^2)$

# 1f

#Through the jags function, it is possible to perform an MC simulation:


t = 10000 #steps
mydata <- list(   x = c( 1.0,  1.5,  1.5,  1.5, 2.5,   4.0,  5.0,  5.0,  7.0,
                         8.0,  8.5,  9.0,  9.5, 9.5,  10.0, 12.0, 12.0, 13.0,
                         13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
                  Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
                        2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
                        2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57), N = 27)
parameters = c('alpha', 'beta', 'gamma', 'tau') 


inits = list(alpha = 1.2, beta = 1.2, tau = 1, gamma = 0.5)  
inits = list(inits) 

datajags = jags(data = mydata, inits = inits, parameters.to.save = parameters, model.file = 'Model.txt', n.chains = 1, n.thin = 1, n.burnin = 10, n.iter = t)



# 1g

#From all the trace plots, it's possible to see that every parameter is well-balanced at around the average:
  
  
plot(datajags$BUGSoutput$sims.matrix[,"alpha"],type="l",ylim=c(2.2,3), main="Traceplot of alpha", col = "#CD6090",xlab="t", ylab="")
plot(datajags$BUGSoutput$sims.matrix[,"beta"],type="l", main="Traceplot of beta", col = "royalblue3",xlab="t", ylab="",ylim=c(0.8,1.5))
plot(datajags$BUGSoutput$sims.matrix[,"gamma"],type="l", main="Traceplot of gamma", col = "#008B00",xlab="t", ylab="",ylim=c(0.6,1.5))
plot(datajags$BUGSoutput$sims.matrix[,"tau"],type="l", main="Traceplot of tau", col = "#EE7600",xlab="t", ylab="",ylim=c(0,0.004))




# 1h


n = length(datajags$BUGSoutput$sims.array[,,1])
s = 0
a_t = rep(NA, n)
b_t = rep(NA, n)
g_t = rep(NA, n)
t_t = rep(NA, n)


for(i in 1:n){
  s = s + datajags$BUGSoutput$sims.array[i,,1] 
  a_t[i] = s/i
}

s=0

for(i in 1:n){
  s = s + datajags$BUGSoutput$sims.array[i,,2] 
  b_t[i] = s/i
}

s=0

for(i in 1:n){
  s = s + datajags$BUGSoutput$sims.array[i,,3] 
  g_t[i] = s/i
}

s=0

for(i in 1:n){
  s = s + datajags$BUGSoutput$sims.array[i,,4] 
  t_t[i] = s/i
}




plot(a_t, type="l", col = "#CD6090", lwd=3,xlab="Iteration",ylab=expression(alpha),main= "Empirical average of alpha")
plot(b_t, type="l", col = "royalblue3",lwd=3,xlab="Iteration",ylab=expression(beta),main="Empirical average of beta")
plot(g_t, type="l", col = "#008B00",lwd=3,xlab="Iteration",ylab=expression(gamma),main="Empirical average of gamma")
plot(t_t, type="l", col = "#EE7600",lwd=3,xlab="Iteration",ylab=expression(tau),main="Empirical average of tau")




#library(ggmcmc)
#s=ggs(as.mcmc(datajags))
#ggs_running(s, family="alpha")


# 1i

#It's possible to estimate the parameters by the jags function:


alpha_e<-as.numeric(datajags$BUGSoutput$mean['alpha'])
beta_e<-as.numeric(datajags$BUGSoutput$mean['beta'])
gamma_e<-as.numeric(datajags$BUGSoutput$mean['gamma'])
tau_e<-as.numeric(datajags$BUGSoutput$mean['tau'])
matrixmean<-data.frame(alpha_e,beta_e,gamma_e,tau_e)
matrixmean



#In order to estimate the approximation error, one can use the Monte Carlo Standard Error, that is an estimate of the inaccuracy of Monte Carlo samples.
#mcse function was used in this case.


alpha_mcse<-MCSE(datajags$BUGSoutput$sims.array[,1,"alpha"])
beta_mcse<-MCSE(datajags$BUGSoutput$sims.array[,1,"beta"])
gamma_mcse<-MCSE(datajags$BUGSoutput$sims.array[,1,"gamma"])
tau_mcse<-MCSE(datajags$BUGSoutput$sims.array[,1,"tau"])
matrixmcse<-data.frame(alpha_mcse,beta_mcse,gamma_mcse,tau_mcse)
matrixmcse


# 1l

#Standard deviation can be used to get an idea of posterior uncertainty


sd_alpha = datajags$BUGSoutput$sd$alpha
sd_beta = datajags$BUGSoutput$sd$beta
sd_gamma = datajags$BUGSoutput$sd$gamma
sd_tau = datajags$BUGSoutput$sd$tau

matrixsd_param<-data.frame(sd_alpha, sd_beta, sd_gamma, sd_tau)
matrixsd_param


#In that case the $\beta$ parameter has the greater posterior uncertainty.

# 1m


corPlot(datajags$BUGSoutput$sims.matrix[,-3],
        scale = FALSE)



From the correlation plot, the couple having the highest correlation is ($\alpha$,$\gamma$) 


# 1n


Prediction_20 <- jags(data=mydata, inits=inits, parameters.to.save = c("Ypred20"), model.file = 'Model.txt', n.chains=1,n.iter=10000,n.thin = 1,n.burnin = 1)



print(Prediction_20)


#Analyzing the output of the model, it can be stated that the expected length value for a 20-year dugong is 2,582 meters.

# 1o



Prediction_30 <- jags(data=mydata, inits=inits, parameters.to.save = c('Ypred30'), model.file = 'Model.txt', n.chains=1,n.iter=10000,n.thin = 1,n.burnin = 1)




print(Prediction_30)


#The expected length value for a 30-year dugong is 2,624 meters.

# 1p

#It's possible to use the Standard deviation to evaluate the less precise prediction.


standard_dev_20 = Prediction_20$BUGSoutput$sd[1]
standard_dev_30 = Prediction_30$BUGSoutput$sd[1]



matrixsd<-data.frame(standard_dev_20,standard_dev_30)
colnames(matrixsd)<-c("Sd of Ypred20","Sd of Ypred30")
matrixsd


#From the output it is possible to state that the least accurate prediction is the one with the highest sd, that is Ypred30.