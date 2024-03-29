# We want to make inference on the n parameter of a binomial distribution, we only now the p parameter of the binomial that's equal to 0.45 and
# we have observed a data point s_obs = 12

# we fix the maximum n equal to 200


# obs
s_obs<-12



# function for n
likelihood <- function(N){
  out <- dbinom(x=s_obs,size=N,p=0.45)
  return(out)
}


format(likelihood(12), scientific=FALSE) # likelihood for N=12

format(likelihood(12)* (1/200), scientific=FALSE) # it's a non-conjugate example --> likelihood*prior and also the prior is flat


posterior<-function(n){
  out<-likelihood(n)*(1/200)
  return(out)
}

# mode
which.max(posterior(1:200))

# mean
sum((1:200)*(posterior(1:200)/sum(posterior(1:200)))) 

# median 
(1:200)*(cumsum(posterior(1:200)/sum(posterior(1:200)))>=0.5) # ----> 27




  
