# 2a
   
script_S=c(1,2,3)  

matr<-matrix(c(0,0.5,0.5,5/8, 1/8, 1/4, 2/3, 1/3,0),nrow=3,byrow=T)
round(matr,2)
x0 <- 1 

nsample<-1000
chain<-rep(NA,nsample+1) 

chain[1]<-x0             

t <- 0
for(t in 1:nsample){
  chain[t+1]<-sample(x=script_S,size=1,prob=matr[chain[t],])
}
head(chain)
 

# 2b

   
sim1<-prop.table(table(chain))
sim1
 

# 2c

   
save_val<-rep(0,500)

for(i in 1:500){
  t <- 0
  for(t in 1:nsample){
    chain[t+1]<-sample(x=script_S,size=1,prob=matr[chain[t],])
  }
  save_val[i]<-chain[1000]
}

sim2<-prop.table(table(save_val))
sim2
 

# We are tryng to approximate the stationary distribution $\pi$
  
  
  
  
  
  
  
  
# 2d
  
# It is known that $\pi$ is stationary if $\pi = \pi P$,where:
  
#  * $\pi$ is the probability vector over the state space $S$ (the conditions are $\sum_{i \in S}\pi_{i}=1$ and $\pi_{i} \geq 0$ $\forall i \in S$)
#* P is the transition matrix

#$\Longrightarrow$
  
#  Thus, it's possible to find the theoretical stationary distribution through the solution of the system reported:


# \begin{cases}
# \pi_{1} =  \frac{5}{8}\pi_{2} + \frac{2}{3}\pi_{3}\\
# \pi_{2} = \frac{1}{2}\pi_{1} + \frac{1}{8}\pi_{2} + \frac{1}{3}\pi_{3}\\
# \pi_{3} = \frac{1}{2}\pi_{1} + \frac{1}{4}\pi_{2}\\
# \pi_{1}+\pi_{3}+\pi_{3}=1

# \end{cases}


   
pi_vect <- eigen(t(matr))$vector[,1]/sum(eigen(t(matr))$vector[,1])
pi_vect <-as.numeric(round(t(matr)%*%pi_vect,3))


 


# 2e

   
sim1<-as.numeric(round(sim1,3))
sim2<-as.numeric(round(sim2,3))
rbind(pi_vect,sim1,sim2)
 

#The two simulations approximate the theoretical values of $\pi_i$ fairly well:
#the second simulated state in sim2 is exactly the real value of $\pi_2$, whereas for $\pi_3$ the two simulations have roughly the same distance to the real value.
#In the end $\pi_1$ is well approximated by sim1.

# 2f

   
script_S=c(1,2,3)  

matr<-matrix(c(0,0.5,0.5,5/8, 1/8, 1/4, 2/3, 1/3,0),nrow=3,byrow=T)

x0 <- 2

nsample<-1000
chain2<-rep(NA,nsample+1) 

chain2[1]<-x0            

t <- 0
for(t in 1:nsample){
  chain2[t+1]<-sample(x=script_S,size=1,prob=matr[chain[t],])
}



prop.table(table(chain2))

 

# It is possible to say that we are closer to the real values of $\pi_i$, even if only slightly.
