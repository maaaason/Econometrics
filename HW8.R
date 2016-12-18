library(ggplot2)

B <- 1000
n <- seq(50,500,by=50)
rep <- 2000

#1

As <- rep(0,length(n))
Bs <- rep(0,length(n))

for(i in 1:length(n)){
  a_count <- 0
  b_count <- 0
  for(j in 1:rep){
    x <- rnorm(n[i],0,2)
    sample_mean <- mean(x)
    a_U <- mean(x) + qnorm(0.975)*sd(x)/sqrt(n[i])
    a_L <- mean(x) - qnorm(0.975)*sd(x)/sqrt(n[i])
    a_count <- a_count + (0 >= a_L & 0 <= a_U)
    
    re_dis <- rep(0,B)
    M <- replicate(B,sample(x,n[i],replace=T))
    resample_mean <- colMeans(M)
    re_dis <- sqrt(n[i])*(resample_mean - sample_mean)
    t <- sqrt(n[i])*(sample_mean - 0)
    b_count <- b_count + (t >= quantile(re_dis,0.025) & t <= quantile(re_dis,0.975))
  }
  As[i] <- a_count/rep
  Bs[i] <- b_count/rep
}
data <- data.frame(As,Bs)
data <- reshape(data, varying = c("As", "Bs"), v.names = "probability",timevar = "Methods", times = c("As", "Bs"), direction = "long",idvar = "sample_size", ids = n)
ggplot(data,aes(x = sample_size , group = Methods , colour = Methods))+
  geom_line(aes(y=probability),size=1)+
  geom_point(aes(y=probability),size=1)+
  labs(x="Sample Size",y="Coverage Probability",title="Normal(0,2)")

#2
Bootstrap <- rep(0,length(n))
for(i in 1:length(n)){
  b_count <- 0
  for(j in 1:rep){
    x <- rlnorm(n[i],0,2)
    re_dis <- rep(0,B)
    M <- replicate(B,sample(x,n[i],replace=T))
    resample_mean <- colMeans(M)
    re_dis <- sqrt(n[i])*(resample_mean - mean(x))
    t <- sqrt(n[i])*(mean(x) - 0)
    b_count <- b_count + (t >= quantile(re_dis,0.025) & t <= quantile(re_dis,0.975))
  }
  Bootstrap[i] <- b_count/rep
}
data <- data.frame(Bootstrap)
data <- reshape(data,varying = c("Bootstrap"), v.names = "probability", timevar = "Methods",times = c("Bootstrap"),direction = "long",idvar = "sample_size",ids = n)
ggplot(data,aes(x=sample_size))+
  geom_line(aes(y=probability),size=1)+
  geom_point(aes(y=probability),size=1)+
  labs(x="Sample Size",y="Coverage Probability",title="Lognormal")
#3
Bootstrap <- rep(0,length(n))
for(i in 1:length(n)){
  b_count <- 0
  for(j in 1:rep){
    x <- rt(n[i],4)
    re_dis <- rep(0,B)
    M <- replicate(B,sample(x,n[i],replace=T))
    resample_mean <- colMeans(M)
    re_dis <- sqrt(n[i])*(resample_mean - mean(x))
    t <- sqrt(n[i])*(mean(x) - 0)
    b_count <- b_count + (t >= quantile(re_dis,0.025) & t <= quantile(re_dis,0.975))
  }
  Bootstrap[i] <- b_count/rep
}
data <- data.frame(Bootstrap)
data <- reshape(data,varying = c("Bootstrap"), v.names = "probability", timevar = "Methods",times = c("Bootstrap"),direction = "long",idvar = "sample_size",ids = n)
ggplot(data,aes(x=sample_size))+
  geom_line(aes(y=probability),size=1)+
  geom_point(aes(y=probability),size=1)+
  labs(x="Sample Size",y="Coverage Probability",title="t(4)")