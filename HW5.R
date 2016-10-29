library(ggplot2)
library(invgamma)
library(magrittr)
#1
set.seed(5000)
sample.size <- 100000
upper.bound.of.x.axis <- 50
upper.bound.of.y.axis <- 0.275
x.axis <- runif(sample.size,0,upper.bound.of.x.axis)
y.axis <- runif(sample.size,0,upper.bound.of.y.axis)
data <- as.data.frame(cbind(y.axis,x.axis))
seq <- seq(0.0001,upper.bound.of.x.axis,0.0001)
invgamma <- seq %>% dinvgamma(.,1,2) %>% as.data.frame(.)
ggplot(invgamma,aes(x=seq,y=invgamma))+
  geom_point(data=data,aes(x=x.axis,y=y.axis),colour="black",size=0.00001)+
  geom_line(size=1,colour="red")+
  xlab("Support Set")+
  ylab("Probability Density")
#2
cdf <- function(L,U,data){
  count <- 0
  all   <- 0
  for(i in 1:length(data$x.axis)) if(data$y.axis[i] < dinvgamma(data$x.axis[i],1,2)) all <- all+1
  target <- data[(data$x.axis > L & data$x.axis < U),]
  for(i in 1:length(target$x.axis)) if(target$y.axis[i] < dinvgamma(target$x.axis[i],1,2)) count <- count+1
  return(count/all)
  }
cdf(2,5,data) %>% print(.)