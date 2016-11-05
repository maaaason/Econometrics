library(ggplot2)
library(magrittr)

samplemean <- function(Sample_size,normalize,df,rep){
  data <- as.data.frame(matrix(NA,length(Sample_size)*rep,2))
    for(j in 1:length(Sample_size)){
    for(i in 1:rep){
    data[i+(j-1)*rep,1] <- as.character(Sample_size[j])
    data[i+(j-1)*rep,2] <- Sample_size[j] %>% rt(.,df) %>% {
      if(normalize == T) mean(.)/sqrt(var(.)/Sample_size[j]-1) else mean(.)
    }
        }
      
    }
  colnames(data) <- c("Sample_size","Sample_Mean")
  return(data)
}
#1
sample1 <- c(10,50,100,1000)
ans1 <- samplemean(Sample_size = sample1, normalize = F, df = 3, rep = 10000)

ggplot(data=ans1,aes(Sample_Mean,group=Sample_size,colour=Sample_size))+
  geom_density()+
  xlim(-5,5)+
  ggtitle("Weak law of large number")+
  xlab("Sample Mean")
  

#2
sample2 <- c(3,5,10,20,500)
ans2 <- samplemean(Sample_size = sample2, normalize = T, df = 3, rep = 10000)

ggplot(data=ans2,aes(sample=Sample_Mean,group=Sample_size,colour=Sample_size))+
  stat_qq(size=0.5)+
  ggtitle("Central Limit Theorem")+
  xlab("theoretical")+
  ylab("sample")+
  xlim(-5,5)+
  ylim(-5,5)