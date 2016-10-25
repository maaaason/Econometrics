library(ggplot2)
set.seed(5000)

a <- as.data.frame(matrix(c(rep("switch",18),rep("stay",18)),36,1))
b <- cbind(seq(3,20,1),a,NA)
colnames(b) <- c("door_num","Choice","Probability")

game <- function(b,x,times){
 count = 0
 door_num <- b[x,"door_num"]
 for(i in 1:times){
   car <- sample(1:door_num,1)
   bet <- sample(1:door_num,1)
   empty <- sample(1:door_num,1)
   while(empty == car || empty == bet){
   empty <- sample(1:door_num,1)
   } 
   if(b[x,"Choice"] == "switch"){
   bet_2 <- sample(1:door_num,1)
   while(bet_2 == empty || bet_2 == bet){
   bet_2 <- sample(1:door_num,1)
   }
   if(bet_2 == car) count <- count + 1
   }
   else if(bet == car) count <- count+1   
  }
 return(count/times)
 }
for(i in 1:nrow(b)) b[i,"Probability"] <- game(b,i,10000)

ggplot(data=b,aes(x=door_num,y=Probability,group=Choice,colour=Choice))+
ggtitle("Monty Hall by simulation in R")+
geom_line(size=1)+
geom_point(size=1)+
xlab("Number of door")
