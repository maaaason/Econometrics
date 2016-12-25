library(ggplot2)

B<- 10
n <- seq(25,300,by=25)
rep <- 200
result <- data.frame(sample_size = n , test_size = rep(0,length(n)), power=rep(0,length(n)))

for(i in 1:length(n)){
  rej_num1 <- 0
  rej_num2 <- 0
  for(j in 1:rep){
    X <- cbind(1,matrix(rnorm(2*n[i],0,1),n[i],2))
    y <- X %*% c(-2,0.5,5) + rnorm(n[i],0,1)
    b_hat <- solve(t(X) %*% X) %*% t(X) %*% y
    residual <- y - X %*% b_hat
    
    re_beta <- matrix(0,B,3)
    re_dist <- matrix(0,B,3)
    for(k in 1:B){
      index <- sample(residual,n[i],replace = T)
      re_y <- X %*% b_hat + index
      re_beta[k,] <- solve(t(X) %*% X) %*% t(X) %*% re_y
      re_dist[k,] <- sqrt(n[i])*(re_beta[k,] - b_hat)
      t_s1 <- sqrt(n[i])*(b_hat[3]-5)
      t_s2 <- sqrt(n[i])*(b_hat[2]-0)
    }
    rej_rule1 <- (t_s1 < quantile(re_dist[,3],0.025) | t_s1 > quantile(re_dist[,3],0.975))
    rej_num1 <- rej_num1 + rej_rule1
    rej_rule2 <- (t_s2 < quantile(re_dist[,2],0.025) | t_s2 > quantile(re_dist[,2],0.975))
    rej_num2 <- rej_num2 + rej_rule2
  }
  result$test_size[i] <- rej_num1 / rep
  result$power[i] <- rej_num2 / rep
}
result <- reshape(result, varying = c("test_size", "power"), v.names = "probability",timevar = "Methods", times = c("test_size", "power"), direction = "long",idvar = "sample_size",ids=n)
ggplot(result,aes(x=sample_size), group = Methods , colours = Methods)+
  geom_line(aes(y=probability),size=1)+
  geom_point(aes(y=probability),size=1)+
  labs(x="Sample Size",y="Test size and Power",title="Residual Bootstrap")

