## assignment 1
setwd("C:/Users/admin/Desktop/Mason/Junior/Econometrics/HW")
gdp <- read.table("./R data/tw_gdp.csv",header=T,sep=",")
head(gdp)
str(gdp)

##assignment 2
gdp <- read.table("./R data/tw_gdp.csv",header=T,sep=",",skip=3)
gdp$Year <- as.numeric(as.character(gdp$Year))
gdp <- na.omit(gdp)

##assignment 3
Msquare <- function(x) t(x) %*% x

#with intercept
x1 <- cbind(1,gdp[,"Year"])
y <- c(gdp[,"GDP"])
OLS_estimator1 <- solve(t(x1) %*% x1) %*% t(x1) %*% y
Residuals1 <- y-x1 %*% OLS_estimator1
Centered_Rsquare1 <- Msquare(x1 %*% OLS_estimator1-mean(y))/Msquare(y-mean(y))

#without intercept
x2 <- gdp[,"Year"]
OLS_estimator2 <- solve(t(x2) %*% x2) %*% t(x2) %*% y
Residuals2 <- y-x2 %*% OLS_estimator2
Centered_Rsquare2 <- Msquare(x2 %*% OLS_estimator2-mean(y))/Msquare(y-mean(y))

##assignment 4
#When we regressing gdp(y) on year(x) without intercept, we actually add one more constraint to
#the regression; that is, y=ax for some a.
#Under that assumption, the regression line can move only by rotating about the origin, so 
#usually it's not the best fit and increases the ESS(decreases the RSS).
#Consequently, the centered R-square will drop.

##assignment 5
plot(gdp[,1],Residuals1,type="l",col="blue",xlab="Years",ylab="Residuals",main="With Intercept",lwd=2)
plot(gdp[,1],Residuals2,type="l",col="blue",xlab="Years",ylab="Residuals",main="Without Intercept",lwd=2)
