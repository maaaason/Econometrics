#1
x1 <- matrix(c(-0.13015913,-1.14061508,-2.18437711 ,0.76502176,-0.47352958,-0.39803977,1.03822206,-1.05651530,-0.03758343,1.26600498))
x2 <- matrix(c(0.63578924,0.04538897,0.33247965,0.72683247,0.39043812,0.53607313,0.74997784,0.62636748,0.89519293,0.17034668))
y <- matrix(c(4.7864257,-0.4831901,-3.4338635,7.1662819,4.2470952,3.9809770,7.4340378,1.3553170,5.6138815,7.7252616))

#2
X  <- cbind(1,x1,x2)
colnames(X) = c("Intercept","x1","x2")
OLS_estimator <- solve(t(X) %*% X) %*% t(X) %*% y
Residuals <- y - X %*% OLS_estimator
Variance <- sum(Residuals^2/7)
Standard_error <- sqrt(Variance)
answer2 <- list(OLS_estimator = OLS_estimator,Residuals = Residuals,Variance = Variance,Standard_error = Standard_error)
str(answer2)

#3
X <- cbind(x1,x2)
colnames(X) = c("x1","x2")
OLS_estimator <- solve(t(X) %*% X) %*% t(X) %*% y
Residuals <- y - X %*% OLS_estimator
Variance <- sum(Residuals^2/8)
Standard_error <- sqrt(Variance)
answer3 <- list(OLS_estimator = OLS_estimator,Residuals = Residuals,Variance = Variance,Standard_error = Standard_error)
str(answer3)
