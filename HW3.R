library("ggplot2")

##assignment 1
setwd("C:/Users/admin/Desktop/Mason/Junior/Econometrics/HW")
gdp <- read.csv(file("./R data/weoreptc.xls"),header=T,sep="\t")
head(gdp)
str(gdp)

##assignment 2
gdp <- gdp[1:4,]
Country <- sapply(gdp$Country,function(x) gsub("Taiwan Province of China", "Taiwan",x))
gdp <- gdp[,6:41]
gdp <- sapply(gdp,function(x) gsub(",","",x))
class(gdp) <- "numeric"
gdp <- as.data.frame(gdp)
gdp <- cbind(Country,gdp)
gdp <- reshape(gdp,varying=names(gdp)[-1],v.names="GDP",idvar = "Country",timevar="Year",times=1980:2015,direction = "long")
gdp <- gdp[with(gdp,order(Country)),]

##assignment 3
ggplot(data=gdp, aes(x=Year, y=GDP, group=Country, colour=Country))+
  ggtitle("Cross-country Comparison of GDP")+
  geom_line(size=1.7)+
  geom_point(size=2)+
  ylab("GDP(U.S dollars)")