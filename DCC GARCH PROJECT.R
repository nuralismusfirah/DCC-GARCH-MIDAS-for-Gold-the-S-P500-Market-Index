library(tseries)
library(rugarch) 
library(rmgarch)

data<-read.csv("C:/Users/alis/OneDrive/Documents/Book1.csv")

rgold <- diff(log(data$data.Gold)) * 100
rsp500<-diff(log(data$data.SP500))*100

uspec<-ugarchspec(mean.model=list(armaOrder=c(0,0)),
                  variance.model=list(garchOrder=c(1,1),model="sGARCH"),# GARCH(1,1)model 
                  distribution.model="sstd")

spec2<-dccspec(uspec=multispec(replicate(2,uspec)),
               dccOrder=c(1,1),
               distribution="mvnorm")

fit1<-dccfit(spec2,data.frame(rgold,rsp500))

dccforecast(fit1,n.ahead=5)

cor1<-rcor(fit1)
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_rsp500<-cor1[2,1,]
plot_sp500<-plot.ts(cor_rsp500,col="blue",main="DCC GARCH EMAS-INDEKS PASARAN S&P 500",ylab="INDEKS",xlab="MASA")
all_params<-coef(fit1)
all_params<-round(all_params,4)
params<-data.frame(Parameter=names(all_params),Value=all_params)
print(params)
x_values<-c(25,60,95)
y_values<-cor_rsp500[x_values]
points(x_values,y_values,col="red",pch=19) 
text(x_values,y_values,labels=round(y_values,4
),pos=3,col="black") 
cat("The y-values for x=",x_values,"are:",y_values,"")