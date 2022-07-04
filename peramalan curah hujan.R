library(forecast)
library(TTR)
library(graphics)

hujan <- read.csv("D:/IKWAN/SEMESTER 6/ANALISIS RUNTUN WAKTU/hujan.csv", header = TRUE,sep = ";")
hujan
hujan.ts <- ts(hujan, start = c(1813))
hujan.ts

#SMA
hujan.sma <- SMA(hujan.ts,7)
cbind(hujan.ts,hujan.sma)

#plot
plot(hujan.ts,xlab="Tahun",ylab="Curah Hujan",lty=1,col="black")
points(hujan.ts)
lines(hujan.sma, col = "purple")

#Prediksi
phujan.sma <- lag(hujan.sma,-1)
phujan.sma
sma <- cbind(hujan.ts,hujan.sma,phujan.sma)
sma

#Evaluasi
SSE <- sum((phujan.sma-hujan.ts)^2,na.rm=T)
SSE
MSE <- mean((phujan.sma-hujan.ts)^2,na.rm=T)
MSE
MAPE <- mean(abs((hujan.ts-phujan.sma)/hujan.ts),na.rm=T)
MAPE


