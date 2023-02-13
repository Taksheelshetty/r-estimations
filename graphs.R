# To plot a bar graph (Time vs Volatility)
data=read.csv("highlow.csv")
print(data)

v=data.frame(data$opelow)
c=data.frame(data$time)
list1=as.list(c)
list=as.list(v)
list.time=c("time")
list.names=c("openlow")
num <- as.numeric(unlist(list))
num1<-as.numeric(unlist(list1))

png(filename = "graph1.png")

barplot(num, names.arg = num1, xlab ="time", 
        ylab ="volatility", col ="red", 
        main ="Volatility graph")
dev.off()
#_______________________________________________________________
#overlay of both open low and close high 
png(filename = "graph2.png")
plot(density(num) )
polygon(density(num), col = "blue")
abline(v = mean(num), col = "green")
lines(density(num2), col = "red")                     
dev.off()

#predicting volatity for each minute 
lm.opelow=lm(data$opelow~data$Volume.1000+data$higclose,data=data)
lm.predict.opelow=predict(lm.opelow,data,type="response")
print(lm.predict.opelow)
data$lm.predict.opelow=predict(lm.opelow,data,type="response")
write.csv(prediction,"predict.csv")
print(data$lm.predict.opelow)
print(summary(model))
png(filename = "prediction_plot.png")
plot(x=data$time,y=data$lm.predict.opelow,xlab="time",ylab="predicted volatility",
     main = "Relationship b/n time and price",las=2,cex=0.2,col="blue")
dev.off()
#Prediction of Linear Regression for the given Value
p=data.frame(Volume.1000=5000,highclose=21.2)
prediction=predict(lm.opelow,p)
print(prediction)
#regression line for volatility vs time
png(filename = "Linear_Regression.png")
plot(x=data$time,y=data$opelow,xlab = "time",ylab = "volatility",
     main = "Volatility vs Time",las=2,col="yellow",cex=0.2,abline(lm(data$opelow~data$time)))
dev.off()

#--------------------------------------------------------------------------------------------
#To plot a graph (Time vs Volatility)
png(filename = "high2.png")
plot(x=data$time,y=data$opelow,xlab="time",ylab="volatility",
     main = "Time vs Volatility ",las=2,cex=0.2,col="red")
dev.off()


#------------------------------------------------------------------------------------------------------------
#To plot a graph (Time vs Volume)
png(filename = "high3.png")
plot(x=data$time,y=data$Volume.1000,xlab="time",ylab="volume",
     main = "Time vs Volume ",las=2,cex=0.2,col="green")
dev.off()
