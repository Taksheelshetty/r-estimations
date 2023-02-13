data=read.csv("llb.csv")
print(data)
print(summary(data))
prediction=lm(formula =  Open+Close ~Volume+Openvix+P.E+Change,data=data)
print(summary(prediction))
p=data.frame(Openvix = 12.74   , Volume = 231989000, P.E = 21.1,P.B=4.2, Change =  -0.60,Close= 17837)
model=predict(prediction,newdata=p)
print(model)
install.packages("rsq")
library(rsq)
library(caret)
#nueral linkk
install.packages("nnet")
library("nnet")
fit <- nnet(Open ~ Volume + Openvix + P.E+P.B + Change, data = data, size = 1)
# Generate predictions
p <- data.frame(Openvix = 43.89, Volume = 261792179, P.E = 13.27, Change = -0.28)
predictions <- predict(fit, newdata = p)
# Print the predictions
print(predictions)

library(randomForest)
fit <- randomForest(((Open+Close)) ~ Volume + Openvix + P.E+P.B + Change, data = data)

# Generate predictions
p <- data.frame(Openvix = 18.58   , Volume = 339578445, P.E = 37.27,P.B=3.30 , Change =-0.93,Close=  4080.95)
predictions <- predict(fit, newdata = p)

# Print the predictions
print(predictions-2394)



model <- lm(formula =  Open+Close ~Volume+Openvix+P.E+Change,data=data)
predictions <- predict(model, data)
residuals <- data$Open - predictions
print(residuals)
rse <- sqrt(mean(residuals^2))
print(rse)
