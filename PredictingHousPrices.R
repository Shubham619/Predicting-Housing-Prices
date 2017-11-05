View(homdata)
library(MASS)
library(ggplot2)
library(GGally)
library(caret)
library(randomForest)
library(readr)
library(magrittr)
library(dplyr)
library(sqldf)
library(Metrics)

home <- homdata[,-c(1,2)]

set.seed(230)
intrain<- createDataPartition(p=0.75,y=home$price,list=FALSE)
train <- home[intrain,]
test <- home[-intrain,]



p<-ggplot(train,aes(factor(waterfront),price))+
  geom_boxplot(colour="blue",fill="yellow",outlier.colour = "red",outlier.shape = 1)+
  ggtitle("Waterfront vs Price")

ggplot(train,aes(factor(condition),price))+
  geom_boxplot(fill="yellow",outlier.colour = "red",outlier.shape = 1,colour="blue")+
  ggtitle("Condition vs Price")


ggplot(train,aes(factor(view),price))+
  geom_boxplot(fill="yellow",outlier.colour = "red",outlier.shape = 1,colour="blue")+
  ggtitle("View vs Price")

Size<-train %>% select(contains("sqft"))
ggpairs(Size,lower = list(continuous = "smooth",method='lm'))


zipcode<-sqldf('SELECT zipcode, 
               avg(price) as AveragePrice from train group by zipcode')


ge<-ggplot(zipcode,aes(x=zipcode,y=AveragePrice))+
  geom_point()+
  geom_line(stat="identity")
ge


cor1<-train[,c(1:6,10:19)]
CorrelationMatrix <-cor(cor1)
print(CorrelationMatrix)


model <- train(
  price ~., train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number =10,repeats = 5,
    verboseIter = TRUE
  )
)

print(model)

summary(model)

pred <- predict(model,test[,-1])
acc_lm <- rmse(test$price,pred)


plot(pred)

model <- train(
  price~.,
  tuneLength =3,
  data = train, method ="ranger",
  trControl = trainControl(method = "cv", number = 5,
                           verboseIter = TRUE)
)

plot(model)

model$finalModel

model$results
pred<-predict(model,test[,-1])
acc_rf <-rmse(test$price,pred)



Accu<-data.frame(Model=c('Best Regression','Random Forest'),
                 RMSE=c(acc_lm,acc_rf))
ggplot(Accu,aes(x=Model,y=RMSE))+
  geom_bar(stat='identity')+
  ggtitle('Accuracy Comparison')+
  theme_bw()


