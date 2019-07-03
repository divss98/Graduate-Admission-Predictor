library(tidyverse)
Data<-read.csv("Admission_Predict_1.csv")
str(Data)
head(Data)

#check for missing values in dataset
sapply(Data, function(x) sum(is.na(x)))

summary(Data)

# visualization
#1.correlation plot, install corrplot
# need to remove serial number attribute

library(corrplot)
Corr <- cor(Data)
Corr <- Corr[,-1]
correlation<-corrplot(cor(Corr), method = "color", addCoef.col = "black", 
         tl.col="black", tl.cex=1, number.cex = 1)

#2. outlier detection of Gre and TOEFL Scores

library(ggplot2)
ggplot(Data, aes(x=factor(GRE.Score), Chance.of.Admit)) + geom_boxplot(fill = "red")+
  scale_y_continuous("Chance.of.Admit")+
  labs(title = "Outliers", x = "GRE Scores")

ggplot(Data, aes(x=factor(TOEFL.Score), Chance.of.Admit)) + geom_boxplot(fill = "red")+
  scale_y_continuous("Chance.of.Admit")+
  labs(title = "Outliers", x = "TOEFL Scores")

# 3. to check for relations, linearity,trends
library(ggplot2)

p1 <- ggplot(Data,aes(GRE.Score, Chance.of.Admit, group = 1)) +
  geom_boxplot(color = "red") + 
  geom_jitter(color = "blue") + 
  geom_smooth(method = "loess",color="black") 

p2 <- ggplot(Data,aes(TOEFL.Score, Chance.of.Admit, group = 1)) +
  geom_boxplot(color = "red") + 
  geom_jitter(color = "blue") + 
  geom_smooth(method = "loess", color="black") 

p3 <- ggplot(Data,aes(University.Rating, Chance.of.Admit, group = 1)) +
  geom_boxplot(color = "red") + 
  geom_jitter(color = "blue") + 
  geom_smooth(method = "loess", color="black") 

p4 <- ggplot(Data,aes(SOP, Chance.of.Admit, group = 1)) +
  geom_boxplot(color = "red") + 
  geom_jitter(color = "blue") + 
  geom_smooth(method = "loess", color="black") 

p5 <- ggplot(Data,aes(LOR, Chance.of.Admit, group = 1)) +
  geom_boxplot(color = "red") + 
  geom_jitter(color = "blue") + 
  geom_smooth(method = "loess", color="black") 

p6 <- ggplot(Data,aes(CGPA, Chance.of.Admit, group = 1)) +
  geom_boxplot(color = "red") + 
  geom_jitter(color = "blue") + 
  geom_smooth(method = "loess", color="black") 

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)


# 4.cluster analysis

data1<-select(Data, GRE.Score,TOEFL.Score, CGPA)
#elbow curve
wss=numeric(15L)
for (i in 1:15)
  wss[i]<-(sum(kmeans(data1,centers=i,nstart=25)$withinss))
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares",main="Assessing the Optimal Number of Clusters with the Elbow Method",pch=10 , cex=1)

#optimum k value=3
library(animation)
clusters<-kmeans.ani(data1,3)
clusters

# Prediction

set.seed(200)
#create training and testing set
splitData <- sample(nrow(Data), 0.7*nrow(Data), replace = FALSE)
TrainSet <- New_data[splitData,]
TestSet <- New_data[-splitData,]
str(TrainSet)
str(TestSet)

#Linear Regression
library(modelr)
LinearReg <- lm(Chance.of.Admit ~ ., data = TrainSet)
summary(LinearReg)

#(Intercept) corresponds to the estimated response variable when all the input variables equal zero.
#This coefficient is interpreted as follows: For every one unit increase in a person’s age, 
#the person’s income is expected to increase by the value in table
#R-squared (R2) is a commonly reported metric that measures the variation in the
#data that is explained by the regression model. Possible values of R2 vary from 0 to 1, with values closer
#to 1 indicating that the model is better at explaining the data than values closer to 0.
plot(LinearReg)
mae(LinearReg, data = New_data)

#Decision Tree
library(rpart)
library(rpart.plot)
DecisionTree <- rpart(Chance.of.Admit ~ GRE.Score+TOEFL.Score+LOR+SOP+University.Rating, data = TrainSet)
rpart.plot(DecisionTree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
mae(model=DecisionTree, New_data)

#we choose Linear regression because of lower MAE
library(dplyr)
library(forecast)
pred <- predict(LinearReg,newdata=TestSet)
accuracy(pred, TestSet$Chance.of.Admit)
result <- data.frame(TestSet$Chance.of.Admit, pred)
sample_n(result, 10)
cor(result)

#testing with values
GRE.Score<-337
TOEFL.Score<-118
University.Rating<-4
SOP<-4.5
LOR<-4.5
CGPA<-9.65
Research<-1
test_1<-data.frame(GRE.Score,TOEFL.Score,University.Rating,SOP,LOR,CGPA,Research)
pred1<-predict(LinearReg,newdata = test_1)
pred1
