library(readxl)
 sport_e2 <- read_excel("sport_e2.xlsx")
 View(sport_e2)
#Logistic Regression will be used

#Interdependence will be our target variable 
str(sport_e2)
sport_e2$sport <- as.factor(sport_e2$sport)
sport_e2$sex <- as.factor(sport_e2$sex)
#Converting char into Binary numbers 1 or 2. 
sport_e2$how_alone <- ifelse(sport_e2$how_alone=="True",1,0)
sport_e2$how_alone<-as.factor(sport_e2$how_alone)
sport_e2$how_small_groups<-ifelse(sport_e2$how_small_groups== "True",1,0)
sport_e2$how_small_groups<-as.factor(sport_e2$how_small_groups)
sport_e2$how_big_groups<-ifelse(sport_e2$how_big_groups=="True",1,0)
sport_e2$how_big_groups<-as.factor(sport_e2$how_big_groups)

#Partition Data - Train (80%) & Test(20%)
#80% percent will be taken into my rows and the rest will be th test data.
#Take a sample first
set.seed(1234)
#help(sample)
indexSet <-sample(2,nrow(sport_e2),replace = T, prob = c(0.8,0.2))
train <- sport_e2[indexSet==1,]
test <- sport_e2[indexSet==2,]
indexSet
#Logistic  regression model
help(glm)
mymodel <- glm(as.factor(interdependence) ~ index+importance+sport+age+education+frequency, data = train, family = 'binomial')
summary(mymodel)

#Improving model..
library("MASS")
mymodel2<-stepAIC(mymodel)
summary(mymodel2)
#Log(Odds)=Log(p/1-p))="Logit"
            #-438.854-2.138*(index)+ 40.120*(frequency)
head(train) 
#Logit
y=-438.854-2.138* 134 + 40.120* 5  
#Probability
1/(1+exp(-y))

train$interdependence
mymodel2$fitted.values

#Prediction
p1<-predict(mymodel2, train, type = 'response')
head(p1)
train$interdependence

#Confusion Matrix
install.packages("caret")
install.packages("e1071")

pred1<-ifelse(p1>0.5,1,0)
tab1 <- table(Predicted = pred1, Actual = train$interdependence)
tab1 
