data_liquor <-read.csv(file.choose(),header=T)
View(data_liquor)
summary(data_liquor)
  
library(ISLR)
library(MASS)
library(boot)
set.seed(1)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset =train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
View(Auto)

lm.fit2 <-lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)


lm.fit3 <-lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#K fold

?cv.glm
set.seed(17)
cv.error.10 =rep(0,10)
for (i in 1:10) {
  
  glm.fit =glm(mpg~poly(horsepower,i),data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit,K=10)$delta[1]
  
}
cv.error.10
