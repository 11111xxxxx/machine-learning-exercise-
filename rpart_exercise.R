library(iris)
library(rpart)
library(rpart.plot)
iris

# exercise 1
# get a sample from 150 numbers, we take 50 numbers 
set.seed(10)
s<- sample(150,100)


#split the iris data set into training and test data set 
 iris_train <- iris[s,]
 iris_test <- iris[-s,]

 
 # create a decision tree model 
dtm <- rpart(Species ~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, iris_train, method = "class" )

# visulize the decision tree results 
rpart.plot(dtm, type = 4, extra = 101)

# utilize the tree, do some predictions 
p <- predict(dtm, iris_test, type = "class")
table(iris_test[,5],p)


### exercise 2  https://www.youtube.com/watch?v=XLNsl1Da5MA
# shuffle the data first
set.seed(9810)
g <- runif(nrow(iris))
irisr <- iris[order(g),]

# learn the classification tree 
# [1:100,] means the first 100 rows after shuffled are defined as trainning data set 
m3 <- rpart(Species ~., data = irisr[1:100,], method = "class")

rpart.plot(m3)

summary(m3)

# predict
p3 <- predict(m3,irisr[101:150,],type="class")
table(irisr[101:150,5],rpart_predicted=p3)



#### exercise 3  https://www.youtube.com/watch?v=MoBw5PiW56k
data()
data(msleep)
df <- msleep[,c(3,6,10,11)]
str(df)
head(df)


# build a regression tree that predict sleep variable/ train a regression model 
 m1<-rpart(sleep_total~., data=df, method = "anova")
 m1
rpart.plot(m1,type = 3, digits=3, fallen.leaves = TRUE)

# because the data set is small, just pretend training data and test data are the same, but it's not good in reality 

p1 <-  predict(m1,data=df)
p1
summary(p1)

# get a sense of how the algorithms is performing, one way is to calculate the mean absolute error, compare error between models, choose the lowest one 
MAE <- function(actual, predicted) {mean(abs(actual-predicted))}
MAE(df$sleep_total, p1)


