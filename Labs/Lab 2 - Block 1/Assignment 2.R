## Assignment 2
library(readxl)
library(tree)
library(e1071)
# Importing Data - 2.1
data <- read_excel("creditscoring.xls")
data <- as.data.frame(data)
data$good_bad <- as.factor(data$good_bad)
# Dividing Data into three Train(50%) Test(25%) Validation(25%)
n = nrow(data)
set.seed(12345)
n=dim(data)[1]
# 50% Training Data
id=sample(1:n, floor(n*0.5))
train=data[id,]
# 25% validation & testing Data
Sub_id = data[-id,]
m = dim(Sub_id)[1]
part1 = sample(1:m, floor(m*0.5))
validation = Sub_id[part1,]
testing = Sub_id[-part1,]
# Step 2
# Fitting data using Deviance and gini
tree_deviance = tree(as.factor(good_bad) ~ ., data = train, split = "deviance")
tree_gini = tree(as.factor(good_bad) ~ ., data = train, split = "gini")
summary(tree_gini) # to find number of nodes
# Prediction
## Misclassification for training data
devi_yfit = predict(tree_deviance, newdata = testing,type="class")
gini_yfit = predict(tree_gini, newdata = testing,type="class")
plot(tree_deviance)
plot(tree_gini)
devi_table = table(devi_yfit,testing$good_bad)
gini_table = table(gini_yfit,testing$good_bad)
devi_table

# Missclassification rate Deviance
missclass_devi <- 1-sum(diag(devi_table))/sum(devi_table)
missclass_devi
gini_table
# Missclassification rate Gini
missclass_gini <- 1-sum(diag(gini_table))/sum(gini_table)
missclass_gini
## Misclssification for test data:
devi_yfit = predict(tree_deviance, newdata = testing,type="class")
gini_yfit = predict(tree_gini, newdata = testing,type="class")
plot(tree_deviance)
plot(tree_gini)
devi_table = table(devi_yfit,testing$good_bad)
gini_table = table(gini_yfit,testing$good_bad)
devi_table
# Missclassification rate Deviance
missclass_devi <- 1-sum(diag(devi_table))/sum(devi_table)
missclass_devi
gini_table
# Missclassification rate Gini
missclass_gini <- 1-sum(diag(gini_table))/sum(gini_table)
missclass_gini
### Step 3
index = summary(tree_deviance)[4]$size
trainScore = rep(0,index)
testScore = rep(0,index)
# Graph training and validation
for(i in 2:index) {
  prunedTree=prune.tree(tree_deviance,best=i)
  pred=predict(prunedTree, newdata=validation,type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}
plot(2:index,trainScore[2:index], col="Red",type = "b", main = "Dependence of Deviance",
     ylim=c(min(testScore[2:index]),max(trainScore)), pch=19, cex=1, ylab="Deviance")
points(2:index,testScore[2:index],col="Blue",type="b", pch=19, cex=1)
# misclassification rate for test data
missclass_test_t = prune.tree(tree_deviance, best = 4)
summary(missclass_test_t)
 
yfit = predict(missclass_test_t, newdata = testing, type="class")
valid_ = table(testing$good_bad,yfit)
print("Confusion Matrix")
valid_
mc <- 1-sum(diag(valid_))/sum(valid_)
print("Misclassification rate")
mc
plot(missclass_test_t)
text(missclass_test_t)
### Step 4
# Naive Bayes 2.4
naye = naiveBayes(good_bad ~., data=train)
nav_test = predict(naye, newdata = testing[,-ncol(testing)], type = "class") # -ncol(testing) column
nav_train = predict(naye,newdata = train[,-ncol(train)])  # Removing good-bad
# Confusion Matrix Using Naive Bayes
nv_tbl_test = table(testing$good_bad,nav_test)
print(nv_tbl_test)
nv_tbl_train <- table(train$good_bad,nav_train)
print(nv_tbl_train)
# Missclassification train data value Using Naive Bayes
mc_nav_train <- 1-sum(diag(nv_tbl_train))/sum(nv_tbl_train)
cat("Misclassification train data value Using Naive Bayes is:",mc_nav_train)
# Missclassification test data value Using Naive Bayes
mc_nav_test <- 1-sum(diag(nv_tbl_test))/sum(nv_tbl_test)
cat("Misclassification test data value Using Naive Bayes is:",mc_nav_test)
### Step 5
# Naive Bayes With loss matrix 2.5
naye = naiveBayes(good_bad ~ ., data = train)
# Predicting using Naive
nav_test = predict(naye, testing[,-ncol(testing)] , type="raw")
nav_train = predict(naye, train[,-ncol(train)] , type="raw")
# applying loss matrix if greater then 10 True else False
nav_test = (nav_test[, 2] / nav_test[, 1]) > 10 # check with loss Matrix of 0,1,10,0 values
nav_train = (nav_train[, 2] / nav_train[, 1]) > 10
# confusion matrix for train & test
naive_table = table(testing$good_bad,nav_test)
naive_table_train = table(train$good_bad,nav_train)
# missclasification for train & test
naive_table_train
1-sum(diag(naive_table_train))/sum(naive_table_train)

naive_table
1-sum(diag(naive_table))/sum(naive_table)

