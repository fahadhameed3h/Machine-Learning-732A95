spambase <- read.csv2("spambase.csv", header = TRUE, sep = ";", quote = "\"",
                      dec = ",", fill = TRUE)
spambase <- as.data.frame(spambase)
### Adaboost
n=dim(spambase)[1]
set.seed(12345)
id=sample(1:n, floor(n*2/3))
train=spambase[id,]
test=spambase[-id,]
number_of_trees <- seq(from = 10,to = 100, by = 10)
adaboost <- function(ntrees)
{
  fit <- blackboost(as.factor(Spam) ~., data = train,
                    control = boost_control(mstop = ntrees, nu=0.1),
                    family = AdaExp())
  # misclassification test
  ypredict <- predict(fit, newdata = test, type= "class")
  conf_mat <- table(ypredict,test$Spam)
  error_test <- 1-sum(diag(conf_mat))/sum(conf_mat)
}
error_rates_ada <- sapply(number_of_trees, adaboost)
plot(error_rates_ada,type = "b",main="Adaboost Misclassification", xlab= "Number of Trees", ylab= "Error",
     col="blue", pch=19, cex=1)
# Loss Function = exp(-y * f)
## random forest
random_forest <- function(ntrees)
{
  fit <- randomForest(as.factor(Spam) ~ ., data=train, importance=TRUE,
                      ntree = ntrees)
  # test misclassification
  ypredict <- predict(fit, test,type ="class")
  conf_mat <- table(ypredict,test$Spam)
  error_test <- 1-sum(diag(conf_mat))/sum(conf_mat)
}
error_rates_random <- sapply(number_of_trees, random_forest)
plot(error_rates_random,type = "b",main="Random Forest Misclassification", xlab= "Number of Trees", ylab= col="blue", pch=19, cex=1)
#comparsion random forest Vs adBoost
plot(y = error_rates_ada,x=number_of_trees, type = "l", col="red",
     main= "Performance Evaluation of Adaboost Vs Random Forest",
     xlab = "Number of Trees",ylab="Misclassification Rate", ylim = c(0,0.15))
points(y = error_rates_ada,x=number_of_trees,col="red", pch=19, cex=1)
lines(y = error_rates_random,x=number_of_trees, type= "l", col = "blue")
points(y = error_rates_random,x=number_of_trees,col="blue", pch=19, cex=1)
legend("topright",legend= c("adaboost","random forest"),
       col=c("red","blue"),lty=1,cex=0.8)







#######################################################################

library(mboost)
library(randomForest)
library(ggplot2)
library(reshape2)

data <- read.csv2("spambase.csv", header = TRUE, sep = ";", quote = "\"",
                 dec = ",", fill = TRUE)
data$Spam <- as.factor(data$Spam)

set.seed(1234567890)
train_idx <- sample(nrow(data), floor(nrow(data) * (2 / 3)))
train <- data[train_idx,]
test <- data[-train_idx,]
## ---- end-of-assign4a-init

## ---- assign4a-tree
tree_counts <- seq(10, 100, by=10)
test_errors <- rep(0, length(tree_counts))
train_errors <- rep(0, length(tree_counts))

for (i in 1:length(tree_counts)) {
  fit <- blackboost(Spam ~ ., data=train, family=AdaExp(),
                    control=boost_control(mstop=tree_counts[i]))
  test_error <- 1 - (sum(predict(fit, test, type="class") == test$Spam) / nrow(test))
  train_error <- 1 - (sum(predict(fit, train, type="class") == train$Spam) / nrow(train))
  test_errors[i] <- test_error
  train_errors[i] <- train_error
}
## ---- end-of-assign4a-tree

## ---- assign4a-tree-plot
plot_data <- data.frame(Trees=tree_counts, test=test_errors, train=train_errors)
plot_data <- melt(plot_data, id="Trees", value.name="Error", variable.name="Data")

ggplot(plot_data) +
  xlab("Number of classification Trees") +
  ylab("Misclassification Rate") +
  geom_line(aes(x=Trees, y=Error, color=Data)) +
  geom_point(aes(x=Trees, y=Error, color=Data), size=2) +
  scale_x_discrete(limits=tree_counts)
## ---- end-of-assign4a-tree-plot

## ---- assign4a-forest
test_errors <- rep(0, length(tree_counts))
train_errors <- rep(0, length(tree_counts))

for (i in 1:length(tree_counts)) {
  fit <- randomForest(Spam ~ ., data=train, ntree=tree_counts[i])
  test_error <- 1 - (sum(predict(fit, test, type="class") == test$Spam) / nrow(test))
  train_error <- 1 - (sum(predict(fit, train, type="class") == train$Spam) / nrow(train))
  test_errors[i] <- test_error
  train_errors[i] <- train_error
}
## ---- end-of-assign4a-forest

## ---- assign4a-forest-plot
plot_data <- data.frame(Trees=tree_counts, test=test_errors, train=train_errors)
plot_data <- melt(plot_data, id="Trees", value.name="Error", variable.name="Data")

ggplot(plot_data) +
  xlab("Number of classification Trees") +
  ylab("Misclassification Rate") +
  geom_line(aes(x=Trees, y=Error, color=Data)) +
  geom_point(aes(x=Trees, y=Error, color=Data), size=2) +
  scale_x_discrete(limits=tree_counts)
## ---- end-of-assign4a-forest-plot