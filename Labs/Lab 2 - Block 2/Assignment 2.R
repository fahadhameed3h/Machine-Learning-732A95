# Assignment 2
data <- read.csv2("data.csv", header = TRUE, sep = ";", quote = "\"",
                  dec = ",", fill = TRUE, check.names = FALSE)
data1 <- as.data.frame(data)
data_email <- as.data.frame(data)
data_email$Conference <- as.factor(data$Conference)
n=dim(data_email)[1]
set.seed(12345)
# 70% Training Data
id=sample(1:n, floor(n*0.7))
train=data_email[id,]
# 30% validation & testing Data
test = data_email[-id,]
library(pamr)
rownames(train) <- 1:nrow(train)
which(colnames(train)=="Conference")
x <- t(train[,-4703])
y <- train[[4703]]
test_x <- t(test[,-4703])
mydata <- list(x=x, y= as.factor(y),
               geneid=as.character(1:nrow(x)),genenames = rownames(x))
model_train <- pamr.train(mydata)
cv_model <- pamr.cv(model_train, data = mydata)
pamr.plotcv(cv_model)
print(cv_model)
model_fit <- pamr.train(mydata,threshold = cv_model$threshold[which.min(cv_model$error)])
par(mfrow=c(1,1),mar=c(2,2,2,2))
pamr.plotcen(model_train, mydata, threshold = model_fit$threshold)
features = pamr.listgenes(model_train,mydata, threshold = 1.306,genenames=TRUE)
cat( paste( colnames(train)[as.numeric(features[1:10,1])], collapse='\n' ) )
ypredict <- pamr.predict(model_train, newx = test_x,type = "class", threshold = 1.306)
conf_mat <- table(ypredict, test$Conference)
misclas_centroid <- 1 - (sum(diag(conf_mat))/sum(conf_mat))

## Part 2
library(glmnet)
set.seed(12345)
response <- train$Conference
predictors <- as.matrix(train[,-4703])
elastic_model <- glmnet(x=predictors,y=response,family = "binomial",alpha = 0.5)
cv.fit <- cv.glmnet(x=predictors,y=response,family="binomial",alpha = 0.5)
cv.fit$lambda.min
par(mar=c(2,2,2,2))
plot(cv.fit)
plot(elastic_model)
predictor_test <- as.matrix(test[,-4703])
ypredict <- predict(object = elastic_model,newx = predictor_test, s = cv.fit$lambda.min,
                    type = "class", exact = TRUE)
confusion_mat <- table(ypredict,test$Conference)
misclassification <- 1 - (sum(diag(confusion_mat))/sum(confusion_mat))

library(kernlab)
x <- as.matrix(train[,-4703])
y <- train[,4703]
svm_fit <- ksvm(data = train,Conference ~ . ,kernel="vanilladot",
                scaled = FALSE)
ypred <- predict(svm_fit, newdata = test, type="response")
confusion_mat <- table(ypred,test$Conference)
misclas_svm <- 1 - sum (diag(confusion_mat))/sum(confusion_mat)



benjamini_hochberg <- function(x, y, alpha) {
  pvalues <- apply(x, 2, function(feature) {
    t.test(feature ~ y, alternative="two.sided")$p.value
  })
  m <- length(pvalues)
  
  sorted <- sort(pvalues)
  values <- 1:m * alpha / m
  
  L <- which.min(sorted <= values) - 1
  mask <- sorted <= sorted[L]
  list(mask=mask, pvalues=sorted, features=colnames(x)[order(pvalues)][mask])
}

result <- benjamini_hochberg(x=data[,-ncol(data)], y=data[, ncol(data)], alpha=0.05)
rejected <- length(result$features)

cat("Top 10 features")
cat(paste(result$features[1:10], collapse='\n' ) )

ggplot() +
  ylab("P-Value") + xlab("Index") +
  geom_point(data=data.frame(x=1:length(result$features),
                             y=result$pvalues[result$mask]),
             aes(x=x, y=y), col="red") +
  geom_point(data=data.frame(x=((length(result$features) + 1):(ncol(data) -1)),
                             y=result$pvalues[!result$mask]),
             aes(x=x, y=y), col="blue")

ggplot() +
  ylab("P-Value") + xlab("Index") +
  geom_point(data=data.frame(x=1:length(result$features),
                             y=result$pvalues[result$mask]),
             aes(x=x, y=y), col="red") +
  geom_point(data=data.frame(x=((length(result$features) + 1):150),
                             y=result$pvalues[!result$mask][1:(150 - rejected)]),
             aes(x=x, y=y), col="blue")
