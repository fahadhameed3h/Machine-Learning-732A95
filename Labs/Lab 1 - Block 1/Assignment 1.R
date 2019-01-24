library(ggplot2)
library(kknn)
library(readxl)
data <- read_excel("spambase.xlsx")
data <- as.data.frame(data) # convert into data frame
n=dim(data)[1]  # n = 2740  Length of data
set.seed(12345)
id=sample(1:n, floor(n*0.5))  # Divide data into 50% 2740/2 = 1370 Observations
train=data[id,] #train is data 
test=data[-id,] #test data is newData
knearest=function(data,k,newdata) {
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  X = as.matrix(data[,-p])
  Y = as.matrix(newdata[-p]) # change xn to Yn
  X_hat = X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  Y_hat = Y/matrix(sqrt(rowSums(Y^2)), nrow = n2 , ncol = p - 1)
  C <- X_hat %*% t(Y_hat)
  D <- 1 - C #distacne matrix calculate
  for (i in 1:n2 ) {
    Ni <- order(D[i,])  # order will return the index after sorting from smaller to larger
    N_i <- data[Ni[1:k],"Spam"]  # get k values
    Prob[i] <- sum(N_i) / k   }
  return(Prob) #return proabilities
}
probalities <- knearest(train,5, test)
probalities <- ifelse(probalities > 0.5, 1,0) # if probability is > 0.5 set to 1 else to 0 
conf_mat <- table(spam = test[,ncol(data)] , predicted_val = probalities) # Confusion Matrix
mc <- 1-sum(diag(conf_mat))/sum(conf_mat) # Misclassification Rate

ROC <- function(Y, Yfit, p) {
  m=length(p) # length of p
  TPR=numeric(m) # will create a vector of length p with values 0
  FPR=numeric(m)
  for(i in 1:m)  {
    t <- table(Y,Yfit>p[i])  # misclassification rate table
    TPR[i] <-  t[2,2]/sum(t[2,]) #True +ve Value / Sum of Positive Actual Values 
    FPR[i] <-  t[1,2]/sum(t[1,]) #False +ve Value / Sum of Negative Actual Values 
  }
  return (list(TPR=TPR,FPR=FPR))
}

pi_values <- seq(from = 0.05, to= 0.95 , by=0.05)
Y <- train[,ncol(data)]
knearest_p <- knearest(train, 5 , test) #knearst k = 5
roc_curve_knearest <- ROC(Y, knearest_p , pi_values)
X<-  as.data.frame(roc_curve_knearest)
ggplot() + geom_line(data = X, aes(x = X$FPR, y = X$TPR), color = "red") +
  ggtitle("ROC curve Knearnest()")+xlab("FPR") +ylab("TPR")
sensitivity_kn <- 1 - roc_curve_knearest$FPR