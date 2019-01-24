library(MASS)
library(readxl)
library(Matrix)
library(glmnet)
 
data = read_excel("tecator.xlsx")
plot(data$Protein,data$Moisture)
 
set.seed(12345)
n = nrow(data)
train_indexes = sample(1:n,floor(n*0.5))
train_data = data[train_indexes,]
test_data = data[-train_indexes,]
power = 6
train_error = matrix(0,power,1)
test_error = matrix(0,power,1)
for(i in 1:power)
{
  model = lm(Moisture ~ poly(Protein,i), data=train_data)
  train_predictions = predict(model,train_data)
  test_predictions = predict(model,test_data)
  train_error[i,] = mean((train_data$Moisture - train_predictions)^2) # MSE Formula below 
  test_error[i,] = mean((test_data$Moisture - test_predictions)^2)#mean((predicted-actual Data)^2)
}
ylim = c(min(rbind(train_error,test_error)),max(rbind(train_error,test_error)))#Limits of PLOT
plot(1:power,train_error, col="Green", ylim=ylim)
lines(1:power,train_error, col="Green")
points(1:power,test_error,col="Red")
lines(1:power,test_error, col="Red")

# High Bias -> High error on training set
# High Variance -> Fits training data well but high error in test set 
 
model = lm(Fat ~ ., data=data)
steps = stepAIC(model,direction="both", trace=FALSE)
coeff_aics = steps$coefficients
n_coeff_aics = length(coeff_aics)
print(n_coeff_aics)
 
rl_data <- data.matrix(data)
y <- rl_data[,102] # Select Fat as the only response...
X <- rl_data[,2:101] # Select Channel1-Channel-100 features.
ridge <- glmnet(X, y, alpha = 0) # Fit with the Ridge regression. Alpha=0
plot(ridge, xvar="lambda", label=TRUE)
lasso <- glmnet(X, y, alpha = 1) # Fit with the Lasso regression. Alpha=1
plot(lasso, xvar="lambda", label=TRUE)
 
kfoldcv <- cv.glmnet(X, y,  type.measure="mse", nfolds=20) #cross validation on lasso
feature_selection <- coef(kfoldcv, s = "lambda.min")
plot(kfoldcv)
 