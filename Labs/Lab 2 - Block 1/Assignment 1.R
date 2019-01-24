library(ggplot2)
data<- read.csv2("australian-crabs.csv" ,sep = ",",dec=".")
p <- ggplot(data, aes(x=CL, y=RW)) + geom_point(aes(color=sex), size=2 ) +
  scale_color_manual (values = c('blue', 'red')) +
  labs(x="CL carspace length", y="RW rear Width", colour="Classes") +
  ggtitle("original data")
 
X<- data.frame(RW=data$RW , CL=data$CL )
Y <- data$sex
#1.2
library(MASS)
disc_fun=function(label, S)
{
  X1 = X[Y==label,]
  mean_v <- c(mean(X1$RW) ,mean(X1$CL))
  covaiance_mat_inverse <- solve(S)
  prior_prob <- nrow(X1) / nrow(X)
  w1 <- covaiance_mat_inverse %*% mean_v
  b1 <- ((-1/2) %*% t(mean_v) %*% covaiance_mat_inverse %*% mean_v) + log(prior_prob)
  w1<- as.vector(w1)
  return(c(w1[1], w1[2], b1[1,1]))
}
X1=X[Y=="Male",]
X2=X[Y=="Female",]
S=cov(X1)*dim(X1)[1]+cov(X2)*dim(X2)[1]
S=S/dim(X)[1]
#discriminant function coefficients
res1=disc_fun("Male",S)
res2=disc_fun("Female",S)
#1.2
#decision boundary coefficients 'res'
res <- c( -(res1[1]-res2[1]) , (res2[2]-res1[2]), (res2[3]-res1[3]))
# classification
d=res[1]*X[,1]+res[2]*X[,2]+res[3]
Yfit=(d>0)
plot(X[,1], X[,2], col=Yfit+1, xlab="CL", ylab="RW")
#slope and intercept
slope <- (res[2] / res[1] ) * -1
intercept <- res[3] /res[1] * -1
#1.3
#plot decision boundary
X<- cbind(X,sex=Y)
p <- ggplot(X, aes(x=CL, y=RW)) + geom_point(aes(color=sex), size=2 ) +
  scale_color_manual (values = c('blue', 'red')) +
  labs(x="CL carspace length", y="RW rear Width", colour="Classes") +
  geom_abline(slope = slope, intercept = intercept) +
  ggtitle("Descion Boundary LDA")
#1.4
glm1 <- glm(sex ~ CL + RW,family=binomial(link="logit"), data=data)
slope1 <- -(glm1$coefficients[2] / glm1$coefficients[3] )
intercept1 <- -(glm1$coefficients[1] /glm1$coefficients[3] )
print(qplot(
  x =data$CL,
  y = data$RW,
  data = data,
  color = data$sex ,
  main="CL vs RW",
  xlab="Carapace Length", ylab = "Rear Width")
  +geom_abline(slope = slope1, intercept = intercept1,colour='purple')+ggtitle("CL Vs RW in Logistic Regression"))
cat("Decision boundary with linear regression:",slope1, "+",intercept1, "* k\n")
