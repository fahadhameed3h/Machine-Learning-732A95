library(ggplot2)
library(fastICA)
library(pls)
library(reshape2)

data <- read.csv2("NIRSpectra.csv")

X <- scale(data[, -ncol(data)]) # removing column 127 viscosity
y <- data[, ncol(data)]
 
pca <- prcomp(X) # can also do here scaling using scale=TRUE
 
lambda <- pca$sdev^2 # Eigenvalues
variances <- (lambda / sum(lambda))* 100 # proportion of variation

var99_comp_count <- which.max(cumsum(variances) > 99) # which show variance greater then 99%
components <- as.data.frame(pca$x[, 1:var99_comp_count]) # extracting first two components that show variance greater then 99 %
 
pc_comps <- 1:10
plot_data <- data.frame(x=pc_comps, Variance=variances[pc_comps])

ggplot(plot_data, aes(x=x, y=Variance)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=pc_comps, labels=as.numeric(pc_comps)) +
  xlab("Principal Component")
  
ggplot(components) +
  geom_point(aes(x=PC1, y=PC2)) +
  scale_x_continuous(breaks=pretty(components$PC1, n=6)) +
  scale_y_continuous(breaks=pretty(components$PC2, n=6))

U<-pca$rotation#Loadings are the coordinates of the principal components in the original vector space

r1 <- pca$rotation[,1] ; r2 <- pca$rotation[,2]
ggplot() + geom_point(aes(x = 1:length(r1), y = r1, col = "PC1")) +
           geom_point(aes(x = 1:length(r2), y = r2, col = "PC2")) +
           labs(title = "Plot of loadings", x = "Index", y = "Loadings")

ica <- fastICA(X, var99_comp_count, alg.typ="parallel", fun="logcosh", alpha=1,
               method="R", row.norm=FALSE, maxit=200, tol=1e-06, verbose=FALSE)

Wp <- ica$K %*% ica$W # W`=K*W summary(ica)
d <- dim(Wp)[1]
Wp1 <- Wp[,1]
Wp2 <- Wp[,2]
ggplot() + geom_point(aes(x = 1:d, Wp1, col = "Wp1")) + geom_point(aes(x = 1:d, Wp2, col = "Wp2")) +
           labs(title = "Plot of columns of W'", x = "Index", y = "W'")
  
score1 <- ica$S[,1]
score2 <- ica$S[,2]
d1 <- dim(score1)[1]
ggplot() + geom_point(aes(x = score1, y = score2)) + 
labs(title = "Plot of scores of principal components"
, x = "Index" , y = "W'")
 
predictors <- paste("X", seq(750, 1000, 2), sep="")
f <- formula(paste("Viscosity ~ ", paste(predictors , collapse = " + ")))

set.seed(12345)
pcr.fit <- pcr(f, data = data, validation = "CV")
validationplot(pcr.fit,val.type = "MSEP")