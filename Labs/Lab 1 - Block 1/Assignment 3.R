mylin=function(X,Y, Xpred){
  Xpred1=cbind(1,Xpred)
  X= cbind(1,X)
  beta <- solve(t(X) %*% X) %*% (t(X) %*% Y) # Lecture 1d slide(7)
  Res=Xpred1 %*% beta
  return(Res)
}

myCV=function(X = as.matrix(swiss[,2:6]) , Y = swiss[[1]] , Nfolds = 5){
  n=length(Y)
  p=ncol(X)
  
  ind=sample(n,n)
  X1=X[ind,]
  Y1=Y[ind]
  sF=floor(n/Nfolds)
   
  MSE=numeric(2^p-1)
  Nfeat=numeric(2^p-1)
  
  Features=list()
  curr=0
  #we assume 5 features.
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1){
            model = c(f1,f2,f3,f4,f5)
            if (sum(model)==0) next()
            SSE=0
            seq_1 <- seq(from=1, to=n, by = sF)
            seq_2 <- seq(0, n, sF)
            ind_ <- which(model == 1)
            for (k in 1:Nfolds){
              i <- seq_1[k]
              j <- seq_2[k+1] 
              
              folds_ <- ind[i:j]
              Xtest <- X1[folds_,ind_]
              Xtrain <- X1[-folds_,ind_]
              Ytrain <- Y1[-folds_]
              Yp <- Y1[folds_]
              Ypred <- mylin(X = Xtrain,Y = Ytrain,Xpred = Xtest)
              SSE=SSE+sum((Ypred-Yp)^2)
            }
            curr=curr+1
            MSE[curr]=SSE/n
            Nfeat[curr]=sum(model)
            Features[[curr]]=model
          }
  plot(Nfeat,MSE, type = "p", xlab = "Number of features",
       ylab = "MSE", pch=19,cex=1)
  #MISSING: plot MSE against number of features
  i=which.min(MSE)
  return(list(CV=MSE[i], Features=Features[[i]]))
}
myCV(as.matrix(swiss[,2:6]), swiss[[1]], 5)
