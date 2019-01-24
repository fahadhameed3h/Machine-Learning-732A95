## Question 2
mixture_model <- function(my_k=2)
{
  set.seed(1234567890)
  max_it <- 100 # max number of EM iterations
  min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
  N=1000 # number of training points
  D=10 # number of dimensions
  x <- matrix(nrow=N, ncol=D) # training data
  true_pi <- vector(length = 3) # true mixing coefficients
  true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
  true_pi=c(1/3, 1/3, 1/3)
  true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
  true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
  true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
  plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
  points(true_mu[2,], type="o", col="red")
  points(true_mu[3,], type="o", col="green")
  # Producing the training data
  for(n in 1:N) {
    k <- sample(1:3,1,prob=true_pi)
    for(d in 1:D) {
      x[n,d] <- rbinom(1,1,true_mu[k,d])
    }}
  K=my_k # number of guessed components
  z <- matrix(nrow=N, ncol=K) # fractional component assignments
  pi <- vector(length = K) # mixing coefficients
  mu <- matrix(nrow=K, ncol=D) # conditional distributions
  llik <- vector(length = max_it) # log likelihood of the EM iterations
  # Random initialization of the paramters
  pi <- runif(K,0.49,0.51)
  pi <- pi / sum(pi)
  for(j in 1:my_k) {
    mu[j,] <- runif(D,0.49,0.51)
  }
  pi
  mu
  for(it in 1:max_it)
  {
    if(K == 2)
    {
      plot(mu[1,], type="o", col="blue", ylim=c(0,1))
      points(mu[2,], type="o", col="red")
    }
    else if(K==3)
    {
      plot(mu[1,], type="o", col="blue", ylim=c(0,1))
      points(mu[2,], type="o", col="red")
      points(mu[3,], type="o", col="green")
    }
    
    else
    {
      plot(mu[1,], type="o", col="blue", ylim=c(0,1))
      points(mu[2,], type="o", col="red")
      points(mu[3,], type="o", col="green")
      points(mu[4,], type="o", col="yellow")
    }
    Sys.sleep(0.5)
    # E-step: Computation of the fractional component assignment
    # Bernoulli distribution
    for (n in 1:N)
    {
      prob_x=0
      for (k in 1:K)
      { #Multivariate Bernouilli distributions
        prob_x = prob_x+prod( ((mu[k,]^x[n,])*((1-mu[k,])^(1-x[n,]))) )*pi[k] #Documentation Eq 1 & 2
      }
      for (k in 1:K)
      {
        z[n,k] = pi[k]*prod( ((mu[k,]^x[n,])*((1-mu[k,])^(1-x[n,]))) ) / prob_x  #Eq 4 E-Step 
      }
    }
    #Log likelihood computation.
    likelihood <-matrix(0,nrow =1000,ncol = K)
    llik[it] <-0
    for(n in 1:N)
    {
      for (k in 1:K)
      {
        likelihood[n,k] <- pi[k]*prod( ((mu[k,]^x[n,])*((1-mu[k,])^(1-x[n,]))))
      }
      llik[it]<- sum(log(rowSums(likelihood)))
    }
    cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
    flush.console()
    # Stop if the lok likelihood has not changed significantly
    if (it > 1)
    {
      if (llik[it]-llik[it-1] < min_change)
      {
        if(K == 2)
        {
          plot(mu[1,], type="o", col="blue", ylim=c(0,1))
          points(mu[2,], type="o", col="red")
        }
        else if(K==3)
        {
          plot(mu[1,], type="o", col="blue", ylim=c(0,1))
          points(mu[2,], type="o", col="red")
          points(mu[3,], type="o", col="green")
        }
        else
        {
          plot(mu[1,], type="o", col="blue", ylim=c(0,1))
          points(mu[2,], type="o", col="red")
          points(mu[3,], type="o", col="green")
          points(mu[4,], type="o", col="yellow")
        }
        break
      }
    }
    #M-step: ML parameter estimation from the data and fractional component assignments
    mu<- (t(z) %*% x) /colSums(z)
    # N - Total no. of observations
    pi <- colSums(z)/N
  }
  cat("value of updated pi is " , pi )
  cat("\n")
  sprintf("value of updated mu is")
  print(mu)
  plot(llik[1:it], type="o")
}
mixture_model(2)
mixture_model(3)
mixture_model(4)
