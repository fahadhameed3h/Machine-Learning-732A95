library(neuralnet)
library("grDevices")

set.seed(1234567890)
Var <- runif(50, 0, 10)#sample 50 points uniformly at random in intervals 0,10 
trva <- data.frame(Var, Sin=sin(Var)) # apply Sin function to each point
tr <- trva[1:25,] # Training 1 to 25
va <- trva[26:50,] # Validation 26 to 50

# Random initializaiton of the weights in the interval [-1, 1]
results = rep(0,10) # numeric class 10 elements with 0 values
winit <- runif(250,-1,1) # 250 points between -1 and 1

for(i in 1:10) {
    nn <- neuralnet(formula = Sin ~ Var, data=tr, hidden = 10,
                    threshold = i/1000 ,startweights = winit)
  result = compute(nn, va$Var)$net.result 
  results[i] = mean((result - va$Sin)^2)
}
best = which.min(results) # Most appropiate value of threshold select is i = 4
nn <- neuralnet(formula = Sin ~ Var , data=trva, hidden = 10, threshold = best/1000, startweights = winit)
plot(nn) 
plot(prediction(nn)$rep1, col="Black")  # predictions (black dots)
points(trva, col = "red") # data (red dots)