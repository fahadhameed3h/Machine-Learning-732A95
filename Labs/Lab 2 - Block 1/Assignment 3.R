# Assignment 3
library(tree)
# 3.1 Data import, reorder and Plot
set.seed(12345)
data = read.csv2("State.csv", header = TRUE)
data = data[order(data$MET),] # reordering data with increase of MET variable
plot(EX ~ MET, data = data, pch = 19, cex = 1,col="blue")
# 3.2
set.seed(12345)
control_parameter = tree.control(nobs = nrow(data),minsize = 8)
fit_tree = tree(formula = EX ~ MET,data = data,control = control_parameter)
leave_fit = cv.tree(fit_tree)
plot(leave_fit$size, leave_fit$dev, main = "Deviance Vs Size of Tree" ,
type="b",col="red", pch= 19,cex=1)

op_tree = prune.tree(fit_tree,best = leave_fit$size[which.min(leave_fit$dev)])

plot(op_tree)
text(op_tree, pretty=1, cex = 0.8, xpd = TRUE)

fitted_val = predict(op_tree, newdata=data)

plot(data$MET, data$EX)
points(data$MET, fitted_val, col="red")

hist(residuals(op_tree))
# 3.3 Non-Paramatric Bootstrap
library(boot)
f_np = function(data,index){
  sample = data[index,]
  Ctrl = tree.control(nrow(sample), minsize = 8)
  fit = tree( EX ~ MET, data=sample, control = Ctrl)
  optimal_tree = prune.tree(fit, best= leave_fit$size[which.min(leave_fit$dev)])
  return(predict(optimal_tree, newdata=data))
}
np_bs = boot(data, statistic = f_np, R=1000)
conf_bound = envelope(np_bs,level=0.95) # For 95% Confidence interval

predictions = predict(op_tree,data)
plot(np_bs)
fig_data = data.frame(orig = data$EX, x=data$MET, pred=predictions,
                      upper=conf_bound$point[1,], lower=conf_bound$point[2,])
fig = ggplot(fig_data, aes(x,predictions,upper,lower))
p = fig + geom_point(aes(x, pred)) +
  geom_point(aes(x, orig),colour="blue") +
  geom_line(aes(x,upper),colour="red") +
  geom_line(aes(x,lower),colour="red")
p
# 3.4 Paramatric Bootstrap
set.seed(12345)
parama_conf = function(data){
  controll = tree.control(nrow(data), minsize = 8)
  fit = tree( EX ~ MET, data=data, control = controll)
  op_tree = prune.tree(fit, best=leave_fit$size[which.min(leave_fit$dev)])
  return(predict(op_tree, newdata=data))
}
param_predict = function(data){
  controll = tree.control(nrow(data), minsize = 8)
  fit = tree( EX ~ MET, data=data, control = controll)
  op_tree = prune.tree(fit, best=leave_fit$size[which.min(leave_fit$dev)])
  predictions = predict(op_tree, newdata=data)
  return(dbinom(nrow(data),predictions,sd(resid(fit))))
}
rnd = function(data, model){
  sample = data.frame(MET=data$MET, EX=data$EX)
  sample$EX = rnorm(nrow(data), predict(model,newdata=data),sd(resid(model)))
  return(sample)
}
set.seed(12345)
param_boot_conf = boot(data, statistic = parama_conf, R=1000, mle = op_tree,
                       ran.gen = rnd, sim = "parametric")
confidence_bound_param = envelope(param_boot_conf, level=0.95)
param_boot_predict = boot(data, statistic = param_predict, R=1000, mle = op_tree,
                          ran.gen = rnd,sim = "parametric")
prediction_bound_param = envelope(param_boot_predict, level=0.95)
plot(param_boot_conf)
plot(param_boot_predict)
predictions = predict(op_tree,data)
fig_data = data.frame(orig = data$EX, x=data$MET, pred=predictions,
                          upper_c=confidence_bound_param$point[1,],
                          lower_c=confidence_bound_param$point[2,],
                          upper_p=prediction_bound_param$point[1,],
                          lower_p=prediction_bound_param$point[2,])
para_plot = ggplot(fig_data, aes(orig,x,pred,upper_c,lower_c, upper_p, lower_p))
para_plot = para_plot + geom_point(aes(x, pred)) + geom_point(aes(x, orig),colour="blue") +
              geom_line(aes(x,upper_c),colour="red")+geom_line(aes(x,lower_c),colour="red")+
              geom_line(aes(x,upper_p),colour="green")+geom_line(aes(x,lower_p),colour="green")
para_plot

