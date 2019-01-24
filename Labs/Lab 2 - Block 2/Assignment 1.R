library(readxl)
library(ggplot2)
library(reshape2)

data <- read_excel("influenza.xlsx")
ggplot(data)+geom_point(aes(x = Time, y = Mortality, color = "Mortality")) +
  geom_point(aes(x = Time, y = Influenza, color = "Influenza")) 
# From plot it can be observed that influenza cases have the peaks at the same 
# points where the morltality plot has peaks,
# it shows that when there is increasing rate of mortality, the influenza cases have increased 
# or it can be interpreted as increasing number of influenza cases have affected the mortality rate 
 
library(mgcv)
k_week = length(unique(data$Week)) #Number of unique weeks in data 
gm_model<-gam(Mortality~Year+s(Week,k=k_week),data=data,family="gaussian",method="GCV.Cp") #GCV.Cp-Generalized CV
cat("Intercept =" , coef(gm_model)["(Intercept)"], "\n") # is W0
cat("Year coefficent =" , coef(gm_model)["Year"])  # is W1

#Probilistic model: y = wo + w1x1 + w2x1^2 + e  (where wo=intercept,  w1= est value of 1st var, w2= est value of 2nd var)
#Probilistic model: $y = -680.589 + 1.233*x1 + s(Week) + {\epsilon}~N(0,{\sigma}^2)
pred <- predict(gm_model)
df_plot <- data.frame(time= data$Time, mortality = data$Mortality, pred = as.vector(pred))

p1 <- ggplot(df_plot, aes(x= time, y = mortality)) +
  geom_point(colour= "blue") +
  geom_line(aes(time,pred),colour = "red") + coord_cartesian(xlim = c(1995,2003))
p1
plot(gm_model) # Spline component
summary(gm_model) # to check significant values using p values

m1<-gam(data$Mortality~data$Year+s(data$Week,k=52,sp=0.1),data = data,family = "gaussian")
pred2<-predict(m1)
m2<-gam(data$Mortality~data$Year+s(data$Week,k=52,sp=100),data = data,family = "gaussian")
pred3<-predict(m2)

df_plo3<-data.frame(pred2=as.vector(pred2),pred3=as.vector(pred3),
                    mortality=data$Mortality,time=data$Time)
p3<-ggplot(data=df_plo3,aes(x=time))+
  geom_point(aes(y=mortality,colour="Actual Values"))+
  geom_line(aes(y=pred2,colour="Predicted Values with very low sp"))+
  geom_line(aes(y=pred3,colour="Predicted Values with very high sp"))+
  scale_colour_manual("Legend",
                      breaks = c("Predicted Values with very low sp",
                                 "Predicted Values with very high sp","Actual Values"),
                      values = c("red","blue","#99FF00"))+
  ggtitle("Actual Mortality vs Predicted with different values of sp")
p3

summary(m1); summary(m2)
# the very high annd very low values of penalty factor leads to underfitting of model, 
# as it is evident from the plot in which green line represents the predicted values when 
# penalty factor is too high while red line represents the mortality when penalty factor is too low  
# deviance and degrees of freedom decreases with increase in penalty factor

df_plot <- data.frame(time= data$Time, influenza = data$Influenza, residual = as.vector(gm_model$residuals))

p2 <- ggplot(df_plot, aes(x= time, y = influenza)) +
  geom_point(colour= "blue") +
  geom_line(aes(time,residual),colour = "red")
p2

# Yes, it is evident from the plot that the temporal pattern in residuals seems to be correlated 
# to the outbreak of influenza since the peaks in influenza occurs relative to the peaks in 
# residuals

w <- unique(data$Week)
y <- unique(data$Year)
i <- unique(data$Influenza)

fit_f <- gam(data$Mortality ~ s(data$Year,k=length(y)) + s(data$Week,k=length(w)) 
         +s(data$Influenza,k=length(i)),data=data,family="gaussian",method="GCV.Cp")
pred_f <- predict(fit_f)

par(mfrow=c(2,2))
plot(fit_f)

# It can be illustrated from the plots of spline components that mortality does not depend much on
# year and have little change annually that is with weeks, but the mortality shows a significant 
# relation with influenza that is with increasing cases of influenza, mortality increases
par(mfrow=c(1,1))
df_plot <- data.frame(time= data$Time, mortality = data$Mortality, predicted = as.vector(pred_f))

p3 <- ggplot(df_plot, aes(x= time, y = mortality)) +
  geom_point(colour= "blue") +
  geom_line(aes(time,predicted),colour = "red")
p3
# The plot of original and fitted values implies that this model is better than the previous 
# models as it gives the predicted values closest to the original values. This also indicates that 
# including influenza in modelling has a significant impact on fitting.