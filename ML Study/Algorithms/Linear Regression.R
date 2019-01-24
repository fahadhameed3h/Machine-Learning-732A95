############## LINEAR REGRESSION ############

data(airquality)
names(airquality)

#[1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"

plot(Ozone~Solar.R,data=airquality)

#calculate mean ozone concentration (na´s removed)
mean.Ozone=mean(airquality$Ozone,na.rm=T)

abline(h=mean.Ozone)

#use lm to fit a regression line through these data:

model1=lm(Ozone~Solar.R,data=airquality)

model1
# Intecept = Value of Ozone Concentration at zero Solar.R the line cut of at y-axis 
# Distance of line cut from zero
# Solar.R = Slope --> delta Y / delta X

abline(model1,col="red")
plot(model1)

termplot(model1)
summary(model1)


############################################  MULTIPLE REGRESSION ###########################

data(airquality)
names(airquality)

#[1] -Ozone-   -Solar.R- -Wind-    -Temp-    -Month-   -Day-

# Produce plots for some explanatory variables
plot(Ozone~Solar.R,airquality)
plot(Ozone~Wind,airquality)

coplot(Ozone~Solar.R|Wind,panel=panel.smooth,airquality)

model2=lm(Ozone~Solar.R*Wind,airquality)
plot(model2)

summary(model2)
termplot(model2)

summary(airquality$Solar.R)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    7.0   115.8   205.0   185.9   258.8   334.0       7 

summary(airquality$Wind)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.700   7.400   9.700   9.958  11.500  20.700 

Solar1=mean(airquality$Solar.R,na.rm=T)
Solar2=100
Solar3=300

predict(model2,data.frame(Solar.R=100,Wind=10))

p1=predict(model2,data.frame(Solar.R=Solar1,Wind=1:20))
p2=predict(model2,data.frame(Solar.R=Solar2,Wind=1:20))
p3=predict(model2,data.frame(Solar.R=Solar3,Wind=1:20))

plot(Ozone~Wind,airquality)
lines(1:20,p1)
lines(1:20,p2)
lines(1:20,p3)