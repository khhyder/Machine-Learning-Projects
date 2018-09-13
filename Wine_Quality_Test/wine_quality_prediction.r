
#Load Reqruired Libraries
library(lattice)
library(caTools)
library(corrplot)

#Read data from the disk
red_wine_df <- read.csv("C:\\Users\\Amara\\Desktop\\data_set\\winequality-red.csv")
white_wine_df <- read.csv("C:\\Users\\Amara\\Desktop\\data_set\\winequality-white.csv")

#Split dataset for training and testing
set.seed(2)
split <- sample.split(red_wine_df$quality, SplitRatio = 0.65) 
tr_red <- subset(red_wine_df,split=="TRUE")
ts_red <- subset(red_wine_df, split=="FALSE")
#for whitewine dataset
set.seed(2)
split2 <- sample.split(white_wine_df$quality, SplitRatio = 0.65)
tr_white <- subset(white_wine_df,split=="TRUE")
tst_white <- subset(white_wine_df, split=="FALSE")

#Create scatterplot to see the trend
splom(~red_wine_df[c(1:6,12)], groups=NULL,data=red_wine_df,axis.line.tck=0,axis.text.alpha=0)
splom(~red_wine_df[c(7:12)], groups=NULL,data=red_wine_df,axis.line.tck=0,axis.text.alpha=0)
splom(~white_wine_df[c(1:6,12)], groups=NULL,data=white_wine_df,axis.line.tck=0,axis.text.alpha=0)
splom(~white_wine_df[c(7:12)], groups=NULL,data=white_wine_df,axis.line.tck=0,axis.text.alpha=0)

###### Variance Study#####

#Variance study for red wine
var_red <- var(red_wine_df)
barplot(var_red)
boxplot(var_red)
boxplot(red_wine_data)
summary(red_wine_data)
var_red

#Variance study for White wine
var_white <- var(white_wine_df)
barplot(var_white)
boxplot(var_white)
var_white

#### Calcualte and visualise COrrelation Matrix#####

cor_red <- cor(red_wine_df)
cor_white <- cor(white_wine_df)
corrplot(cor_red,type = "lower")
corrplot(cor_white,type = "lower")

#Study alcohol and wine from red_wine data
plot(red_wine_df$alcohol,red_wine_df$quality)
abline(lm(red_wine_df$quality~red_wine_df$alcohol),col="red")

#Implement multivariate linear regression
all_fet_red <- lm(red_wine_df$quality ~ red_wine_df$fixed.acidity + red_wine_df$volatile.acidity + red_wine_df$citric.acid + red_wine_df$residual.sugar + red_wine_df$chlorides + red_wine_df$density + red_wine_df$alcohol + red_wine_df$total.sulfur.dioxide + red_wine_df$pH +red_wine_df$sulphates + red_wine_df$free.sulfur.dioxide)
summary(all_fet_red)
red_mod_trn <- lm(quality~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + total.sulfur.dioxide + density + pH + sulphates + alcohol, data=tr_red)
summary(red_mod_trn)
all_fet_white <-lm(white_wine_df$quality ~ white_wine_df$fixed.acidity + white_wine_df$volatile.acidity + white_wine_df$citric.acid + white_wine_df$residual.sugar + white_wine_df$chlorides + white_wine_df$density + white_wine_df$alcohol + white_wine_df$total.sulfur.dioxide + white_wine_df$pH + white_wine_df$sulphates + white_wine_df$free.sulfur.dioxide)
summary(all_fet_white)
white_mod_tr <- lm(quality~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + total.sulfur.dioxide + density + pH + sulphates + alcohol, data=tr_white)
summary(white_mod_tr)

#Remove variable with lowest p values.
red_tr_revised <- lm(quality~ volatile.acidity + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol, data=tr_red)
summary(red_tr_revised)
white_tr_revised <- lm(quality~ fixed.acidity + volatile.acidity + residual.sugar + total.sulfur.dioxide + pH + sulphates + alcohol, data=tr_white)
summary(white_tr_revised)

#Model prediction for both red and white wine test data
predic_red <- predict(red_mod_trn,ts_red)
predic_red
predic_white <- predict(white_mod_tr,tst_white)
predic_white

#Visualize prediction over test data
plot(ts_red$quality, type = "l", lty=1.8, col="yellow")
lines(predic_red, type="l", col="red")
plot(tst_white$quality, type = "l", lty=1.8, col="green")
lines(predic_white, type="l", col="red")

##### Visualize Multivariate Polynomial Regression #####

#For red wine data set:
plot(tr_red$alcohol, tr_red$quality, main="Polynomial Regression", las=1)
model1<- lm(tr_red$quality ~tr_red$alcohol)
summary(model1)
abline(model1, lwd=3, col="red")
model2 <-lm(tr_red$quality ~ tr_red$alcohol + I(tr_red$alcohol^2))
summary(model2)
lines(smooth.spline(tr_red$alcohol, predict(model2)), col="green", lwd=4)
model3 <-lm(tr_red$quality ~ tr_red$alcohol + I(tr_red$alcohol^2)+I(tr_red$alcohol^3))
summary(model3)
lines(smooth.spline(tr_red$alcohol, predict(model3)), col="blue", lwd=4)
model4 <-lm(tr_red$quality ~ tr_red$alcohol + I(tr_red$alcohol^2)+I(tr_red$alcohol^3)+I(tr_red$alcohol^4))
summary(model4)
lines(smooth.spline(tr_red$alcohol, predict(model4)), col="magenta", lwd=4)

#For white wine data
plot(tr_white$alcohol, tr_white$quality, main="Polynomial Regression", las=1)
model5<- lm(tr_white$quality ~tr_white$alcohol)
summary(model5)
abline(model5, lwd=3, col="green")
model6 <-lm(tr_white$quality ~ tr_white$alcohol + I(tr_white$alcohol^2))
summary(model6)
lines(smooth.spline(tr_white$alcohol, predict(model6)), col="blue", lwd=3)
model7 <-lm(tr_white$quality ~ tr_white$alcohol + I(tr_white$alcohol^2)+I(tr_white$alcohol^3))
summary(model7)
lines(smooth.spline(tr_white$alcohol, predict(model7)), col="red", lwd=3)

# using the partial F-test
ANOVA(model5, model7)

####Error assumption
plot(model7) #Hit enter four times to get error plot