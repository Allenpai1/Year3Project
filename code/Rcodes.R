

#Author: Haijin Li


##################################
library(glmpath)
data2<-read.csv('/Users/allenli/Desktop/Year3Project/Dataset/Gene expression dataset/trian.csv',sep=',')
data2
ytrain<-read.csv('/Users/allenli/Desktop/Year3Project/Dataset/Gene expression dataset/ytrian.csv',sep=',')
data2
names(data2)
head(data2)
dim(data2)
scaled.dat <- scale(data2)
scaled.dat

dim(scaled.dat)
options(max.print=1000000)
dim(scaled.dat)

ytrain<-ytrain['cancer']
ytrain
ytrain <- as.numeric(unlist(ytrain))
fit<-glmpath(scaled.dat,ytrain,family=binomial)
par(mfrow=c(1,1))
plot(fit)
plot(fit, xvar="lambda")
plot(fit, xvar="step")
plot(fit, xvar="step", xlimit=8)
plot(fit, type="aic")
plot(fit, type="bic")

library(glmnet)

traindata3<-read.csv('/Users/allenli/Desktop/Year3Project/regressiontrain.csv',sep=',',nrows=50)
ytrain<-read.csv('/Users/allenli/Desktop/Year3Project/regressiontraintarget.csv',sep=',',nrows=50)
testdata<-read.csv('/Users/allenli/Desktop/Year3Project/regressiontest.csv',sep=',')
ytest<-read.csv('/Users/allenli/Desktop/Year3Project/regressiontesttarget.csv',sep=',')
head(traindata3)
dim(traindata3)
dim(ytrain)
traindata3<-traindata3[2:220]
testdata<-testdata[2:220]
testdata[1:219]
ytest<-ytest[2]
ytrain<-ytrain[2]
dim(ytrain)[1]
traindata3['sales']=ytrain
testdata['sales']=ytest
dim(testdata)[2]
house.full<-lm(formula=sales~.+0,data=traindata3)
house.fit.intercept<-lm(formula=sales~MSSubClass+0,data=traindata3)
forward.step.model<-step(house.fit.intercept,scale=summary(house.full)$sigma^2,scope=list(lower=house.fit.intercept, upper=house.full),direction="forward")

backward.step.model<-step(house.full,scale=summary(house.full)$sigma^2)
pred4  <- mapply('predict', forward.step.model, split(testdata,testdata$state))

pred.w.plim <- predict(forward.step.model, testdata, interval = "prediction")

formula(forward.step.model)

lm.formula<-lm(formula=formula(backward.step.model),data=traindata3)

y_hat_test<-predict(lm.formula,data=testdata[1:219])

testMSE<-mean((testdata$sales-y_hat_test)^2)
testMSE


summary(forwaed)
summary(house.full)

msea<-1:218
mseb<-1:218

msea
mseb
capture.output(msea, file = "msea.csv") 
capture.output(mseb, file='mseb.csv')
for (index in 1:218){
  val<-index
  traindata<-traindata3[1:val]
  testdata2<-testdata[1:val]
  traindata['sales']=ytrain
  testdata2['sales']=ytest
  MSEfb<-Forward.Backward.Calcuation(traindata,testdata2)
  msea[val]=MSEfb[1]
  mseb[val]=MSEfb[2]
}

Forward.Backward.Calcuation<-function(train,test){
  house.full<-lm(formula=sales~.+0,data=train)
  house.fit.intercept<-lm(formula=sales~MSSubClass+0,data=train)
  forward.step.model<-step(house.fit.intercept,scale=summary(house.full)$sigma^2,scope=list(lower=house.fit.intercept, upper=house.full),direction="forward")
  backward.step.model<-step(house.full,scale=summary(house.full)$sigma^2)
  f<-formula(forward.step.model)
  b<-formula(backward.step.model)
  forward.lm<-lm(formula=f,data=test)
  backward.lm<-lm(formula=b,data=test)
  p<-dim(test)[2]
  fyHtest<-predict(forward.lm,data=testdata[1:p-1])
  byHtest<-predict(backward.lm,data=testdata[1:p-1])
  ftestMSE<-mean((testdata$sales-fyHtest)^2)
  btestMSE<-mean((testdata$sales-byHtest)^2)
  mse_vector<-c(ftestMSE,btestMSE)
  return(mse_vector)
}
msea<-1:217
msea
for (index in 151:217){
  val<-index
  traindata<-traindata3[1:val]
  testdata2<-testdata[1:val]
  traindata['sales']=ytrain
  testdata2['sales']=ytest
  MSEfb<-Forward.Calcuation(traindata,testdata2)
  msea[val]=MSEfb
}
dim(traindata3)
dim(traindata3[1:10])
Forward.Calcuation<-function(train,test){
  house.full<-lm(formula=sales~.+0,data=train)
  house.fit.intercept<-lm(formula=sales~MSSubClass+0,data=train)
  forward.step.model<-step(house.fit.intercept,scale=0,scope=list(lower=house.fit.intercept, upper=house.full),direction="forward")
  f<-formula(forward.step.model)
  forward.lm<-lm(formula=f,data=test)
  p<-dim(test)[2]
  fyHtest<-predict(forward.lm,data=testdata[1:p-1])
  ftestMSE<-mean((testdata$sales-fyHtest)^2)
  return(ftestMSE)
}

s<-Forward.Backward.Calcuation(traindata3,testdata)

s
length(coef(forward.step.model))
summary(backward.step.model)

MSE<-mean(forward.step.model$residuals^2)
MSE
MSE2<-mean(backward.step.model$residuals^2)
MSE2
length(coef(backward.step.model))

length(coef(forward.step.model))

lasso.fit <- glmnet(x2, y2, alpha = 1.0,standardize = TRUE)



elastic.fit <- glmnet(x2, y2, alpha = 0.5)
summary(step.model)




library(latex2exp)
data1<-read.csv("flies.csv")
data1
scaled.dat <- scale(data1)
scaled.dat<-as.data.frame(scaled.dat)
x<-cbind(scaled.dat$length,scaled.dat$wing,scaled.dat$sex)


flies.fit.intercept<-lm(formula=eye~sex+0,data=scaled.dat)
flies.full<-lm(eye~length*wing*sex,data=data1)
forward.step.model<-step(flies.fit.intercept,scale=summary(flies.full)$sigma^2,scope=list(lower=flies.fit.intercept, upper=flies.full),direction="forward")
backward.step.model<-step(flies.full)


y<-scaled.dat$eye
x
y
# check that we get mean of 0 and sd of 1
colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
apply(scaled.dat, 2, sd)
head(scaled.dat)
names(scaled.dat)
dim(scaled.dat)
lambda <- 10^seq(3,-2,by=-.1)
lambda
fit2 <- glmnet(x, y,alpha=0.5, lambda = 0.1535394)
summary(fit2)
fit <- cv.glmnet(x, y,alpha=0.5 ,nfolds=8, lambda = lambda)
fit

opt_lambda <- fit$lambda.min
opt_lambda
mse.min <- min(fit$cvm)
mse.min
plot(fit2,cex.lab=1,mgp=c(2.2,1,0), xvar="lambda", label=TRUE, xlab=TeX(r'($\log(\lambda_1,\lambda_2)$)'),ylab=TeX(r'($\hat{\theta}_{elastic}(\lambda_1,\lambda_2)$)'))
fig.dim=c(4,2)
title('Elastic-net regression solution path',line=3)
dev.off()
fig.dim=c(2,2)
plot(fit,colour="blue", method = "line", xlab=TeX(r'($\log(\lambda_1)$)'))
title('Lasso regression cross validation selection',line=3)
dev.off()
scaled.dat$sex<-as.factor(data1$sex)
data1$sex

fit.full<- lm(eye~length*wing*sex,data=data1)


fit.ss<-lm(eye~wing+sex+length+wing:sex+sex:length,data=data1)
summary(fit.full)

summary(fit.full)$sigma
summary(fit.ss)$sigma
summary(fit.ss)
#7) 0.537

anova(fit.full)

#8)6.5082

step(fit.full,scale=summary(fit.full)$sigma^2)
fit.intercept<-lm(eye~1,data=scaled.dat)
step(fit.intercept,scale=summary(fit.full)$sigma^2,scope=list(lower=fit.intercept, upper=fit.full),direction="forward")



#9) 4.29

par(mfrow=c(2,2))
plot(fit.full)
#Q10) 2x2 plot

#Q11) come back 

dev.off()
fit.add<- lm(eye~length+wing+sex,data=data1)
summary(fit.add)
#standard error:0.1577 and R value 0.9054
library(MASS)
boxcox(fit.add)
#12)lambda very close to 0 so log transformation 

fit.add.BC.<- lm(log(eye)~length+wing+sex,data=data1)
summary(fit.add.BC.)
#standard error:0.06334 and R value 0.927

d<-cooks.distance(fit.add.BC.)#put model in 
plot(d)
identify(d)
#point 16
d
#13) 0.6043624482 which above 0.5 might have high influence


#Part2: PCA

##################################


fliesnew<-data.frame("length"=data1$length,"wing"=data1$wing,"eye"=data1$eye)
head(fliesnew)
names(fliesnew)
dim(fliesnew)
flies<-fliesnew[,c("length","wing")]#take only the regressors variables
flies
flies.pca<-prcomp(flies)
flies.pca$sdev
flies.pca$rot

plot(flies.pca)

flies.pca$sdev^2

#Can check eigenvalues correct
eigen(var(flies))$values

#Can check proportions correct
flies.pca$sdev^2/sum(flies.pca$sdev^2)

summary(flies.pca)
#Q15)0.04618 lost 

#The mean vectors
m<-colMeans(flies)
m
flies.pca$x[,1]

flies.regression<-cbind(fliesnew,flies.pca$x[,1])
flies.regression
flies.regression<-data.frame("length"=data1$length,"wing"=data1$wing,"eye"=data1$eye,"pc1"=flies.pca$x[,1])
flies.regression
#add the pc to gaia data
fit.pcr.<-lm(eye~pc1,data=flies.regression)
summary(fit.pcr.)
#16) estimated coefficient 0.49480


#Part3:  Factorial Experiments

##################################

data2<-read.csv("cars.csv")
head(data2)
names(data2)
dim(data2)
data2

data2$cyl<-as.factor(data2$cyl)
data2$cyl
data2$gear<-as.factor(data2$gear)
data2$gear

#17) i) yes, as there is no empty cells
#ii) no, as the number of replicates in every cell is not equal.To see this, there are 
#14 replicate for cyl levels 8 , but only 7 replicates for level 6

car.add<-lm(mpg~cyl+gear,data=data2)
summary(car.add)
new<-data.frame(cyl=as.factor(8), gear=as.factor(4))
predict(car.add, newdata=new, interval="prediction", level=0.99)
predict(car.add, newdata=new)
#18) 16.20978

car.inter<-lm(mpg~cyl*gear,data=data2)
summary(car.inter)
interaction.plot(data2$cyl,data2$gear, data2$mpg,ylab="mpg",xlab="cyl",trace.label="gear")
