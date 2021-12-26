#loading the data and displaying the data

dia <- read.csv("https://raw.githubusercontent.com/Datamanim/datarepo/main/diabetes/train.csv")
View(dia)


#displaying first 5 and last 5 records in dataset.
head(dia)
tail(dia)


#displaying mean of every column
dia %>% summarise_each(funs(mean))



#displaying median
median(dia$Glucose)
median(dia$Age)



mfv(dia$SkinThickness)


#correlation between GLucose and Blood Pressure.

cor(dia$Glucose,dia$BloodPressure)



#Plotting Blood Pressure and Glucose
ggplot(dia) + aes(x=Glucose,y=BloodPressure) + geom_point(colour="#0c4c8a") + theme_minimal()



#Plotting a pair plot
pairs(dia[,c(2,3,6,7)])



#plotting the corrplot of Glucose against blood pressure

corrplot(cor(dia[,c(1,2,3,4,5,6,7,8,9)]),method= "number",type="upper")





#plotting histogram of blood pressure
hist(dia$BloodPressure,breaks = 20,main = "blood pressure histogram")



#we plot a histogram and compare it to the probability density function of the ??2-distribution with df=7

d_1<-rchisq(dia$BloodPressure,df=7)  



hist(d_1, breaks = 50, freq = FALSE, main = ('Histogram for chi square distribution '))

curve(dchisq(x, df = 7), from = 0, to = 25, n = 5000, col= 'orange', lwd=2, add = T)





#we plot a histogram and compare it to the probability density function of the t-distribution with df=10

d_2<-rt(dia$Glucose,df=10) 


hist(d_2,freq = FALSE,breaks=50)
hist(d_2, breaks = 50, freq = FALSE, main = ('Histogram for chi square distribution '))
  
curve(dt(x, df = 10), from = -4, to = 4, n = 5000, col= 'orange', lwd=2, add = T)




#contour function
contour_plot<-function(x,y)
{
  mu1<-mean(x)
  mu2<-mean(y)
  mu3<-c(mu1,mu2)
  c1<-cor(x,y)
  s1<-sqrt(var(x))
  s2<-sqrt(var(y))
  sigma1<-matrix(c(s1,c1,c1,s2),ncol = 2)
  library(MASS)
  bivn<-mvrnorm(100000,mu=mu3,Sigma = sigma1)
  head(bivn)
  bivn.kde<-kde2d(bivn[,1],bivn[,2],n=50)
  contour(bivn.kde,col="red")
}

contour_plot(dia$Glucose, dia$BloodPressure)




#perspective plot
perspec_plot<-function(x,y)
{
  mu1<-mean(x)
  mu2<-mean(y)
  mu3<-c(mu1,mu2)
  c1<-cor(x,y)
  s1<-sqrt(var(x))
  s2<-sqrt(var(y))
  sigma1<-matrix(c(s1,c1,c1,s2),ncol = 2)
  library(MASS)
  bivn<-mvrnorm(100000,mu=mu3,Sigma = sigma1)
  head(bivn)
  bivn.kde<-kde2d(bivn[,1],bivn[,2],n=50)
  persp(bivn.kde, theta = 200, phi = 20, 
        shade = 0.75, col = "light blue", expand = 0.5, r = 2, 
        ltheta = 240, ticktype = "detailed")
}  

perspec_plot(dia$Glucose, dia$BloodPressure)




#2 sided confidence interval:-


cd_normalsigma_unknown<-function(n,alpha)
{
  mu<-mean(n)
  s=sqrt(var(n))
  len1<-length(n)
  z1<-qt(1-(alpha/2),df=((len1)-1))
  f1<-(mu-(z1*(s/sqrt(len1))))
  f2<-(mu+(z1*(s/sqrt(len1))))
  f<-c(f1,f2)
  return(f)       
}

cd_normalsigma_unknown(dia$Glucose, 0.05)



#hypothesis testing:-
null_hypothesis<-function(samp,a,alpha)
{
  xbar<-mean(samp)
  s<-sqrt(var(samp))
  len1<-length(samp)
  z_stat<-qt(1-(alpha/2),df=((len1)-1))
  z1<-(xbar-a)*sqrt(len1)/s
  if(abs(z1)<=z_stat){
    print("Hyptothesis is in acceptance region")
  }else{
    print("Hypothesis is in rejectance region")
  }
}

null_hypothesis(dia$Glucose, 94.93, 0.05)




#linear regression
ggplot(dia,aes(x=Glucose,y=BloodPressure)) + geom_point() + stat_smooth(method = lm)


#linear regression model
model<-lm(BloodPressure~Glucose,data=dia)
model



new.glu<-data.frame(Glucose=c(180,105,126))
predict(model,newdata = new.glu)


#conf interval:-
confint(model)




#plot with prediction interval:-

pred.int<-predict(model,interval="prediction")

mydata<-cbind(dia,pred.int)



p<-ggplot(mydata,aes(Glucose,BloodPressure)) + geom_point() + stat_smooth(method=lm)



p + geom_line(aes(y=lwr),color="red", linetype="dashed")+ geom_line(aes(y=upr),color="red", linetype="dashed")
