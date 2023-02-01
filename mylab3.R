# Task 1
dird="\\Users\\reese\\Desktop\\fa23\\stats\\lab-3\\"
getwd()

# Task 2
spruce.df = read.csv("SPRUCE.csv")
head(spruce.df)

# Task 3

with(spruce.df,  {
  plot(Height~BHDiameter,bg="Blue",pch=21, cex=1.2,main = "Height v. BHDiameter", ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
}
)

with(spruce.df, {
  layout(matrix(1:3,nr=3,byrow=TRUE))
  trendscatter(Height~BHDiameter,f=0.5)
  trendscatter(Height~BHDiameter,f=0.6)
  trendscatter(Height~BHDiameter,f=0.7)
  
})

#Task 4

layout(matrix(1:4,nr=2,nc=2))

#Lets look at where the plots will go

AB = with(spruce.df, 
          plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,main = "Height v. BHDiameter (with least squares regression line",ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
spruce.lm=with(spruce.df, lm(Height~BHDiameter))
abline(spruce.lm)

with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
#make yhat the estimates of E[Height | BHDiameter]
yhat=with(spruce.df,predict(spruce.lm,data.frame(BHDiameter)))
#OR you could use -- (yhat values the predicted values for all the BHDiameter values )
yhat=fitted(spruce.lm)

# Draw in segments making the residuals (regression deviations)
with(spruce.df,{
  segments(BHDiameter,Height,BHDiameter,yhat)
})
abline(spruce.lm)

#residual sum of squares
RSS=with(spruce.df,sum((Height-yhat)^2))

#MSS
with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)

#make nieve model
with(spruce.df, abline(h=mean(Height)))
abline(spruce.lm)

#make the explained deviations (explained by the model)
with(spruce.df, segments(BHDiameter,mean(Height),BHDiameter,yhat,col="Red"))
MSS=with(spruce.df,sum((yhat-mean(Height))^2))
MSS

with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
with(spruce.df,abline(h=mean(Height)))
with(spruce.df, segments(BHDiameter,Height,BHDiameter,mean(Height),col="Green"))
TSS=with(spruce.df,sum((Height-mean(Height))^2))
par(mfrow=c(1,1))

# Task 5

summary(spruce.lm)
# Slope = 0.48147
# Intercept = 9.14684
# EQN = 0.48147 * BHDiameter + 9.14684
predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))

# Task 6

library(ggplot2)
g=ggplot(spruce.df, aes(x=BHDiameter,y=Height,colour=BHDiameter))
g=g+geom_point() + geom_line()+ geom_smooth(method="lm")
g+ggtitle("Height Vs BHDiameter")


