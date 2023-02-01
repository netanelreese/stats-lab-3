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

with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,main = "Height v. BHDiameter (with least squares regression line",ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)))
)
spruce.lm=with(spruce.df, lm(Height~BHDiameter))
abline(spruce.lm)

#Task 4


