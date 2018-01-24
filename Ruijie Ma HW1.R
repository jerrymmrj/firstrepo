1:20
20:1
c(1:20,19:1)
tem <- c(4,6,3)
a <- rep(tem, length=30)
a
b <- rep(tem, length=31)
b
d<-rep(tem,times=c(10,20,30))
d
x <-seq(3,6,by=0.1)
e<-exp(1)
(e**x)*cos(x)
(.01**seq(3,36, by=3))*(0.2**seq(1,34, by=3))
j<-(2**seq(1,25))
k<-seq(1,25)
j/k
i<-10:100
(i**3)+4*i**2
i<-1:25
(2**i/i)+(3**i/i**2)
paste("label",1:30)
paste("fn",1:30,sep="")
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
new_y<-yVec[-1]
new_x<-xVec[-250]
new_y - new_x
new_x<-xVec[-1]
new_y<-yVec[-250]
sin(new_y)/cos(new_x)
new_x1<-xVec[-249:-250]
new_x2<-xVec[c(-1,-250)]
new_x3<-xVec[c(-1,-2)]
new_x1+2*new_x2-new_x3
new_x4<-xVec[-250]
e**(-new_x)/(new_x4+10)
yVec[yVec>600]
yy<-sort(yVec)
order(yy)[sort(yy)>700]
xx<-sort(xVec)
xx[150]
xmean<-mean(xVec)
absx<-abs(xVec-xmean)
absx**(1/2)
sum(yVec>max(yVec)-200)
sum(xVec%%2==0)
yorder<-order(yVec)
xVec[yorder]
q<-seq(1,250,3)
yVec[q]
q<-seq(2,38,2)
w<-seq(3,39,2)
1+sum(cumprod(q/w))