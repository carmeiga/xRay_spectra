setwd("C:/Users/Carlos M Garcia/Google Drive/______tecnicasFES")


nacl <- read.csv("csvs/NaCl-AlfredoCarlos.csv")

y=nacl$yobs
theta=nacl$X.twotheta/2

plot(y~theta)
points(nacl$diff~theta,col='red')
points(nacl$yobs~theta,col='blue')


library(pracma)

p=8
pea=sort(findpeaks(y, npeaks=p, minpeakdistance=20,threshold=0, sortstr=FALSE)[,2])

par(mfrow=c(2,4))


for(k in 1:p) {
m=pea[k]
a=theta[m]-1/2
b=theta[m]+1/2

a1=which.min(abs(a-theta))
b1=which.min(abs(b-theta))


tot=a1:b1


yt=y[tot]
tt=theta[tot]
plot(yt~tt)

# code from 
# https://stackoverflow.com/questions/25015410/r-find-full-width-at-half-maximum-for-a-gausian-density-distribution

xmax <- tt[yt==max(yt)]

x1 <- tt[tt < xmax][which.min(abs(yt[tt < xmax]-max(yt)/2))]
x2 <- tt[tt > xmax][which.min(abs(yt[tt > xmax]-max(yt)/2))]
points(c(x1, x2), c(yt[tt==x1], yt[tt==x2]), col="red")

}

plot(y~theta)
points(y[tot]~theta[tot],col='red')


plot(x1,y1,type='l',xlab='Lonxitude de onda (nm)',ylab='Intensidade normalizada (-)')

points(x1q[pea],y11[pea],col='blue',pch=16)
