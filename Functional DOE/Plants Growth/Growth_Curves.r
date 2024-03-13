rm(list = ls())
library(fda)

#l <- which(CanadianWeather$place == "Vancouver")
cd <- read.csv('growth-data.csv')

t = c(0,1,2,3,5,7)

cols <- paste("day", c(0, 1, 2, 3, 5, 7), sep = "")
pots <- paste("pot_", 1:12, sep = "")
d <- t(as.matrix(cd[,cols]))

Target <- c(8)
splines <- create.bspline.basis(rangeval = c(0, max(t)),nbasis = 10,norder = 3)
fd <- Data2fd(d,argvals = t,basisobj = splines,lambda = 0.08)


#########################################################################################
############################## USING smooth.basisPar ####################################
#########################################################################################
growth <- smooth.basisPar(t, d, fdobj= splines, lambda=0.08) 

#Individually Smooth Each Curve, Evaluate at Equally Spaced Timepoints, Store in a Matrix
smoothed.curves = matrix(0,nrow=length(x),ncol=12)
for (i in 1:12) {
  temp.smooth = smooth.basisPar(t, d[,i], fdobj= splines, lambda=0.08)
  smoothed.curves[,i] = eval.fd(seq(0,7,length.out = 51),temp.smooth$fd)
}

#Generate a Single Functional Data Object For Use in FPCA
Overall = smooth.basisPar(x, smoothed.curves, fdobj= splines, lambda=0.08)

## Recreate the Overlay Plot of All Profiles, Now with Smoothed Versions
plot(Overall$argvals,Overall$y[,1],xlab='Time (Days)',ylab='Smoothed Growth (cm)',
     ylim=c(min(Overall$y),max(Overall$y)),lty=1,lwd=4,col='gray',type='l',cex.lab = 1.3,cex.main=2,cex.axis=1.3)
for (j in 2:ncol(Overall$y)) {
  lines(Overall$argvals,Overall$y[,j],lty=1,lwd=4,col='gray')
}
lines(Overall$argvals,Overall$y[,4],lty=1,lwd=4,col='black')
legend('topleft',legend=c('Target'),lty=1,lwd=4,col=c('black'),bty='n',cex=2)


## For Illustration, Compare the Discrete and Smoothed Profiles for the Control Sample
plot(Overall$argvals,Overall$y[,8],xlab='Time (Dayse)',ylab='Smoothed Growth (cm)',
     ylim=c(min(Overall$y),max(Overall$y)),lty=1,lwd=4,col='black',type='l',cex.lab=1.3,cex.axis=1.3)
lines(full.domain,d[,8],col='red',lty=2,lwd=4)
legend('topleft',legend=c('Discrete Curve','Smooth Curve'),lty=c(2,1),lwd=4,col=c('red','black'),bty='n',cex=2)

##### Perform Functional Principal Components Analysis #####

pc = pca.fd(Overall$fd,nharm=3) 

#In a 2x2 Grid Plot the Mean Function, Actual FPCs, and FPC Scores ([2,2] Cell Empty)
par(mfrow=c(2,2))
PC.Matrix = eval.fd(x,pc$harmonics)
mean.function = as.vector(eval.fd(x,pc$meanfd))

plot(x,mean.function,lwd=3,ylab='Material Characteristic',xlab='Time (Days)',
     main='Mean Function',col='black',ylim=c(0,13),type='l',cex.lab=1.5,cex.axis=1.5,cex.main=2)

plot(x,PC.Matrix[,1],lwd=3,ylab='',xlab='Time (Days)',main='PC Functions',col='blue',
     ylim=range(PC.Matrix),type='l',cex.lab=1.5,cex.axis=1.5,cex.main=2)
lines(x,PC.Matrix[,2],lwd=3,col='red',lty=2)
abline(h=0,lwd=2,lty=3)
legend('bottomleft',legend=c('FPC 1','FPC 2'),col=c('blue','red'),lty=c(1,2),lwd=2,bty='n',cex=1.5)

#Plot PC Scores in 2-Dimensions, Highlighting Control Sample
plot(pc$scores,pch=16,cex=2,col='gray',main='FPC Scores',xlab='FPC Score 1',ylab='FPC Score 2',
     cex.lab=1.5,cex.axis=1.5,cex.main=2)
points(pc$scores[Target,1],pc$scores[Target,2],pch=16,col='black',cex=2)
legend('top',legend=c('Exp. Materials','Target'),pch=16,
       col=c('gray','black'),cex=1.5)


##### Clustering FPC Scores #####

#Apply K-Means Clustering with K=4 to the PC Scores
fit = kmeans(pc$scores,3)

#Update Scores Plot and Smooth Curve Overlay with Cluster Colors
colors = c('red','green','orange')

par(mfrow=c(1,2))
plot(pc$scores,col=colors[fit$cluster],pch=16,cex=3,ylab='FPC Score 2',xlab='FPC Score 1',
     main='Clustering FPC Scores',cex.lab=1.5,cex.axis=1.5,cex.main=1.5)

plot(x,smoothed.curves[,1],type='l',col=colors[fit$cluster],lwd=3,ylim=c(0,13),
     ylab='Growth (cm)',xlab='Time (Days)',main='Clustering Curves',cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
for (j in 2:j) {
  lines(x,smoothed.curves[,j],type='l',col=colors[fit$cluster[j]],lwd=4,ylim=c(0,13))
}
legend('topleft',legend=c(paste('Cluster:',1:3)),col=c(colors),lty=1,bty='n',cex=1.5,lwd=3)
abline(h=0,lwd=2,lty=3)

##### Exploration of Impact of Various FPC Scores on Functional Data Shape #####

x = seq(0,7, length.out = 51)

# Store the Mean Function and PC Functions (Karhunen-Loeve Expansion Building Blocks)
mean.function = as.vector(eval.fd(x,pc$meanfd))
pc.functions = eval.fd(x,pc$harmonics) #When Used in this Way, Empirical Basis Functions

# Randomly Select 12 Pairs of New Scores (Restricted to the Range of Observed Scores)

new.scores = cbind(runif(100,min(pc$scores[,1]),max(pc$scores[,1])),runif(100,min(pc$scores[,2]),max(pc$scores[,2])))
new.curves = matrix(0,nrow=100,ncol=51)

par(mfrow=c(2,2))
for (l in 2:100) {
  plot(x,pc.functions[,1],type='l',lwd=3,col='blue',ylab='Percentage Growth', xlab='Time (Days)',
       main='PC Function 1',ylim=c(-6,6),cex.lab=1.3,cex.axis=1.3,cex.main=1.4)
  for (h in 1:(l-1)){
    lines(x,pc.functions[,1] * new.scores[h,1],col='gray',lwd=2,lty=3)
  }
  lines(x,pc.functions[,1] * new.scores[l,1],col='blue',lwd=3,lty=2)
  plot(x,pc.functions[,2],type='l',lwd=3,col='red',ylab='Percentage Growth', xlab='Time (Days)',
       main='PC Function 2',ylim=c(-2,2),cex.lab=1.3,cex.axis=1.3,cex.main=1.4)
  for (h in 1:(l-1)){
    lines(x,pc.functions[,2] * new.scores[h,2],col='gray',lwd=2,lty=3)
  }
  lines(x,pc.functions[,2] * new.scores[l,2],col='red',lwd=3,lty=2)
  plot(pc$scores,col='white',pch=16,cex=3,ylab='FPC Score 2',xlab='FPC Score 1', main='PC Scores', cex.lab=1.3,cex.axis=1.3,cex.main=1.4)
  points(pc$scores[Target,1],pc$scores[Target,2],pch=16,col='black',cex=3)
  points(new.scores[1:(l-1),1],new.scores[1:(l-1),2],col='gray',cex=2.5,pch=1)
  points(new.scores[l,1],new.scores[l,2],col='maroon',cex=2.5,pch=13)
  
  new.curves[l,] = mean.function + pc.functions[,1] * new.scores[l,1] + pc.functions[,2] * new.scores[l,2]
  
  plot(x,target.curve,ylim=c(0,13),type='l',lwd=3,ylab='Growth (cm)',xlab='Time (Days)',
       main='New Growth Curves',cex.lab=1.3,cex.axis=1.3,cex.main=1.4)
  for (h in 1:(l-1)){
    lines(x,new.curves[h,],col='gray',lwd=2,lty=3)
  }
  lines(x,new.curves[l,],col='maroon',lwd=3)
  lines(x,target.curve,lwd=2)

  Sys.sleep(1)
}