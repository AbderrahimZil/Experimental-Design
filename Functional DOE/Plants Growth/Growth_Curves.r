rm(list = ls())
library(fda)

#l <- which(CanadianWeather$place == "Vancouver")
cd <- read.csv('growth-data.csv')

t = c(0,1,2,3,5,7)
full.domain = t
x = seq(0,7, length.out = 51)

cols <- paste("day", c(0, 1, 2, 3, 5, 7), sep = "")
pots <- paste("pot_", 1:12, sep = "")
d <- t(as.matrix(cd[,cols]))

Target <- c(8)
splines <- create.bspline.basis(rangeval = c(0, max(t)),nbasis = 10,norder = 3)
fd <- Data2fd(d,argvals = t,basisobj = splines,lambda = 0.08)


#########################################################################################
############################## USING smooth.basisPar ####################################
#########################################################################################
# Smooth Data Using a Directly Specified Roughness Penalty
growth <- smooth.basisPar(t, d, fdobj= splines, lambda=0.08) # the result is a fdSmooth object
plot(growth$fd)

# extract the fd object
#gfd = growth$fd

# extract evaluation a t each time point
#gfeval = eval.fd(x,gfd)
#plot(x, gfeval[,1], type='l')

#Individually Smooth Each Curve, Evaluate at Equally Spaced Timepoints, Store in a Matrix
smoothed.curves = matrix(0,nrow=length(x),ncol=12)
for (i in 1:12) {
  temp.smooth = smooth.basisPar(t, d[,i], fdobj= splines, lambda=0.08)
  #plot(temp.smooth$fd) # this one is smooth
  smoothed.curves[,i] = eval.fd(seq(0,7,length.out = 51),temp.smooth$fd)
  #plot(smoothed.curves[,i]) # this one is points only
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
#legend('topleft',legend=c('Exp. Materials','Target'),lty=1,lwd=4,col=c('gray','black'),bty='n',cex=2)


## For Illustration, Compare the Discrete and Smoothed Profiles for the Control Sample
plot(Overall$argvals,Overall$y[,8],xlab='Time (Dayse)',ylab='Smoothed Growth (cm)',
     ylim=c(min(Overall$y),max(Overall$y)),lty=1,lwd=4,col='black',type='l',cex.lab=1.3,cex.axis=1.3)
lines(full.domain,d[,8],col='red',lty=2,lwd=4)
legend('topleft',legend=c('Discrete Curve','Smooth Curve'),lty=c(2,1),lwd=4,col=c('red','black'),bty='n',cex=2)

##### Section 3: Perform Functional Principal Components Analysis #####

pc = pca.fd(Overall$fd,nharm=3) #Perform FPCA and Store Top 2 PC Functions

#Create the Interpretable Plot of PC Functions (FPCs) (Note Not Presenting Actual FPCs)
par(mfrow=c(1,3))

plot(pc$scores)

plot(pc)
eval.fd(x, pc)
cof <- pc$harmonics$coefs
plot(cof)

plot(pc$varprop) #Display Proportion of Variance Explained

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


##### Section 4: Clustering FPC Scores #####

#Apply K-Means Clustering with K=4 to the PC Scores
fit = kmeans(pc$scores,3)

#Update Scores Plot and Smooth Curve Overlay with Cluster Colors
colors = c('red','green','orange')

par(mfrow=c(1,2))
plot(pc$scores,col=colors[fit$cluster],pch=16,cex=3,ylab='FPC Score 2',xlab='FPC Score 1',
     main='Clustering FPC Scores',cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
#points(pc$scores[Target,1],pc$scores[Target,2],col='black',pch=16,cex=3)

plot(x,smoothed.curves[,1],type='l',col=colors[fit$cluster],lwd=3,ylim=c(0,13),
     ylab='Growth (cm)',xlab='Time (Days)',main='Clustering Curves',cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
for (j in 2:j) {
  lines(x,smoothed.curves[,j],type='l',col=colors[fit$cluster[j]],lwd=4,ylim=c(0,13))
}
#lines(x,smoothed.curves[,Target],type='l',col='black',lwd=3,ylim=c(0,1))
legend('topleft',legend=c(paste('Cluster:',1:3)),col=c(colors),lty=1,bty='n',cex=1.5,lwd=3)
abline(h=0,lwd=2,lty=3)


##### Section 3: Perform Functional Principal Components Analysis #####
pc = pca.fd(Overall$fd,nharm=2) #Perform FPCA and Store Top 2 PC Functions
pc_scores = data.frame(pc$scores)
names(pc_scores) <- c('PC1', 'PC2')
rownames(pc_scores) <- paste("Run/Pot ", 1:12, sep = "")


#Create the Interpretable Plot of PC Functions (FPCs) (Note Not Presenting Actual FPCs)
par(mfrow=c(1,2))
plot(pc$harmonics[2])
plot(pc$scores)
pc$varprop #Display Proportion of Variance Explained

#In a 2x2 Grid Plot the Mean Function, Actual FPCs, and FPC Scores ([2,2] Cell Empty)
par(mfrow=c(2,2))
PC.Matrix = eval.fd(x,pc$harmonics)
mean.function = as.vector(eval.fd(x,pc$meanfd))

plot(x,mean.function,lwd=3,ylab='Material Characteristic',xlab='Time (Hours)',
     main='Mean Function',col='black',ylim=c(0,13),type='l',cex.lab=1.5,cex.axis=1.5,cex.main=2)

plot(x,PC.Matrix[,1],lwd=3,ylab='',xlab='Time (Hours)',main='PC Functions',col='blue',
     ylim=range(PC.Matrix),type='l',cex.lab=1.5,cex.axis=1.5,cex.main=2)
lines(x,PC.Matrix[,2],lwd=3,col='red',lty=2)
abline(h=0,lwd=2,lty=3)
legend('bottomright',legend=c('FPC 1','FPC 2'),col=c('blue','red'),lty=c(1,2),lwd=2,bty='n',cex=1.5)

#Plot PC Scores in 2-Dimensions, Highlighting Control Sample
plot(pc$scores,pch=16,cex=2,col='gray',main='FPC Scores',xlab='FPC Score 1',ylab='FPC Score 2',
     cex.lab=1.5,cex.axis=1.5,cex.main=2)
points(pc$scores[Target,1],pc$scores[Target,2],pch=16,col='black',cex=2)
legend('top',legend=c('Exp. Materials','Target'),pch=16,col=c('gray','black'),lty=2,box.lty = 2,bty='n',cex=1.5)

plot(Overall$argvals,Overall$y[,1],xlab='Time (Days)',ylab='Smoothed Growth (cm)',
     ylim=c(min(Overall$y),max(Overall$y)),lty=1,lwd=4,col='gray',type='l',cex.lab = 1.3,cex.main=2,cex.axis=1.3)
for (j in 2:ncol(Overall$y)) {
  lines(Overall$argvals,Overall$y[,j],lty=1,lwd=4,col='gray')
}
lines(Overall$argvals,Overall$y[,4],lty=1,lwd=4,col='black')
legend('topleft',legend=c('Exp. Materials','Target'),lty=1,lwd=4,col=c('gray','black'),bty='n',cex=1.5)


#Suppose We Wanted to Plot the Nearest j Observed Curves Based on Euclidean Distance in 2-D Space

Y = t(d)
control = Target
Plot.Closest = function(j) {
  distances = c()
  for (i in 1:k) distances[i] = sqrt(sum((pc$scores[control,] - pc$scores[i,])^2))
  closest = seq(1:k)[order(distances)[2:(j+1)]]
  plot(full.domain[!is.na(Y[closest[1],])],Y[closest[1],!is.na(Y[closest[1],])],type='l',
       col=colors[fit$cluster[closest[1]]],
       lwd=3,ylim=c(0,13),xlab='Time (Days)',ylab='Growth (cm)',
       main=paste('Closest',j,'Curve'),cex.lab=1.5,cex.axis=1.5,cex.main=2)
  for (l in 2:length(closest)) {
    lines(full.domain[!is.na(Y[closest[l],])],Y[closest[l],!is.na(Y[closest[l],])],lwd=2,
          col=colors[fit$cluster[closest[l]]])
  }
  lines(full.domain[!is.na(Y[control,])],Y[control,!is.na(Y[control,])],lwd=3,col='black')
}

Plot.Closest(11)


##### Section 5: Exploration of Impact of Various FPC Scores on Functional Data Shape #####

#Store the Mean Function and PC Functions (Karhunen-Loeve Expansion Building Blocks)
mean.function = as.vector(eval.fd(seq(0,7),pc$meanfd))
pc.functions = eval.fd(seq(0,7),pc$harmonics) #When Used in this Way, Empirical Basis Functions

#Randomly Select 12 Pairs of New Scores (Restricted to the Range of Observed Scores)

new.scores = cbind(runif(100,min(pc$scores[,1]),max(pc$scores[,1])),runif(100,min(pc$scores[,2]),max(pc$scores[,2])))
new.curves = matrix(0,nrow=100,ncol=8)

#For the First Set of Scores, Plot the following in a 2 x 2 Plot Matrix
#The 1st FPC (Blue Solid Line) with FPC1 * New Score Overlaid (Dashed Blue Line)
#The 2st FPC (Red Solid Line) with FPC2 * New Score Overlaid (Dashed Red Line)
#The Scores Plot of Original Data with Added Point for New Score Pair (Maroon)
#The Resulting New Curve Based on Karhunen-Loeve (Maroon) Overlaid with the Mean Function

#For the Remaining New Score Pairs, Progressively Build on Above Plot.
#All Results from Past Iterations are Changed to Light Gray Color. 
#Current Iteration in Full Colors (Blue, Red, and Maroon Respectively)

##### Section 5: Exploration of Impact of Various FPC Scores on Functional Data Shape #####

x = seq(0,7, length.out = 51)

#Store the Mean Function and PC Functions (Karhunen-Loeve Expansion Building Blocks)
mean.function = as.vector(eval.fd(x,pc$meanfd))
pc.functions = eval.fd(x,pc$harmonics) #When Used in this Way, Empirical Basis Functions

#Randomly Select 12 Pairs of New Scores (Restricted to the Range of Observed Scores)

new.scores = cbind(runif(100,min(pc$scores[,1]),max(pc$scores[,1])),runif(100,min(pc$scores[,2]),max(pc$scores[,2])))
new.curves = matrix(0,nrow=100,ncol=51)

#For the First Set of Scores, Plot the following in a 2 x 2 Plot Matrix
#The 1st FPC (Blue Solid Line) with FPC1 * New Score Overlaid (Dashed Blue Line)
#The 2st FPC (Red Solid Line) with FPC2 * New Score Overlaid (Dashed Red Line)
#The Scores Plot of Original Data with Added Point for New Score Pair (Maroon)
#The Resulting New Curve Based on Karhunen-Loeve (Maroon) Overlaid with the Mean Function

par(mfrow=c(2,2))
plot(x,pc.functions[,1],type='l',lwd=3,col='blue',ylab='Growth (cm)',
     xlab='Time (days)',main='PC Function 1',ylim=c(min(pc.functions[,1])- .1,max(pc.functions[,1])+.1),
     cex.lab=1.3,cex.axis=1.3,cex.main=1.5)
lines(x,pc.functions[,1] * new.scores[1,1],col='blue',lwd=3,lty=2)
abline(h=0,lwd=2,lty=3)
plot(x,pc.functions[,2],type='l',lwd=3,col='red',ylab='Percentage Performance',
     xlab='Time (days)',main='PC Function 2',ylim=c(-2,2),cex.lab=1.3,cex.axis=1.3,cex.main=1.5)
lines(x,pc.functions[,2] * new.scores[1,2],col='red',lwd=3,lty=2)
abline(h=0,lwd=2,lty=3)

plot(pc$scores,col='gray',pch=16,cex=3,ylab='FPC Score 2',xlab='FPC Score 1',main='PC Scores',
     cex.lab=1.3,cex.axis=1.3,cex.main=1.5)
#points(pc$scores[Target,1],pc$scores[Target,2],col='black',pch=16,cex=3)
points(new.scores[1,1],new.scores[1,2],col='maroon',cex=3,pch=13)
# Construct curves target and new, each curve is a linear combination of PCA1 and PCA2
target.curve = mean.function + pc.functions[,1] * pc$scores[Target,1] + pc.functions[,2] * pc$scores[Target,2]
new.curves[1,] = mean.function + pc.functions[,1] * new.scores[1,1] + pc.functions[,2] * new.scores[1,2]

plot(x,target.curve,ylim=c(0,15),type='l',lwd=3,ylab='Growth (cm)',xlab='Time (Days)',
     main='Resulting Growth Curve',cex.main=1.5,cex.lab=1.3,cex.axis=1.3)
#legend('top',legend=c('Target'),lty=1,lwd=4,col=c('black'),bty='n',cex=1.5)
lines(x,new.curves[1,],col='maroon',lwd=3)
#dev.copy(png,'aaaACurves.png')
#dev.off()

#For the Remaining New Score Pairs, Progressively Build on Above Plot.
#All Results from Past Iterations are Changed to Light Gray Color. 
#Current Iteration in Full Colors (Blue, Red, and Maroon Respectively)

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
  #plot(pc$scores[Target,1],pc$scores[Target,2],col='black',pch=16,cex=3)
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
  #legend('top',legend=c('Target'),lty=1,lwd=4,col=c('black'),bty='n',cex=1.5)
  
  #dev.copy(png,paste('ACurves', l, '.png', sep = ""))
  #dev.off()
  Sys.sleep(1)
}


##### Section 7: Regression Analysis on pc scores, this is a scalar on scalar regression
library(jtools)

X <- cbind(cd[, c('substrate', 'qlight', 'cress')], pc$scores[,1], pc$scores[,2])
names(X) <- c('substrate', 'qlight',  'cress', 'Y1', 'Y2')

xs <- rbind(c(1, 1, 1),
            c(1, 1, 0),
            c(0, 1 ,1),
            c(1, 0, 0),
            c(0, 0, 0),
            c(0, 1, 0),
            c(1, 1, 1),
            c(1, 0, 1),
            c(0, 0 ,1),
            c(1, 0, 0),
            c(0, 0, 1),
            c(0, 1, 0))
y1 <- pc$scores[,1]
lm.model <- lm(y1 ~ xs)

#anova.mod = anova(lm(Y ~ substrate * qlight * cress, data = X))
aov.mod = aov(Y2 ~ substrate * qlight + cress, data = X)
summary(aov.mod)

s <- X$substrate
q <- X$qlight
c <- X$cress

y1 <- X$Y1
y2 <- X$Y2

lm1 <- lm(y1 ~ s * q + c)
lm2 <- lm(y2 ~ s * q )

summary(lm2)

point = data.frame(s = 'soil', q = 'light', c = 'curled')
predict(lm1, point)
predict(lm2, point)


# predictig the pc scores at given conditions
point <- data.frame(expand.grid(substrate=unique(X$substrate), 
                                qlight=unique(X$qlight), 
                                cress=unique(X$cress)))



# Create some sample data
set.seed(123)
x <- 1:10
y <- 2*x + rnorm(10)

# Fit a linear regression model
model <- lm(y ~ x)

# New data point for prediction
new_point <- data.frame(x = 15)

# Make prediction
predicted_value <- predict(model, newdata = new_point)

# Print predicted value
print(predicted_value)


#model.tables( aov.mod, type = "means", se = T )

X$substrate <- as.factor(X$substrate)
X$qlight <- as.factor(X$qlight)
X$cress <- as.factor(X$cress)

contrasts(X$substrate) <- contr.sum(2)
contrasts(X$qlight) <- contr.sum(2)
contrasts(X$cress) <- contr.sum(2)



set.seed(313) # seed

t = 100 # time points
n = 50 # sample size

# generate some mock data
Y = sweep(matrix(rnorm(n * t, 0, 1), ncol = 50), 1, sin(1:100)) 
plot(Y[,50], type='l')
X2 = sample(c(0, 1), size = n, replace = TRUE)
X3 = rnorm(n)

# generate a basis for Y
Yfd = fda::smooth.basis(1:t, Y, fda::create.bspline.basis(c(1, 100), 2, 1))$fd

# run the regression
ffit = fda::fRegress(Yfd ~ X2 + X3)
plot(fda::predict.fRegress(ffit))

Overall
str(X)
knitr::kable(anova.mod)
summary.lm(aov.mod)

# linear model 
model <- lm(Y ~ substrate * qlight + cress, data = X)
summ(model)

summary(model$coefficients)

##### Section 4: Clustering FPC Scores #####
#### Curves
library(funFEM)
# Clustering
res = funFEM(fd,K=3)
# Visualization of the partition and the group means
par(mfrow=c(1,2))
plot(fd,col=res$cls,lwd=2,lty=1)
fdmeans = fd
fdmeans$coefs = t(res$prms$my)
plot(fdmeans,col=1:max(res$cls),lwd=2)


library(daewr)
data(sugarbeet)
