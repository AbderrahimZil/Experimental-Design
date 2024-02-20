mesh <- expand.grid(seq(-1.41, 1.41, by=0.02), seq(-1.41, 1.41, by=0.02))
colnames(mesh) <- c("x1", "x2")

mesh$Time <- mesh$x1*5 + 85
mesh$Temperature <- mesh$x2*5 + 175

mesh$Y1 <- predict(modelY1, newdata = mesh)
mesh$Y2 <- predict(modelY2, newdata = mesh)
mesh$Y3 <- predict(modelY3, newdata = mesh)

library(desirability)

# respect order of functions to evaluate
dMnL <- dMin(low=3000, high=3400, scale = 1)
dMnH <- dMax(low=78.5, high=80, scale=1)
dMnT <- dTarget(low=62, target=65, high=68, lowScale = 1, highScale = 1)

overallD <- dOverall(dMnH, dMnT, dMnL)

desir <- predict(overallD, mesh[,5:7], TRUE)
dataD <- cbind(mesh[,3:4], desir)


library(plot3Drgl)
image2Drgl(X, Y, z = d, contour = TRUE, main = "Overlapped Contours", 
           xlab='Time (min)', ylab='Temperature (degC)', axes=TRUE)

###############################################
################## Optimization################

searchGrid <- expand.grid(Time = seq(-1.41, 1.41, by=0.01),
                          Temperature = seq(-1.41, 1.41, by=0.01))

for(i in 1:dim(searchGrid)[1]){
  tmp <- optim(as.vector(searchGrid[i,]),
               rsmOpt,
               dObject = overallD,
               space = "circular",
               control = list(fnscale = -1))
  if(i == 1)
  {
    best <- tmp
  } else {
    if(tmp$value > best$value) best <- tmp
  }
}