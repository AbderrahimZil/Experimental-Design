# The original source is: Derringer and Suich (1980), "Simultaneous
#     Optimization of Several Response Variables", Journal of Quality
#     Technology, Vol. 12, No. 4, pp. 214-219.

# Description:  This experiment was used in the development of a tire
# tread component, its usage will be illustrated in the development of a rubber compound for tire treads.  

# The controllable factors are:
#     X1 - hydrated silica level
#     X2 - silane coupling agent
#     X3 - sulfer level

# The response variables and their desired ranges are
#     Y1 - PICO abrasion index (120 < Y1)
#     Y2 - 200% modulus        (1000 < Y2)
#     Y3 - elongation at break (400 < Y3 < 600)0
#     Y4 - hardness            (60 < Y4 < 75)

## read data
impr <- read.csv('IMPROVE-5_5_3_2_2.txt')


## fit the linear models
library(rsm)
md1 <- rsm(Y1 ~ SO(x1,x2,x3), data = impr)
md2 <- rsm(Y2 ~ SO(x1,x2,x3), data = impr)
md3 <- rsm(Y3 ~ SO(x1,x2,x3), data = impr)
md4 <- rsm(Y4 ~ SO(x1,x2,x3), data = impr)


mesh$Y1 <- predict(md1, newdata = mesh)
mesh$Y2 <- predict(md2, newdata = mesh)
mesh$Y3 <- predict(md3, newdata = mesh)
mesh$Y4 <- predict(md4, newdata = mesh)

## setting the desired ranges according to the specs
dMxY1 <- dMax(low=120, high=170, scale = 1)
dMxY2 <- dMax(low=1000, high=1300, scale = 1)
dTrY3 <- dTarget(400, 500, 600)
dTrY4 <- dTarget(60, 67.5, 75)

overallD <- dOverall(dMxY1, dMxY2, dTrY3, dTrY4)

## calculate the overall desirability
des <- predict(overallD, mesh[,4:7], TRUE)

## populating Z
X2 <- unique(Toplot$x2)
X3 <- unique(Toplot$x3)

z = matrix(data=NA, nrow = nrow, ncol=ncol)

l <- c()
for (i in X2)
{
  for (j in X3)
  {
    v <- Toplot[Toplot$x2 == i & Toplot$x3 == j,]['Overall']
    l <- c(l, v)
  }
}

## 3D plot
persp(X2, X3, d, theta = -27, 
      phi=18,ticktype="detailed", zlim = c(0,.70) , col="cyan",zlab="Overall Desirability",
      nticks=5, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=TRUE)