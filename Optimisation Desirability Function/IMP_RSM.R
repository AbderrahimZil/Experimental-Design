# The original source is: Derringer and Suich (1980), "Simultaneous
#     Optimization of Several Response Variables", Journal of Quality
#     Technology, Vol. 12, No. 4, pp. 214-219.

# Description:  This experiment was used in the development of a tire
# tread component.  The controllable factors are:
#     X1 - hydrated silica level
#     X2 - silane coupling agent
#     X3 - sulfer level

# The response variables and their desired ranges are
#     Y1 - PICO abrasion index (120 < Y1)
#     Y2 - 200% modulus        (1000 < Y2)
#     Y3 - elongation at break (400 < Y3 < 600)0
#     Y4 - hardness            (60 < Y4 < 75)

# Its usage will be illustrated in the development of a rubber compound for tire treads.

impr <- read.csv('IMPROVE-5_5_3_2_2.txt')
## 

# rescale to (0, 1)
impr$Y1 <- scales::rescale(impr$Y1, to=c(0,10))
impr$Y2 <- scales::rescale(impr$Y2, to=c(0,10))
impr$Y3 <- scales::rescale(impr$Y3, to=c(0,10))
impr$Y4 <- scales::rescale(impr$Y4, to=c(0,10))

library(reshape2)
df <- melt(impr,  id.vars = c("Run", "x1", "x2", "x3" ),
           measure.vars = c("Y1", "Y2", "Y3", "Y4"),
           value.name = "Value",
           variable.name = 'Response')



#########################
##########################
ggplot(df, aes(x3, Value, colour = Response)) + 
  geom_point(size = 3) + 
  geom_smooth(se = FALSE, size = 1.5) +
  theme_light()+
  theme(legend.position = "none")+
  theme(legend.position="top")+
  xlab("x3") + ylab("Rescaled Response")

theme(legend.position='top', 
      legend.justification='',
      legend.direction='horizontal')
##########################
##########################

library(rsm)
md1 <- rsm(Y1 ~ SO(x1,x2,x3), data = impr)
md2 <- rsm(Y2 ~ SO(x1,x2,x3), data = impr)
md3 <- rsm(Y3 ~ SO(x1,x2,x3), data = impr)
md4 <- rsm(Y4 ~ SO(x1,x2,x3), data = impr)

summary(md4)

mesh <- expand.grid(seq(-.1, -.1, by=0), seq(-1.0, 1.0, by=0.05), seq(-1.0, 1.0, 0.05))
colnames(mesh) <- c("x1", "x2", "x3")

mesh$Y1 <- predict(md1, newdata = mesh)
mesh$Y2 <- predict(md2, newdata = mesh)
mesh$Y3 <- predict(md3, newdata = mesh)
mesh$Y4 <- predict(md4, newdata = mesh)


library(desirability)

dMxY1 <- dMax(low=120, high=170, scale = 1)
dMxY2 <- dMax(low=1000, high=1300, scale = 1)
dTrY3 <- dTarget(400, 500, 600)
dTrY4 <- dTarget(60, 67.5, 75)

overallD <- dOverall(dMxY1, dMxY2, dTrY3, dTrY4)


des <- predict(overallD, mesh[,4:7], TRUE)

Toplot <- cbind(mesh[,1:3], des)


## populating Z
X2 <- unique(Toplot$x2)
X3 <- unique(Toplot$x3)

nrow <- length(X2) 
ncol <- length(X3)

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

d <- matrix(unlist(l), nrow = nrow, ncol = ncol)

persp(X2, X3, d, theta = -27, 
      phi=18,ticktype="detailed", zlim = c(0,.70) , col="cyan",zlab="Overall Desirability",
      nticks=5, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=TRUE)

library(plot3D)
persp3D(X2, X3,d, 
        theta=30, phi=40, axes=TRUE,scale=2, box=TRUE, nticks=5, 
        ticktype="detailed", xlab="Pressure (psi)", ylab="Plate Gap (mm)", zlab="Desirability", 
        main="Desirability profile")

library(plot3Drgl)
image2Drgl(X2, X3, z = d, contour = TRUE, main = "Overlapped Contours", axes=TRUE)

persp3Drgl(X2, X3,z = d,
           xlab='', ylab='', zlab='Overall Desirability', 
           contour = list(side = "zmin"),smooth = TRUE,scale=2,
           ticktype='detailed', box=TRUE, axes=TRUE, expand=.75, nticks=5,
           shade=0.25, theta=40, phi=30)

zz = step(md1,direction="both")

anova(update(zz,~1),zz)
summary(zz)
#install.packages("olsrr")
library(olsrr)
k <- ols_step_all_possible(md1)
plot(k)

dk <- data.frame(k)
hist(impr$Y4)
par(mfrow=c(2,2))
plot(md1)    

max(dk$adjr)