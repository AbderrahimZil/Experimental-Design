S <- S[,c('Viscosity', 'Pressure', 'Plate.gap', 'Tear', 'Leakage')]
str(S)


# constructing the mesh

mesh <- expand.grid(seq(-1.6, 1.6, by=0.2), seq(-1.6, 1.6, by=0.2))

mesh$Pressure <- mesh$xp*6 + 180
mesh$Gap <- mesh$xg*1.8 + 0
mesh$Vis <- mesh$xv*30.5 + 350

# predicting the surface using the fitted models
mesh$Tear <- predict(model_T, newdata = mesh)
mesh$Leak <- predict(model_L, newdata = mesh)

#install.packages("desirability")
library(desirability)

# the goal is to minimize both the responses
dMnT <- dMin(low=0, high=0.75, scale = 1)
dMnL <- dMin(low=0, high=0.1, scale = 1)

# calculating the overall desirability
overallD <- dOverall(dMnT, dMnL)

des <- predict(overallD, mesh[,7:8], TRUE)
toplot <- cbind(mesh[,4:5], des)

## populating Z

X <- unique(toplot$Pressure)
Y <- unique(toplot$Gap)

nrow <- length(X) 
ncol <- length(Y)

z = matrix(data=NA, nrow = nrow, ncol=ncol)

l <- c()
for (i in X)
{
  for (j in Y)
  {
    v <- toplot[toplot$Pressure == i & toplot$Gap == j,]['Overall']
    l <- c(l, v)
  }
}

d <- matrix(unlist(l), nrow = nrow, ncol = ncol)

#install.packages("plot3Drgl")
library(plot3Drgl)


# the 3d plot
persp3Drgl(x=X, y=Y,z = d,
           xlab='Pressure (psi)', ylab='Plate Gap (mm)', zlab='Overall Desirability', 
           contour = list(side = "zmin"),smooth = TRUE,scale=2,
           ticktype='detailed', box=TRUE, axes=TRUE, expand=.75, nticks=5,
           shade=0.25, theta=40, phi=30)
