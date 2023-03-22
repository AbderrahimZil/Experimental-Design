# Data source:
# Quality Progress "For Starbucks, It's in the Bag", March 2011, pp. 18-2. 
# http://rube.asq.org/quality-progress/2011/03/design-of-experiments/for-starbucks-its-in-the-bag.html

S <- read.csv('starbucks.csv')
S <- S[,c('Viscosity', 'Pressure', 'Plate.gap', 'Tear', 'Leakage')]

str(S)

##
library(rsm)

###################################################################
# The goal of the analysis is to determine the process conditions # 
### that would meet the specifications for Leakage and tearing. ###
###################################################################

## Process variables
# v: plastic viscosity (cP)
# p: calmp pressure (psi)
# g: plate gap (mm)

## Responses
# Leakage: Average over 20 binary entry
# Tear: Average Over 20 Likert Scale entry

### Tear Model
# Fitting a second order model
model_T <- rsm(Tear ~ SO(xv,xp,xg), data = S)

# checking significance of the fit
summary(model_T)

# we reduce the model to retain only the significant terms
model_T <- rsm(Tear ~ FO(xv,xp,xg) + TWI(xv,xp,xg) + PQ(xg), data = S)
summary(model_T)
# the updated model represents a good fit for the response 
# even though R� dropped by a bit the adjusted-R� increased

# Assessing the fit
## the fitted model seems to comply with the linear model assumptions
par(mfrow=c(2,2))
plot(model_T)
par(mfrow=c(1,1))

## Plotting
par(mfrow=c(2,2))
contour(model_T, ~xg+xp,
        image = TRUE, 
        xlabs=c("Plate Gap (mm)", "Pressure (psi)"))
par(mfrow=c(1,1))

### Leakage Model
# the leakage is the proportion of samples failing the water test, 
# which has a variability expected to change with the size of the proportion.
# Therefore, it would be necessary to transform it to stabilize the variance.

# th appropriate the transformation to stabilize the variance is arsin(sqrt())

# Fitting a second order model to the transformed response
model_L <- rsm(asin(sqrtLeakage) ~ FO(xp, xg)+ PQ(xg), data = S)

# checking significance of the fit
summary(model_L)
par(mfrow=c(2,2))
plot(model_L)
par(mfrow=c(1,1))


## Overlay contours
# Viscosity affected only tear, and lower viscosity minimized tear, 
# so it was best to operate at the minimum value of viscosity (at 300 cP)

par(mfrow=c(1,1))
contour(model_T, ~xg+xp, at =c(xv = -1.6129032),
        levels = c(0.75), col = "blue",
        xlabs=c("Plate Gap (mm)", "Pressure (psi)"))

contour(model_L, ~xg+xp,
        levels = c(0.05), col ="red",
        xlabs=c("Plate Gap (mm)", "Pressure (psi)"), add=TRUE)
par(mfrow=c(1,1))