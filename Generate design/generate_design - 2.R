## Generate designs using DoE.bas package
# install.packages('DoE.base')
library(DoE.base)
design <- fac.design(factor.names=list(T=c(-1, 1),
                                       C=c(-1, 1),
                                       K=c(-1, 1)),
                     randomize = FALSE)

?fac.design
design
design <- add.response(design = design,
                       response = data.frame('Yield'=c(60,72,54,68,52,83,45,80)))


# ?response.names
# ?add.response


# help(fac.design)
# help(DoE.base)

# build the linear model
lm.model <- lm(Yield ~ T * K * C, data = design)
summary(lm.model)
round(summary(lm.model)$coefficients,2)
knitr::kable(summary(lm.model)$coefficients,2)

anova(lm.model)

# cube plot
# install.packages('FrF2')
library("FrF2")
attach(design)
cubePlot(Yield, T, C, K, 
         main=paste("Cube plot for Yield"),
         cex.title=1.4,
         cex.lab=1.3, 
         cex.ax=par("cex.axis"),  
         cex.clab=1.2, 
         size=0.2)
# compare the predicted to to actual
# cubePlot(lm.model,"T","K","C",main="Cube plot for pilot plant investigation", modeled = TRUE)

# Cub plot
# library("FrF2")
# cubePlot(lm.model,"T","K","C",main="Cube plot for pilot plant investigation", modeled = FALSE)
# lm and aov method for class design objects

# reveal interactions in the system
par(mfrow=c(1,3))
interaction.plot(design$T, design$C, design$Yield)
interaction.plot(design$T, design$K, design$Yield)
interaction.plot(design$C, design$K, design$Yield)
par(mfrow=c(1,1))

# install.packages('rsm')
library(rsm)
?rsm

# install.packages('pid)
library(pid)
?contourPlot

# update the linear model
library(stats)
lm.model <- update( lm.model, Yield ~ T + C + K + T : K, data = design)


# linear.model <- update.formula(lm(Yield ~ T * K * C), lm( Yield ~ T + C + K + T : K ))
round(summary(lm.model)$coefficients,2)
knitr::kable(summary(lm.model)$coefficients)
confint.lm(lm)


library(pid)
# Central composite design
P <- c(-1,   +1,  -1,  +1,     0, -1.41,     0, +1.41)
T <- c(-1,   -1,  +1,  +1, +1.41,     0, -1.41,     0)
y <- c(715, 713, 733, 725,   738,   717,   721,   710)

mod.CCD <- lm(y ~ P*T + I(P^2) + I(T^2))  
contourPlot(mod.CCD, 'P', 'T', xlim=c(-2.2, 2.2), ylim=c(-3,2))
