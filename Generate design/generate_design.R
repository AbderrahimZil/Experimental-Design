## Create a factorial design using FrF2 library
## FrF2 creates a Fractional Factorial Designs with 2-Level Factors
library(FrF2)

# Our goal is to create a 3-factors full factorial design
# We specify the number of run (here 8 = 2^3) and the # of factors (here 3)
# Factor levels are fed for the third argument as shown bellow

design <- FrF2(8, 3,
             factor.names = list(T = c(160, 180),
                                 C = c(20, 40),
                                 K = c('A', 'B')),
             randomize = FALSE)

# Afetr running the experiments we insert the response 
# into the design matrix

Yield <- c(60, 72, 54, 68, 52, 83, 45, 80)
design <- add.response(design = design, response = Yield)
summary(design)

# For visualisation urposes we display the design
cube_plot <- cubePlot(Yield, T, C, K, main=paste("Cube Plot"),
                      cex.title=0.8,
                      cex.lab=1.0, 
                      cex.ax=par("cex.axis"), 
                      cex.clab=1.2,
                      size = .2)
# main effects plot
plot(design, cex = 1.0, cex.lab = 1.0, cex.axis = 0.9,
     main = "Main effects plot for MI", cex.main = 0.9)

MEPlot(design, abbrev = 5, cex.xax = 0.9, cex.main = 0.9)

# interaction plot
IAPlot(design, abbrev = 3, show.alias = TRUE, lwd = 2,
       cex = 2, cex.xax = 1.2, cex.lab = 1.5)





## Create a factorial design using FrF2 library
## FrF2 creates a Fractional Factorial Designs with 2-Level Factors
library(FrF2)

# Our goal is to create a 3-factors full factorial design
# We specify the number of run (here 8 = 2^3) and the # of factors (here 3)
# Factor levels are fed for the third argument as shown bellow

design <- FrF2(8, 3,
               factor.names = list(T = c(160, 180),
                                   C = c(20, 40),
                                   K = c('A', 'B')),
               randomize = FALSE)

# Afetr running the experiments we insert the response 
# into the design matrix

Yield <- c(60, 72, 54, 68, 52, 83, 45, 80)
design <- add.response(design = design, response = Yield)
summary(design)

# For visualisation urposes we display the design

cube_plot <- ?cubePlot(Yield, T, C, K, main=paste("Cube Plot"),
                       cex.title=0.8,
                       cex.lab=1.0, 
                       cex.ax=par("cex.axis"), 
                       cex.clab=1.2,
                       size = .2)

# main effects plot
plot(design, cex = 1.0, cex.lab = 1.0, cex.axis = 0.9,
     main = "Main effects plot for MI", cex.main = 0.9)

MEPlot(design, abbrev = 5, cex.xax = 1.1, cex.main = 1.4)

# interaction plot
IAPlot(design, abbrev = 3, show.alias = TRUE, lwd = 2,
       cex = 2, cex.xax = 1.2, cex.lab = 1.5)

# linear model
# In order to carry out a ANOVA-type (main effects and interaction) analysis, 
# one needs sum contrasts in the linear model. (This is true for factors with 
# two levels, but does not generalize to factors with more levels)
contrasts(design$T) <- contr.sum(2)
contrasts(design$C) <- contr.sum(2)
contrasts(design$K) <- contr.sum(2)

# anova
df <- data.frame(design)
df$T = as.factor(df$T)
df$C = as.factor(df$C)

# lm model
lm.model <- lm(Yield ~ T * C * K, data = design)
round(summary(lm.model)$coefficients,2)
knitr::kable(summary(lm.model)$coefficients)

# install.packages("texreg")
require(texreg)
screenreg(list(lm.model), 
          custom.coef.names=c("(Intercept)", "T","K","C","T:K","T:C",
                              "K:C","T:K:C"))


# the 95% confidence interval for each effect estimate
knitr::kable(confint.lm(lm.model))

# graphica lmethods
# Normal and half normal effects plots of a simple design
par( mfrow = c(2,1) )
DanielPlot(lm.model, code = FALSE, half = TRUE, alpha = 0.05, autolab=TRUE,
           cex.main = 0.8, cex.pch = 0.9, cex.lab = 1.0, cex.fac = .8,
           cex.axis = 0.9, main="Daniel Plot")# half argument boolean

# Lenth
library(BsMD)
LenthPlot(lm.model, main = "Lenth Plot of Effects",
          cex.main = 0.8, cex.fac = .9,
          adj=1,
          limits = TRUE)

par( mfrow = c(1,1) )

halfnormal(coef(lm.model)[-1],alpha=.025)
LGB( coef(lm.model)[-1], rpt = TRUE )



# Optimisation : Optimum Operating Conditions

# update the linear model
library(stats)
lm.model <- update( lm.model, Yield ~ T + C + T : K, data = design)
summary(lm.model)

# since K is a categorical factor we isolate the -+ levels and study Yiel ~ T + C

# RSM
library(rsm)
summary(df)
df <- data.frame(design)
# since design is coding levels as factors
df$T = as.numeric(as.character(design$T))
df$C = as.numeric(as.character(design$C))

attach(df)

rsm <- rsm(Yield ~ FO(T, C, K) + TWI(T, C, K), data = df)
canonical(rsm)
summary(rsm)

# contourplot
par( mfrow=c(1,2) )
contour(rsm, ~ T + C, main='A', cex.main = 0.9,
        at=c(K='A'), image = TRUE, zlim = c(40,85),
        xlabs=c("Concentration","Temperature")) # colored: image = TRUE

contour(rsm, ~ T + C, main='B', cex.main = 0.9,
        at=c(xK='B'), image = TRUE, zlim = c(40,85),
        xlabs=c("Concentration","Temperature"))
par( mfrow=c(1,1) )


