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
