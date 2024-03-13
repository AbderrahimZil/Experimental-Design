rm(list = ls())
library(fda)

cd <- read.csv('growth-data.csv')
t = c(0,1,2,3,5,7)
x = seq(0,7, length.out = 51)

cols <- paste("day", c(0, 1, 2, 3, 5, 7), sep = "")
pots <- paste("pot_", 1:12, sep = "")
d <- t(as.matrix(cd[,cols]))

splines <- create.bspline.basis(rangeval = c(0, max(t)),nbasis = 10,norder = 3)
fd <- Data2fd(d,argvals = t,basisobj = splines,lambda = 0.08)
growth <- smooth.basisPar(t, d, fdobj= splines, lambda=0.08) # the result is a fdSmooth object

smoothed.curves = matrix(0,nrow=length(x),ncol=12)
for (i in 1:12) {
  temp.smooth = smooth.basisPar(t, d[,i], fdobj= splines, lambda=0.08)
  smoothed.curves[,i] = eval.fd(seq(0,7,length.out = 51),temp.smooth$fd)
}

#Generate a Single Functional Data Object For Use in FPCA
Overall = smooth.basisPar(x, smoothed.curves, fdobj= splines, lambda=0.08)
pc = pca.fd(Overall$fd,nharm=3) #Perform FPCA and Store Top 2 PC Functions

pc_scores = data.frame(pc$scores)
names(pc_scores) <- c('PC1', 'PC2')
rownames(pc_scores) <- paste("Run/Pot ", 1:12, sep = "")
#Store the Mean Function and PC Functions (Karhunen-Loeve Expansion Building Blocks)
mean.function = as.vector(eval.fd(seq(0,7),pc$meanfd))
pc.functions = eval.fd(seq(0,7),pc$harmonics) #When Used in this Way, Empirical Basis Functions
#Store the Mean Function and PC Functions (Karhunen-Loeve Expansion Building Blocks)
mean.function = as.vector(eval.fd(x,pc$meanfd))
pc.functions = eval.fd(x,pc$harmonics) 

X <- cbind(cd[, c('substrate', 'qlight', 'cress')], pc$scores[,1], pc$scores[,2])
names(X) <- c('substrate', 'qlight',  'cress', 'Y1', 'Y2')

y1 <- pc$scores[,1]
y2 <- pc$scores[,2]

s <- X$substrate
q <- X$qlight
c <- X$cress

y1 <- X$Y1
y2 <- X$Y2

#anova.mod = anova(lm(Y ~ substrate * qlight * cress, data = X))
aov.mod = aov(Y2 ~ substrate * qlight + cress, data = X)
summary(aov.mod)

lm1 <- lm(y1 ~ s * q + c)
lm2 <- lm(y2 ~ s * q )

library(shiny)
library(ggplot2)

lm1 <- lm(y1 ~ s * q + c)
lm2 <- lm(y2 ~ s * q )

# UI code
ui <- fluidPage(
  titlePanel("Growth Simulation Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Input controls for user-defined levels
      radioButtons(inputId = "S", label = "Substrate:", c('soil','cotton')),
      radioButtons(inputId = "L", label = "Light:", c('light','dark')),
      radioButtons(inputId = "C", label = "Cress:", c('plain','curled'))
    ),
    mainPanel(
      plotOutput("predictionPlot")
    )
  )
)

# Server code
server <- function(input, output) {
  # Reactive expression for generating predictions
  predictions <- reactive({
    # Generate predictions based on user-defined levels
    generatePredictions(input$S, input$L, input$C)
  })
  
  # Function to generate predictions based on levels
  generatePredictions <- function(S, L, C) {
    point = data.frame(s = S, q = L, c = C)
    
    pcs1 <- predict(lm1, point)
    pcs2 <- predict(lm2, point)
    
    growth_curve <- mean.function + pc.functions[,1] * pcs1 + pc.functions[,2] * pcs2
    
    return(growth_curve)
  }
  
  # Render plot based on reactive predictions
  output$predictionPlot <- renderPlot({
    ggplot(data.frame(X = seq_along(predictions()), Predictions = predictions()), aes(x = X, y = Predictions)) +
      geom_line(color = "blue") +
      labs(title = "Predicted Growth",
           x = "Days", y = "Growth in cm")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
