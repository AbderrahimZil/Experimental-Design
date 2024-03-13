library(shiny)
library(ggplot2)

# the dependent variables which are the pca scores
y1 <- pc$scores[,1]
y2 <- pc$scores[,2]

# independent variables which are the process controls
s <- X$substrate
q <- X$qlight
c <- X$cress

# build linear model for the scores after chicking its validity
lm1 <- lm(y1 ~ s * q + c)
lm2 <- lm(y2 ~ s * q )

## Shiny App
ui <- fluidPage(
  titlePanel("Growth Simulation Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Controls
      radioButtons(inputId = "S", label = "Substrate:", c('soil','cotton')),
      radioButtons(inputId = "L", label = "Light:", c('light','dark')),
      radioButtons(inputId = "C", label = "Cress:", c('plain','curled'))
    ),
    mainPanel(
      plotOutput("predictionPlot")
    )
  )
)

server <- function(input, output) {
  predictions <- reactive({
    generatePredictions(input$S, input$L, input$C)
  })
  
  generatePredictions <- function(S, L, C) {
    point = data.frame(s = S, q = L, c = C)
    
    pcs1 <- predict(lm1, point)
    pcs2 <- predict(lm2, point)
    
    growth_curve <- mean.function + pc.functions[,1] * pcs1 + pc.functions[,2] * pcs2
    
    return(growth_curve)
  }
  
  output$predictionPlot <- renderPlot({
    ggplot(data.frame(X = seq_along(predictions()), Predictions = predictions()), aes(x = X, y = Predictions)) +
      geom_line(color = "blue") +
      labs(title = "Predicted Growth",
           x = "Days", y = "Growth in cm")
  })
}

shinyApp(ui = ui, server = server)
