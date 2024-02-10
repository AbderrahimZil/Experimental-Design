rm(list = ls())
library(rsm)

df <- read.csv("data.txt")

## to coded variables
CR <- coded.data(df,
                 x1 ~ (F1 - 85)/5,
                 x2 ~ (F2 - 175)/5)

### First Response Analysis

# We start by fitting a second order model and analyze the summary.
# Looking at the interaction x1:x2 pvalue, we can conclude that 
# the TWI is not significant.

modelY1 <- rsm(Y1 ~ SO(x1,x2), data = CR)
#|            |   Estimate| Std. Error|    t value| Pr(>&#124;t&#124;)|
#|:-----------|----------:|----------:|----------:|------------------:|
#|(Intercept) | 79.9399546|  0.1190886| 671.264432|          0.0000000|
#|x1          |  0.9950503|  0.0941549|  10.568222|          0.0000148|
#|x2          |  0.5152028|  0.0941549|   5.471862|          0.0009340|
#|x1:x2       |  0.2500000|  0.1331451|   1.877650|          0.1025192|
#|x1^2        | -1.3764493|  0.1009842| -13.630347|          0.0000027|
#|x2^2        | -1.0013360|  0.1009842|  -9.915772|          0.0000226|

## Multiple R-squared:  0.9827,	Adjusted R-squared:  0.9704 


# We update the model by removing the two way interaction term
# from looking at the regression metrics for the SO model and 
# the updated one we witness a drop in both R� and adjR�

updatedmodelY1 <- rsm(Y1 ~ FO(x1,x2) + PQ(x1,x2), data = CR)

#|            |   Estimate| Std. Error|    t value| Pr(>&#124;t&#124;)|
#|:-----------|----------:|----------:|----------:|------------------:|
#|(Intercept) | 79.9399546|  0.1365992| 585.215469|          0.0000000|
#|x1          |  0.9950503|  0.1079993|   9.213489|          0.0000156|
#|x2          |  0.5152028|  0.1079993|   4.770428|          0.0014078|
#|x1^2        | -1.3764493|  0.1158327| -11.883082|          0.0000023|
#|x2^2        | -1.0013360|  0.1158327|  -8.644675|          0.0000249|

## Multiple R-squared:  0.974,	Adjusted R-squared:  0.961 

# WE THEN CONCLUDE TO RETAIN THE SECONDE ORDER MODEL FOR MODELING Y1. 

### Second Response Analysis
modelY2 <- rsm(Y2 ~ SO(x1, x2), data = CR)
#|            |   Estimate| Std. Error|    t value| Pr(>&#124;t&#124;)|
#|:-----------|----------:|----------:|----------:|------------------:|
#|(Intercept) | 70.0002112|  1.0173081| 68.8092512|          0.0000000|
#|x1          | -0.1552734|  0.8043134| -0.1930509|          0.8524009|
#|x2          | -0.9483932|  0.8043134| -1.1791339|          0.2768648|
#|x1:x2       | -1.2500000|  1.1373851| -1.0990121|          0.3081185|
#|x1^2        | -0.6873222|  0.8626518| -0.7967551|          0.4517659|
#|x2^2        | -6.6891348|  0.8626518| -7.7541535|          0.0001112|

## Multiple R-squared:  0.8997,	Adjusted R-squared:  0.8281 

# FROM THE COEFFICIENTS ESTIMATES ONLY x2^2 IS SIGNIFICANT.
# GIVEN THE INHERITANCE PRINCIPALE, x2 SHOULD BE INCLUDED IN THE MODEL.

updatedmodelY2 <- rsm(Y2 ~ FO(x1, x2) + PQ(x1, x2), data = CR)

#|            |   Estimate| Std. Error|    t value| Pr(>&#124;t&#124;)|
#|:-----------|----------:|----------:|----------:|------------------:|
#|(Intercept) | 70.0002112|  1.0173081| 68.8092512|          0.0000000|
#|x1          | -0.1552734|  0.8043134| -0.1930509|          0.8524009|
#|x2          | -0.9483932|  0.8043134| -1.1791339|          0.2768648|
#|x1:x2       | -1.2500000|  1.1373851| -1.0990121|          0.3081185|
#|x1^2        | -0.6873222|  0.8626518| -0.7967551|          0.4517659|
#|x2^2        | -6.6891348|  0.8626518| -7.7541535|          0.0001112|

## Multiple R-squared:  0.8824,	Adjusted R-squared:  0.8236 

### Third Response Analysis
modelY3 <- rsm(Y3 ~ SO(x1, x2), data = CR)
#|            |   Estimate| Std. Error|    t value| Pr(>&#124;t&#124;)|
#|:-----------|----------:|----------:|----------:|------------------:|
#|(Intercept) | 3375.97523|   77.06576| 43.8064215|          0.0000000|
#|x1          |  205.12597|   60.93044|  3.3665601|          0.0119765|
#|x2          |  177.36678|   60.93044|  2.9109718|          0.0226292|
#|x1:x2       |  -80.00000|   86.16214| -0.9284820|          0.3840565|
#|x1^2        |  -41.74373|   65.34984| -0.6387733|          0.5432972|
#|x2^2        |   58.28648|   65.34984|  0.8919146|          0.4020561|

## Multiple R-squared:  0.759,	Adjusted R-squared:  0.5868 

updatedmodelY3 <- rsm(Y3 ~ FO(x1, x2), data = CR)

#|            |  Estimate| Std. Error|   t value| Pr(>&#124;t&#124;)|
#|:-----------|---------:|----------:|---------:|------------------:|
#|(Intercept) | 3386.1538|   45.93569| 73.715096|          0.0000000|
#|x1          |  205.1260|   58.56117|  3.502764|          0.0057001|
#|x2          |  177.3668|   58.56117|  3.028744|          0.0127044|

## Multiple R-squared:  0.682,	Adjusted R-squared:  0.6184

par(mfrow=c(1,2))

contour(modelY1, ~x1+x2,
        col = "blue",
        xlabs=c("Time (min)", "Temperature (�C)"))

persp(modelY1, ~x1+x2,
      xlabs=c("Time (min)", "Temperature (�C)"),
      expand = 1.5,
      contours = TRUE
      )
###
contour(modelY2, ~x1+x2,
        col = "red",
        xlabs=c("Time (min)", "Temperature (�C)"))

persp(modelY2, ~x1+x2,
      xlabs=c("Time (min)", "Temperature (�C)"),
      expand = 1.5,
      contours = TRUE
      )

### 
contour(modelY3, ~x1+x2,
        col = "green",
        xlabs=c("Time (min)", "Temperature (�C)"))

persp(modelY3, ~x1+x2,
      xlabs=c("Time (min)", "Temperature (�C)"),
      expand = 1.5,
      contours = TRUE
      )


### Overlayed Contours

par(mfrow=c(1,1))
contour(modelY1, ~x1+x2,
        levels= c(78.5),
        col = "blue",
        xlabs=c("Time (min)", "Temperature (�C)"))
###
contour(modelY2, ~x1+x2,
        levels= c(68),
        col = "red",
        xlabs=c("Time (min)", "Temperature (�C)"),
        add=TRUE)

### 
contour(modelY3, ~x1+x2,
        levels= c(3400),
        col = "green",
        xlabs=c("Time (min)", "Temperature (�C)"),
        add=TRUE)