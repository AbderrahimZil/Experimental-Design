# unit  Modified   Unmodified
# 1     16.85      16.62 
# 2     16.40      16.75 
# 3     17.21      17.37 
# 4     16.35      17.12 
# 5     16.52      16.98 
# 6     17.04      16.87 
# 7     16.96      17.34 
# 8     17.15      17.02 
# 9     16.59      17.08 
# 10    16.57      17.27 

# transform data to long format transformation

df <- data.frame(modified, unmodified)
library(tidyr)
mortar <- pivot_longer(data = df,
                     cols = modified:unmodified,
                     names_to = "Type",
                     values_to = "Strength")

# boxplot using the formula Y ~ X
boxplot(
  data = mortar, 
  Strength ~ Type,
  main = 'Strength per the mortar recipe',
  width=c(80, 80),
  boxwex = 0.25
  )

## Compliance with the assumptions
# Normality of the residuals
model = aov(Strength ~ Type, data = df.long)
qqnorm(y = model$residuals,
       pch = 5,
       frame = TRUE,
       main=c(paste('Q-Q Plot of the'),
              paste('model residuals')))
qqline(model$residuals, lwd = 2, col='red')

# checking normality of the residuals
# Shapiro-Wilk normality test on Strength
shapiro.test(model$residuals)

# homogeneity of variance
# Levene's test
library(car)
leveneTest(Strength ~ Type, data=df.long, center=mean)
summary(df.long)

## performing the test
t.test(data = df.long,
       Strength ~ Type,
       alternative = 'less',
       var.equal = TRUE)
