battery <- read.csv('Battery_Design.csv')
summary(battery)
battery$Type = factor(battery$Type)

# Check for the completeness and the balance of the design
with(battery, table(Temperature, Type) ) #balanced design, equal units # per cell           

# boxplot
p <- ggboxplot(battery,
               x = 'Temperature',
               y = 'Life',
               color = 'Type',
               palette = "jco",
               main='',
               xlab = 'Temperature (°F)',
               ylab = 'Life (Hr)')
ggpar(p,
      legend = "right", legend.title = "Material Type",
      font.legend = c(10, "bold", "black"))


# Credits to: Jon B Stats and Psych Youtube channel
# interaction between temperature and type
# Additional options to change
# xlab = "label your x-axis"
# ylab = "label your y-axis"
# main = "title for your plot"
# ylim = "range of values along y-axis"
# trace.label = "label your legend"
# type = "puts markers on your plot"
# pch = "customize markers on your plot"
# col = "adds color to your plot"
# fixed = "orders your factors based on your dataset"
with(data = battery, {interaction.plot(x.factor = Temperature,
                                       trace.factor = Type,
                                       response = Life,
                                       type = "b",
                                       pch = c(24,18,22),
                                       fixed = TRUE,
                                       leg.bty = "o",
                                       main = "Interaction Plot of Temperature and Material Type",
                                       xlab = "Temperature (°F)",
                                       trace.label = 'Material Type',
                                       ylab = "Life (h)")})


# Credits to: Jon B Stats and Psych Youtube channel
# interaction between temperature and type
# Additional options to change
# xlab = "label your x-axis"
# ylab = "label your y-axis"
# main = "title for your plot"
# ylim = "range of values along y-axis"
# trace.label = "label your legend"
# type = "puts markers on your plot"
# pch = "customize markers on your plot"
# col = "adds color to your plot"
# fixed = "orders your factors based on your dataset"
with(data = battery, {interaction.plot(x.factor = Temperature,
                                       trace.factor = Type,
                                       response = Life,
                                       type = "b",
                                       pch = c(24,18,22),
                                       fixed = TRUE,
                                       leg.bty = "o",
                                       main = "Interaction Plot of Temperature and Material Type",
                                       xlab = "Temperature (°F)",
                                       trace.label = 'Material Type',
                                       ylab = "Life (h)")})

## Analysis Of Variance 

# code as factors for anova analysis
battery$Type = factor(battery$Type)
battery$Temperature = factor(battery$Temperature)

## Build the linear model
anova.model = aov(Life ~ Type * Temperature, data = battery)
summary(anova.model)

# Model adequacy checking

par(mfrow=c(2,2))
plot(anova.model)
par(mfrow=c(1,1))


# It might be the case that the experimenter wants to compare the performance between material types 
# at specific temperature levels; at  T=15(°F) ,  T=70(°F)  and  T=125(°F) . The experimenter might be 
# interested as well in each type performance at different temperature levels  Type1 ,  Type2  and  Type3 .

library(emmeans)
library(multcomp)
summary(glht(anova.model, emm(pairwise ~ Type * Temperature)), test = adjusted(type = "holm"))


## Linear Model

# Since Temperature is a quantitative factor, one might be interested in the battery life change over the range of temperature.
# To study Temperature effect we can compute a linear model of of the two independent variables Temperature and material Type.

df <- battery
df$Type = factor(df$Type)
lm.fit <- lm(Life ~ Temperature * Type, data = df)
summary(lm.fit)


# Plotting
interact_plot(lm.fit, pred = Temperature,
               modx = Type, plot.points = TRUE,
               interval = FALSE, data = battery)