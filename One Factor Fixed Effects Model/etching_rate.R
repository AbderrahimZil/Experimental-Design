
## df 
#|power |  Y1|  Y2|  Y3|  Y4|  Y5|
#|:-----|---:|---:|---:|---:|---:|
#|160   | 575| 542| 530| 539| 570|
#|180   | 565| 593| 590| 579| 610|
#|200   | 600| 651| 610| 637| 629|
#|220   | 725| 700| 715| 685| 710|


df$power = factor(df$power)
knitr::kable(df)

## transform to long format
library(reshape2)
df <- melt(df, id.vars = 'power',
           measure.vars = c("Y1", "Y2", "Y3", "Y4", "Y5" ),
           value.name = "rate")

# visualize
boxplot(rate ~ power,
        data = df,
        xlab = 'Power (W)',
        ylab = 'Etch rate (�/min)')

# EDA
library(plyr)
ddply(df, ~power, function(data) summary(data$rate))

# Anova
anova.model = aov(rate ~ power , data = df)
anova(anova.model)

# assumption checking
par(mfrow=c(2,2))
plot(anova.model)
par(mfrow=c(1,1))


## Posthoc tests
# One would like to ...
library(multcomp)
mc = glht(model = anova.model, linfct = mcp(power = "Tukey"))
summary(mc, test=adjusted(type="holm"))

#Linear Hypotheses:
  #Estimate Std. Error t value Pr(>|t|)    
  #180 - 160 == 0    36.20      11.55   3.133  0.00925 ** 
  #200 - 160 == 0    74.20      11.55   6.422 2.53e-05 ***
  #220 - 160 == 0   155.80      11.55  13.485 2.24e-09 ***
  #200 - 180 == 0    38.00      11.55   3.289  0.00925 ** 
  #220 - 180 == 0   119.60      11.55  10.352 8.47e-08 ***
  #220 - 200 == 0    81.60      11.55   7.063 1.07e-05 ***


## regression model
# the experimenter might be interested in knowing ....
linear_fit <- lm(rate ~ poly(power, 2), data = df)
summary(linear_fit)


# predictions
effect_plot(linear_fit, pred = power, interval = TRUE,
            int.type = "confidence", int.width = .8, data = df,
            plot.points = TRUE,
            x.label = 'Power (W)',
            y.label = 'Etch rate (�/min)') # note data arg