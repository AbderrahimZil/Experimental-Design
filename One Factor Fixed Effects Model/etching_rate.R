# setting the design for for one factor design
# long.format data
set.seed(7638)
f <- factor( rep( c(160,180,200,220), each = 5)) # five replicates are collected
rate <- sample( f, 20 ) # randomize
order <- 1:20
plan <- data.frame( order=order, rate=strength )

# wide format
set.seed(7638)
levels <- factor( c(160,180,200,220) ) # five levels are set
plan <- data.frame(level=levels)
# write.csv( plan, file = "wide_one_factor.csv", row.names = FALSE)
# now go and fill the data

# trans.factor <- factor (rep (c("t0", "t12", "t24", "t72"), c(4,3,3,3)))

# replicates <- 5
# Ys <- data.frame(matrix(NA, nrow = length(levels), ncol = replicates))
# data.frame( Ys )
# data.frame( levels=levels)
# bind(data.frame( levels=levels), data.frame( Ys ))

#names <- c("v","u","w")
#df <- data.frame(names)
#for (k in names) df[[k]]<-as.numeric()
#df

df <- read.csv("plasma_etching.csv")
str(df)

df 
#|power |  Y1|  Y2|  Y3|  Y4|  Y5|
#|:-----|---:|---:|---:|---:|---:|
#|160   | 575| 542| 530| 539| 570|
#|180   | 565| 593| 590| 579| 610|
#|200   | 600| 651| 610| 637| 629|
#|220   | 725| 700| 715| 685| 710|


df$power = factor(df$power)
knitr::kable(df)

#???install.packages("reshape2")
library(reshape2)
df <- melt(df, id.vars = 'power',
           measure.vars = c("Y1", "Y2", "Y3", "Y4", "Y5" ),
           value.name = "rate")

df <- df[-2] # delete measure category Y1 ... Y5
summary(df)

boxplot(rate ~ power,
        data = df,
        xlab = 'Power (W)',
        ylab = 'Etch rate (Å/min)')
# the strength keeps increasing as as content content increases up to 30 %(si) (level)
# beyond 30% there is a marked decrease in tensil strength

# we wich to test for differences between the mean trength at all the 4 levels of Power 
# dot plot
require(ggplot2)
qplot(df$power,
           df$rate,
           xlab = 'Power (W)', ylab = 'Etch rate (Å/min)') + theme_classic()


p <- ggplot(df, aes(x=power, y=rate)) +
  geom_dotplot(binaxis='y', stackdir='center') +
  theme_classic()
print(p +labs( title= "", y="Etch rate (Å/min)", x = "Power (W)"))



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


# Posthoc tests
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
  
plot(TukeyHSD(anova.model, "power"))
confint(mc)

plot(mc, main = "",
     xlim=c(0,160),
     ylim = c(0.5, 6.0),
     las=3) # las=2 for x axis


## regression model
# the experimenter might be interested in knowing ....
df$power = as.numeric(as.character(df$power))
linear_fit <- lm(rate ~ poly(power, 2), data = df)
summary(linear_fit)


# prediction
library(jtools)
effect_plot(linear_fit, pred = power, interval = TRUE,
            int.type = "confidence", int.width = .8, data = df,
            plot.points = TRUE,
            x.label = 'Power (W)',
            y.label = 'Etch rate (Å/min)') # note data arg