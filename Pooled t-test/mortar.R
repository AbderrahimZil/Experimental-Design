# unit  modified   unmodified
# 1     16.85      17.50
# 2     16.40      17.63
# 3     17.21      18.25
# 4     16.35      18.00
# 5     16.52      17.86
# 6     17.04      17.75
# 7     16.96      18.22
# 8     17.15      17.90
# 9     16.59      17.96
# 10    16.57      18.15

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

df
par(mfrow=c(1,2))
hist(df.long[df.long$Type=='modified',]$Hardness, breaks = 4)
hist(df.long[df.long$Type=='unmodified',]$Hardness)
qqplot(df.long[df.long$Type=='modified',]$Hardness)

dat = df.long[df.long$Type=='modified',]$Hardness
hist(dat, breaks = seq(min(dat), max(dat), length.out = 11))
dat1 = df.long[df.long$Type=='unmodified',]$Hardness

hist(dat1, breaks = seq(min(dat1), max(dat1), length.out = 11))

# Q-Q plot
attach(df.long)
par(mfrow=c(1,2))
qqnorm(y = Strength[Type == 'modified'],
       pch = 5,
       frame = TRUE,
       main=c(paste('Q-Q Plot of the'),
              paste('modified mortar Strength data')))
qqline(Strength[Type == 'modified'], lwd = 2, col='red')

qqnorm(y = Strength[Type == 'unmodified'],
       pch = 5,
       frame = TRUE,
       main=c(paste('Q-Q Plot of the'),
              paste('unmodified mortar Strength data')))
qqline(Strength[Type == 'unmodified'], lwd = 2, col='red')

# Shapiro-Wilk normality test on Hardness
shapiro.test(model$residuals)
shapiro.test(Strength[Type == 'unmodified'])


> ggplot(df.long, aes(x=Hardness, fill=Type)) + 
  +   geom_histogram(aes(y=..density..), position="identity", alpha=0.7, binwidth = .1)+
  +   theme_classic() +
  +   geom_density(alpha=0.6) + 
  +   scale_fill_brewer()

t.test(Strength ~ Type, data = df.long, var.equal = TRUE, alternative = 'one.sided')

library(car)
leveneTest(Hardness ~ Type, data=df.long, center=mean) # Levene's test
summary(df.long)

t.test(data = df.long,
       Strength ~ Type,
       alternative = 'less',
       var.equal = TRUE)

model = aov(Strength ~ Type, data = df.long)
model$fitted.values

qqnorm(y = model$residuals,
       pch = 5,
       frame = TRUE,
       main=c(paste('Q-Q Plot of the'),
              paste('model residuals')))
qqline(model$residuals, lwd = 2, col='red')

