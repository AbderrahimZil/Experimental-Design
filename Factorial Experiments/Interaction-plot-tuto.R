### R Studio Tutorial: Creating and Editing Interaction Plots ###

# Research Question: Is there a two-way interaction between men 
# and women employees in the amount of close friends they report 
# at work and their overall job satisfaction?

# Step 1: Upload dataset

# Data1 (insert angled bracket here)- read.csv(file.choose())

# Step 2: Examine dataset

head(Data1)

# Step 3: Run a standard "interaction.plot"
#interaction.plot(dataset$var1, dataset$var2, dataset$response)

interaction.plot(Data1$Friend, Data1$Sex, Data1$Job_Satisfaction)

# x.factor = "variable for your x-axis"
# trace.factor = "grouping variable"
# response = "variable for your y-axis"

with(Data1,{interaction.plot(x.factor = Friend, trace.factor = Sex, response = Job_Satisfaction)})

# This cleans up the labels slightly

# OR... you can remove the subheadings

with(Data1,{interaction.plot(Friend, Sex, Job_Satisfaction)})

# Step 4: Start editing your "interaction.plot"

# Additional options to change
# xlab = "label your x-axis"
# ylab = "label your y-axis"
# main = "title for your plot"
# ylim = "range of values along y-axis"
# trace.label = "label your legend"
# type = "puts markers on your plot"
# pch = "customize markers on your plot"
# col = "adds colour to your plot"
# fixed = "orders your factors based on your dataset"

interaction.plot(Data1$Friend, Data1$Sex, Data1$Job_Satisfaction, 
                 xlab = "Close Friends at Work", ylab = "Overall Job Satisfaction",                 main = "Employees' Overall Job Satisfaction and Close Friends at Work", ylim = c(1,10), trace.label = "Gender", type = "b", col=c("red","green"), pch = c(19,17), fixed = TRUE)

# Step 5: Properly label your legend and x-axis
# This is going to involve recoding your variables

# So create a copy of the dataset just so you do not overwrite the original

Data2 (insert angled bracket here)- Data1

library(car)

Data2$Sex (insert angled bracket here)- recode(Data2$Sex, '0 = "Male"; 1 = "Female";', as.factor.result = FALSE)

Data2$Friend (insert angled bracket here)- recode(Data2$Friend, '0 = "low"; 1 = "medium"; 2 = "high"; 3 = "very high";', as.factor.result = FALSE)

# Step 6: Rerun your "interaction.plot"

interaction.plot(Data2$Friend, Data2$Sex, Data2$Job_Satisfaction,  xlab = "Close Friends at Work", ylab = "Overall Job Satisfaction",                  main = "Employees' Overall Job Satisfaction and Close Friends at Work", ylim = c(1,10), trace.label = "Gender", type = "b", col=c("red","green"), pch = c(19,17), fixed = TRUE)

# Step 7: Reorder the labels on your x-axis
# Create a factor with four levels to reorder the labels that will display on the x-axis

x1  = factor(Data2$Friend, levels=c("low", "medium", "high", "very high"))

# Step 8: Run your finalized "interaction.plot"

interaction.plot(x1, Data2$Sex, Data2$Job_Satisfaction, xlab = "Close Friends at Work", ylab = "Overall Job Satisfaction", main = "Employees' Overall Job Satisfaction and Close Friends at Work",                  ylim = c(1,10), trace.label = "Gender", type = "b", col=c("red","green"),                  pch = c(19,17), fixed = TRUE)


require(graphics)

par(mfrow=c(1,1))

summary(esophNA)
attach(esophNA)

## deal with NAs:
esoph[66,] # second to last age group: 65-74
esophNA <- esoph; esophNA$ncases[66] <- NA
with(esophNA, {
  interaction.plot(agegp, alcgp, ncases/ncontrols, col = 2:5)
  # doesn't show *last* group either
  interaction.plot(agegp, alcgp, ncases/ncontrols, col = 2:5, type = "b")
  ## alternative take non-NA's  {"cheating"}
  interaction.plot(agegp, alcgp, ncases/ncontrols, col = 2:5,
                   fun = function(x) mean(x, na.rm = TRUE),
                   sub = "function(x) mean(x, na.rm=TRUE)")
})
rm(esophNA) # to clear up




install.packages("sciplot")
library(sciplot)
bargraph.CI(df$B, df$Taste, df$C)


data(ToothGrowth)
# One way design
bargraph.CI(x.factor = dose, response = len, data = ToothGrowth)

lineplot.CI(df$B, df$Taste, df$C, fun = function(x) mean(x, na.rm=TRUE), type = 'l')
