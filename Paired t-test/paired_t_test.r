# hardness data
# tip1 tip2
# 1     7    6
# 2     3    3
# 3     3    5
# 4     4    3
# 5     8    8
# 6     3    2
# 7     2    4
# 8     9    9
# 9     5    4
# 10    4    5

# add unit column
hardness$specimen = 1:nrow(hardness)

# reorder columns 
hardness <- hardness[, c("specimen", "tip1", "tip2")]

# to long format **paired**
library(tidyr)
hardness <- pivot_longer(data = hardness,
                         cols = tip1:tip2,
                         names_to = "Tip",
                         values_to = "Hardness")

hardness$specimen = factor(hardness$specimen)
summary(hardness)
hardness
attach(hardness)

boxplot( Hardness ~ Tip, data = hardness )
library(ggpubr)

t.test(Hardness[Tip == 'tip2'], Hardness[Tip == 'tip1'], paired= TRUE, var.equal = TRUE)

qqnorm(Hardness[Tip == 'tip2']); qqline(Hardness[Tip == 'tip2'])
## Ends here###########################################################