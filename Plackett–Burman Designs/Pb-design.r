library(FrF2)

des = pb(12) # run orders randomized
des <- des[,-(7:11)]
# subj11 has -1 -1 -1 -1 -1 -1 he's a control (does not get any treat)

factors =c('A', 'B', 'C', 'D', 'E', 'F')

# Generate significant effects D, F and F:E

set.seed(2)
yield = rnorm(12,0,0.5)
yield <- yield + 1*(des$D==-1) - 1.5*(des$F==-1) # adding MEff
yield <- yield + 1.25*((as.numeric(des$F)*as.numeric(des$E) ==2))# adding INTEFF
# as.numeric to not turn them into 1 and 2

# Analyze the MEFF
dat = cbind(des, yield=yield)
md = aov(yield ~ ., data=dat) # . stands for A+B+..+F
plot(dat$F, dat$yield)

summary(md)
# looking at the sum.sq we see the sigh of F and D
# E is not sigh this is cuz E is not correlated with EF which is sign
# all other MEFF are correlated with the INT EF.
# This EF sign kinda bleeds into other terms (MEFF)
# wa can witness a bit of significance in all the MEFF correlated with F:E
# namely A and B and C An D, this explains their sum.sq

## to look at the interaction terms --- md = aov(yield ~ (.)^2, data=dat) ---
# which does not work great cuz there is not enough dof to estimate all the INT
# instead it lists them alphabetically 
# AF is missing maybe cuz it's aliased with sum linear comb of the above terms
# since the not all the terms in the anova table are orthogonal
# we need to look at type2 and type3 anova since there is correlation

# fit a model with the best terms
mdx = aov(yield~(F+B+D)^2, data=dat)
summary(mdx)

# try interactionplot



## looking at the interaction plot 