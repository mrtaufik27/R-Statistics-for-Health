# praktek hari kedua

library(psych)
describe
describeBy

# 1 homogeneity of variance / homoscedasticity, H1, CI level
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
# F test untuk 2 variance
var.test(len ~ supp, data = ToothGrowth)
var.test(len ~ supp, data = ToothGrowth, alternative="two.side", conf.level=0.95)
var.test(len ~ supp, data = ToothGrowth, alternative="less", conf.level=0.95)
var.test(len ~ supp, data = ToothGrowth, alternative="greater", conf.level=0.95)

?var.test

# Bartletts's test, many variances
res <- bartlett.test(len ~ supp, data = ToothGrowth)  #untuk satu independent variable
res
res <- bartlett.test(len ~ interaction(supp,dose), data=ToothGrowth)   #untuk dua atau lebih independent variable
res

# levene's test
library(car)
LeveneTest(dat$len ~ dat$supp)
LeveneTest(dat$len ~ interaction(dat$supp,dat$dos))

# Fligner-Killeen's test
fligner.test(dat$len ~ dat$supp)
fligner.test(dat$len ~ interaction(dat$supp,dat$dos))


?t.test(, conf.level=.95)  # jika normal
wilcox.test() # jika tidak normal

library(lsr)
?pairwise.t.test()  # to repidly detect all possible comparison if has > 2 groups



# post hoc test
mod <- aov(len~dos, data=dat)
mod
summary(mod)
par(mfrow=c(2,2))
plot(mod)

# tukey HSD
TukeyHSD(mod, conf.level=0.95)
#windows()
plot(TukeyHSD(mod , conf.level=0.95), las=1, col="blue")

# jika variance tidak constant / tidak homogen maka bisa menggunakan
library(rstatix)
welch_anova_test()
# atau games-howell post hoc test, atau pairweise t test (tanpa asumsi)
games_howell_test()
pairwise_t_test()


pairwise.t.test()

# menggunakan package DescTools
library(DescTools)
PostHocTest(mod)
?PostHocTest

# checking assumption
# http://rstudio-pubs-static.s3.amazonaws.com/308410_2ece93ee71a847af9cd12fa750ed8e51.html
# 1 by plotting the model -> checking variance is constant
# 2 normality test
shapiro.test()     #shapiro wilks test
ks.test()         #kolmogorov-Smirnov test, need mean and sd
# 3 outlier
library(rstatix)
identify_outliers()

# non parametric test
kruskal.test(len~dos, data=dat)   #kruskal test

library(DescTools) 
DunnTest(len~dos, data=dat, method="holm", conf.level=.99)      #dunnet test
DunnTest(len~dos, data=dat, alpha=0.01)      

# Bonferonni  for pairwise comparisons
# "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
pairwise_t_test(len~dos, data=dat, p.adjust.method = "bonferroni")

?pairwise_t_test

