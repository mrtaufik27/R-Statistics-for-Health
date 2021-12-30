# Prakter hari kedua Analytica R for Stats
setwd("C:/Users/USER/Google Drive/04. Lain-lain/sadasa academy/Statistics for Health")

data <- read.csv("whiteness.csv", sep=";")
str(data)
summary(data)

library(tidyverse) # untuk data manipulation
library(ggpubr)   # untuk data visualization
library(rstatix)  # untuk useful function

# merubah urutan, menentukan reference group
levels(data$Education)
data <- reorder_levels(data,Education, order = c("TK","SD"))


######################
# DESCRIPTIVE ANALYSIS
######################

summary(data)

# lebih complex nya dengan klasifikasi per group
data %>%
  group_by(Toothpaste) %>% 
  get_summary_stats(Whiteness)

# data visualization
ggboxplot(data, x="Toothpaste", y="Whiteness")


#################
# check assumption
#################

# finding outliers
data %>%
  group_by(Toothpaste) %>%
  identify_outliers(Whiteness)

# omit outliers
data[data$Whiteness>20 & data$Toothpaste=="T1",]
dat <- data[-5,]

dat %>%
  group_by(Toothpaste) %>%
  identify_outliers(Whiteness)

data[data$Whiteness==19 & data$Toothpaste=="T1",]
dat <- data[-(c(4,5)),]

# normality test
dat %>%
  group_by(Toothpaste)%>%
  shapiro_test(Whiteness)

# visualized the normality each group
ggqqplot(data, "Whiteness", facet.by = "Toothpaste")

# normality of residual distributions
mod <- aov(data=dat, Whiteness~Toothpaste)
ggqqplot(mod$residuals)
shapiro.test(mod$residuals)
shapiro_test(mod$residuals)

# homogeneity of variance
par(mfrow=c(2,2))
plot(mod)      # dilihat dari residual vs fitted value, apakah variance nya constant?

# cara lain menggunakan levene's test
dat %>%
  levene_test(Whiteness~Toothpaste)

levene_test(Whiteness~Toothpaste, data=dat)


################
# Analysis model
################

dat$WGroup <- ifelse(dat$Whiteness>=17.5, "tinggi", "rendah")
str(dat)
dat$WGroup <- as.factor(dat$WGroup)

# Chi-Square, Risk Estimate	Oral_Health_Knowledge_Group	Sex
chisq.test(dat$Sex,dat$WGroup)

# contingency table
ct <- table(dat$Sex,dat$WGroup)
ct

# odd ratio
fisher.test(ct)

# cara lain
library(epiDisplay)
cc(dat$Sex, dat$WGroup)
#  It gave the warning because many of the expected values will be very small and therefore the approximations of p may not be right.
chisq.test(dat$Sex, dat$WGroup,simulate.p.value = TRUE)

# identify cofounding with Mantel Haenzel Ratio
mhor(dat$Sex, dat$WGroup, dat$Education)

#######
# correlation test
#######
# pearson
cor(dat$Oral_Health_Knowledge, dat$OHIS_Score,method="pearson")
# spearman 
cor(dat$Oral_Health_Knowledge, dat$OHIS_Score, method="spearman")
# kendall
cor(dat$Oral_Health_Knowledge, dat$OHIS_Score, method="kendall")

# contingency table
ct <- table(dat$Education,dat$WGroup)
ct
prop.table(ct)
prop.table(ct)*100 # untuk mendapatkan percentage


# chi square
chisq.test(dat$Education, dat$WGroup)

#Apparently, they're not, but we also got the Chi-squared approximation may be incorrect warning. This is because chi-squared statistic follows chi-squared distribution only approximately. The more observations we have, the better approximation is. chisq.test function throws the above warning whenever one of the expected counts is lower than 5 (for what an "expected count" is, see tutorial linked above).
# odd ratio
fisher.test(ct)

##################
# regression model
##################

# linear regression
mod1<-lm(dat$Whiteness~., data=dat)			# Creating linear model
summary(mod1)
# untuk menentukan best fitted model
bm <- step(mod1, direction = "both")  #both, backward, atau "forward"



# logistics regression
levels(dat$WGroup)
dat$WGroup1 <- ifelse(dat$WGroup=="rendah", 0, 1)
mod2<-glm(dat$WGroup1~., data=dat, family=binomial)			# Creating linear model
summary(mod2)
drop1(mod2,test="Chisq")
logistic.display(mod2)
lroc(mod2)

# ordinal logistics regression
m <- polr(dat$WGroup1~., data=dat, Hess=TRUE)  # Hess True (optional) untuk memperlihatkan 

# mutinom regression
library("nnet")
#test <- multinom(y ~ x1 + x2, data = data)





#note
#cara relevel
ml$prog2 <- relevel(ml$prog, ref = "academic")
library("foreign")
ml <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")

#Now we'll calculate Z score and p-Value for the variables in the model.
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
exp(coef(test))

