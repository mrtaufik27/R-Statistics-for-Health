setwd("C:/Users/USER/Google Drive/04. Lain-lain/sadasa academy/basic R")
dat <- read.csv("ATC_ToothGrowth.csv", sep=",")
str(dat)

ToothGrowth

data()    # command untuk melihat dataset yang available di R

?ToothGrowth
help("ToothGrowth")

# DESCRIPTIVE ANALYSIS
summary(dat) # memberikan hasil terkait dg data
mean(dat$len)
sd(dat$len)
var(dat$len)

unique(dat$dose)
table(dat$dose) # frequency

dat$dos <- as.factor(dat$dose)
str(dat)

View(dat)
# Data Visualization
# pie chart / diagram lingkaran
x <- c(21, 62, 10, 53)
labels <- c("Indonesia", "Malaysia", "Singapore", "Thailand")

pie(x, labels)
?pie

pie(dat$len,dat$dos)
pie(dat$len,dat$supp)

pie(dat$dos) # error
pie(table(dat$dos))
pie(table(dat$dos), labels=c("dosis 0.5","dosis 1", "dosis 2") )
pie(table(dat$dos), labels=c("dosis 0.5","dosis 1", "dosis 2"), col=c("red", "blue", "green"))
pie(table(dat$dos), labels=c("dosis 0.5","dosis 1", "dosis 2"), col=c(1, 4, 7), main="Dose")

levels(dat$dos)
levels(dat$dos) <- c("Dosis 1", "Dosis 2", "Dosis 3")

# legend
lg <- c("dosis 1", "dosis 2", "dosis 3")
legend("topright", leg=lg)
legend("topright", leg=lg, col=c(1,4,7), title="Dosis",
       lwd=3)

?legend


# barplot

barplot(dat$supp) # error
barplot(table(dat$supp), main="supplemen gigi", col=c(2,3,5))

# hist
hist(dat$len, main="histogram data panjang gigi", col=6)

# boxplot
boxplot(dat$len, main="histogram data panjang gigi", col=6)

# plot
plot(dat$len, dat$supp)
plot(dat$len, dat$supp, ylab="supplement")
plot(dat$len, dat$supp, ylab="supplement", las=1, xlab="tooth lenght")

?plot


# UJI Hubungan atau correlation

cor()
?cor

df <- mtcars
?mtcars
str(df)
cor(df$wt,df$mpg, method="pearson")

# uji normalitas

shapiro.test(dat$len) # panjang gigi babi

# uji beda

# t test
t.test(dat$len, mu=15)  # independent t test
t.test(dat$len~dat$supp) # t test dengan tidak berpasangan
t.test(dat$len~dat$supp, paired=T) # t test dengan berpasangan

t.test(tinggi_badan~gender) # t test dengan tidak berpasangan


# anova
mod <- aov(dat$len~dat$dos)
summary(mod)

# regresion
mod <- lm(dat$len~dat$dos+dat$supp)
summary(mod)
plot(mod)

windows()
plot(mod)
qqnorm(mod$residuals, col=4, las=1)
qqline(mod$residuals, col=9)

mod$coefficients

?lm
