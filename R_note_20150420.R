install.packages("base")

dt <- read.csv(file.choose())
dt[ , c(2,5)]
subset(dt, Science >= 60)

meandt <- tapply(dt$Science, dt$Group, mean)
meandt

meandt <- data.frame(meandt)
meandt

meandt <- cbind(meandt, rownames(meandt))
meandt

meandt <- cbind(meandt, "sd" = 1:length(meandt[,1]))
meandt

group <- rownames(meandt)
group

for (i in 1:length(meandt[,1]))
  {meandt[i,3] <- mean(subset(dt, Group == group[i])$Science)}

for (i in 1:length(meandt[,1]))
  {meandt[i,4] <- sd(subset(dt, Group == group[i])$Science)}

meandt[,5] <- tapply(dt$Science, dt$Group, length)

t.test(Science ~ Gender, data = dt, var.equal = T)

Two Sample t-test

data:  Science by Gender
t = -1.0211, df = 7, p-value = 0.3412
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -47.5797  18.8797
sample estimates:
  mean in group f mean in group m 
64.40           78.75 


t.test(dt$Literature, dt$Science, pair = T)

Paired t-test

data:  dt$Literature and dt$Science
t = -4.2126, df = 8, p-value = 0.002945
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -17.193365  -5.028857
sample estimates:
  mean of the differences 
-11.11111 

fit <- aov(Literature ~ Group, data = dt)
fit 

Call:
  aov(formula = Literature ~ Group, data = dt)

Terms:
  Group Residuals
Sum of Squares     2.6667 3115.3333
Deg. of Freedom         2         6

Residual standard error: 22.78645


> TukeyHSD(fit, "Group")
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = Literature ~ Group, data = dt)

$Group
diff       lwr      upr     p adj
B-A 0.6666667 -56.41875 57.75209 0.9992924
C-A 1.3333333 -55.75209 58.41875 0.9971738
C-B 0.6666667 -56.41875 57.75209 0.9992924



cor.test(~ Literature + Science, data= dt)

Pearson's product-moment correlation

data:  Literature and Science
t = 6.5107, df = 7, p-value = 0.0003308
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.6817766 0.9847014
sample estimates:
      cor 
0.9264278 

plot(Literature~Science, data = dt)
l1 <- lm(Literature ~ Science, data = dt)
abline(l1, col = "red")

summary(l1)


Lit <- scan()
1: 38 96 65 88 54 27 98 58 30

cbind(dt, Lit)
l2 <- lm(Lit~Science, data = dt)
plot(Lit~Science, data = dt)
abline(l2)
summary(l2)


cor.test(dt$Science, dt$Lit)
aov(Lit ~ Science, data = dt)

expand.grid(h=c(60,80),w=c(100,300),sex=c("Male","Female"))

regression<-function(x,y)
{
 ex.lm<-lm(y~x)
 print(summary(ex.lm))
 print(anova(ex.lm))
 win.graph()
 plot(x,y)
 abline(lm(y~x))
 res<-ex.lm$residuals  #residual plot
 yhat<-predict(ex.lm)
 win.graph()
 plot(yhat,res,main="residuals against fit value")
 abline(h=0)
 win.graph()
 qqnorm(res)
 qqline(res)
}
x<-rnorm(20)
y<-2*x+rnorm(20)
regression(x,y)


