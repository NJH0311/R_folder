# 7장 두 집단의 차이검정

str(sleep)
sleep


library(tidyr)
wide.df <- spread(sleep, key = group, value = extra)
summary(wide.df)

tapply(sleep$extra,
       INDEX = list(sleep$group),
       FUN = mean)

t.test(extra ~ group, data = sleep, paired = T)
t.test(wide.df$'1', wide.df$'2', paired = T)


# 8장 x2 분포와 x2 검정

v <- rchisq(n = 10000, df = 1)
hist(v, col = 'orange')

x <-  seq(0, 15, length = 200)
curve(dchisq(x, df = 1), 0, 15,
      col = "red", lwd = 2, lty = 1)
curve(dchisq(x, df = 5), 0, 15, add =T,
      col = "tomato", lwd = 2, lty = 1)
curve(dchisq(x, df = 10), 0, 15, add = T,
      col = "skyblue", lwd = 2, lty = 1)

qchisq(p = 0.95, df = 1)
# 3.841459
pchisq(q = 2.5, df = 1)
#0.8861537
pchisq(q = 3.841459, df = 1)
#0.95
pchisq(q = 5, df = 1, lower.tail = F)
#0.02534732


mt <- matrix(c(1443, 151, 47, 1781, 312, 135),nrow = 3)
mt

df <- data.frame(mt)
str(mt)
df

colnames(df) <- c('With','Without')
df
rownames(df) <- c('경상','중상','사망')
df

oij <- c(1443,1781,151,312,47,135)
eij <- c(1367,1855.9,196.9,267.4,77.1,104.7)
cs.value <- sum((oij - eij) ^ 2 / eij)
cs.value

#타이타닉
Titanic
class(Titanic)

tb <- margin.table(Titanic, margin= c(4, 2))
class(tb)
tb


chisq.test(tb)


#9장 F-분포와 분산분석
# curve 에러 뜨네..

v <- rf(n = 10000, df1 = 1, df2 = 30)
hist(v, col = 'steelblue')

x <- seq(0, 15, length = 200)
curve(df(x,df1 =1 , df2 = 30),0, 15,
      col = 'tomato', lwd = 2, lty = 1)

curve(df(x,df1 =5 , df2 = 50),0, 15, add = T,
      col = 'blue', lwd = 2, lty = 1)

curve(df(x,df1 =10 , df2 = 80),0, 15, add = T,
      col = 'orange', lwd = 2, lty = 2)

qf(p = 0.95, df1 = 1, df2 = 30)
pf(q = 4.10877, df1 = 1, df2 = 30)
pf(q = 4.10877, df1 = 1, df2 = 30, lower.tail = F)



#10장 일원 분산분석과 이원 분산분석


df <- InsectSprays
str(df)
table(df$spray)

round(tapply(df$count,
             INDEX = list(df$spray),
             FUN = mean),3)
#round : 소숫점 n만큼 뺴고 보여줌, 예)3을 적용, 0.000 까지 

boxplot(count ~ spray, data = df,
        col = 2:7)

aov.result<- aov(count ~ spray, data = df)
summary(aov.result)

TukeyHSD(aov.result)


library(gplots)
plotmeans(count ~ spray, data = df,
          col = "tomato",lwd = 3,
          barcol = 'orange', barwidth = 3)

model.tables(aov.result,type = "mean")
model.tables(aov.result,type = "effect")


plot(TukeyHSD(aov.result),
     las = 1, col = 'tomato')


library(car)
qqPlot(df$count, pch = 19, col = 'orange')
shapiro.test(df$count)

leveneTest(count ~ spray, data = df)

bartlett.test(count ~ spray, data = df)

oneway.test(count ~ spray, data = df)

df <- ToothGrowth
str(df)
unique(df$dose)

df$dose <-  factor(df$dose,
                   levels = c(0.5,1.0,2.0),
                   labels = c("L","M","H"))
str(df)

with(df, tapply(len, list(SUPP = supp, DOSE = dose), mean))

boxplot(len ~ supp * dose, data = df,
        col = c('orange','tomato'))

boxplot(len ~ supp + dose + supp:dose, data = df,
        col = c('orange','tomato'))

aov.result <- aov(len ~ supp * dose, data = df)
summary(aov.result)


TukeyHSD(aov.result)
plot(TukeyHSD(aov.result), las = 1)


#12장 상관관계와 상관분석
cor(cats$Bwt, cats$Hwt)
plot(cats$Bwt, cats$Hwt, pch = 19, col = 'tomato')

cor(cats$Bwt, cats$Hwt, method = 'pearson')
cor(cats$Bwt, cats$Hwt, method = 'spearman')
cor(cats$Bwt, cats$Hwt, method = 'kendall')


# 13장 선형회귀의 이해

library(HistData)
df <- GaltonFamilies
str(df)

#부모의 키가 크면 자녀의 키도 큰가
cor(df$midparentHeight, df$childHeight)
plot(childHeight ~ midparentHeight, data = df,
     col =adjustcolor('blue', alpha = 0.5),
                      pch = 19)


model <- lm(childHeight ~ midparentHeight, data = df)
abline(model, col = 'tomato', lwd = 3)     
#선형 회귀식'


x <-  runif(n = 100, min = 0, max = 100)
y <-  3 * x + 5 + rnorm(100, 0 , 20)
plot(x, y, pch = 19, col = 'skyblue')


cor(x,y)
model <- lm(y ~ x)
abline(model, col = 'tomato', lwd = 2)

summary(model)

abline(a = 1, b = 5, col = 'red',
       lwd = 1, lty = 2)
