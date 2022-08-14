
windows(width = 7, height = 5)
# 창 따로 해서 그래프 보임 (창은 계속 켜져있음)
v <- rbinom(n = 100000, size = 1000, prob = 0.4)
#rbinom:이항분포를 가진 랜덤분석,
#베르누이: 성공하거나 실패하거나,,,
hist(v, col = 'orange', breaks = 30)


set.seed(2022)
v <- runif(n = 100000, min = 0, max = 100)
hist(v, col = 'violetred2', breaks = 20)
#colors() # 색상 종류 목록 나옴
mean(v)
sd(v)

v <- rnorm(n = 100000, mean = 50, sd = 20)
hist(v, col = 'violet',breaks = 20)
#n의 숫자가 커질수록 정규분포에 가까워짐


x <- seq(0,100, length = 100)
y <- dnorm(x, mean = 50, sd = 20)
plot(x,y,type = 's', col = 'tomato', lwd = 3)

x <- seq(0,100, length = 100)
y <- dunif(x, min = 0, max = 100)
plot(x,y,type = 's', col = 'tomato', lwd = 3)


x <- seq(130,210, length = 100)
y <- dnorm(x, mean = 170, sd = 10)
plot(x,y,type = 'l', col = 'tomato', lwd = 3)
#키 


#연습문제
# 국민소득이 평균이 $30,000, 표준편차가 $10,000 인 #정규분포를 따른다고 가정
# 한다 . 즉 , 𝑋를 개인의 소득을 나타내는 확률변수라 할 # 때 ,
#𝑋~𝑁(30000,100002)
# 어떤 사람의 소득이 $25,000 ~ $35,000 사이에 있을 확률을 구하시오
pnorm(35000, mean = 30000, sd = 10000)
pnorm(25000, mean = 30000, sd = 10000)


pnorm(35000, mean = 30000, sd = 10000) - pnorm(25000, mean = 30000, sd = 10000)


pnorm(1, mean=0,sd=1) - pnorm(-1, mean=0,sd=1)

pnorm(2) - pnorm(-2)
pnorm(2.56) - pnorm(-2.56)

( 1- pnorm(87, mean = 68, sd = 10)) * 200

pnorm(87, mean = 68, sd = 10, lower.tail = F)

#연습문제
# 소득이 $25000보다 작을 확률
pnorm (25000, 30000, 10000)
# 소득이 $35000보다 클 확률
1 - pnorm (35000, 30000, 10000)


pnorm(70, mean = 60, sd = 10, lower.tail = F)
pnorm(80, 70, 20, lower.tail = F)


#연습문제 
#동전의 앞면이 나올 확률이 0.5 일 때 동전 던지기를 100 회 시행했다
#동전이 앞면이 나오는 횟수를 𝑋라고 할 때 확률분포의 그래프를 그려보자

x <- rbinom(10000, size = 100, prob = 0.5)
#앞뒷면 나올 확률 1혹은0   그래서 반반0.5 적용
hist(x, col = 'skyblue', breaks = 20, prob = T)
sd(x)
curve(dnorm(x, 50, 5), 30 ,70, col = 'tomato',
      add = T, lwd = 3, lty = 2)






#-----
library(MASS)
height <- na.omit(survey$Height)
length(height)
hist(height, col = 'skyblue', breaks = 20)

mean(height)
sd(height)


X.bar <- c()
for ( i in 1:100000){
    samp <- height[sample(1:209, size = 30)]
    X.bar[i] <- mean(samp)
    X.sd[i] <- sd(samp)
}
hist(X.bar, col = 'skyblue',breaks = 20, prob = T)
x <- seq(160, 180, length = 200)
curve(dnorm(x, mean(height),sd(X.bar)),
      160,180, col = 'tomato',
      add = T, lwd = 3, lty = 2)
#-----

x.1 <- rnorm(n = 5000, mean = 70, sd = 5)
x.2 <- rnorm(n = 5000, mean = 50, sd = 5)
x <- c(x.1, x.2)
hist(x, col = 'skyblue', breaks = 20)


#---
X.bar <- c()
for ( i in 1:100000){
    samp <- x[sample(x, size = 30)]
    X.bar[i] <- mean(samp)
}
hist(X.bar, col = 'skyblue',
     breaks = 20, prob = T)
x.samp <- seq(30,90, length = 200)
'''
curve(dnorm(x.samp, mean(x), sd(X.bar)),
      30, 90, col = "tomato",
      add = T, lwd = 3, lty = 2)
# 라인 그리기 실패.      
'''
#---

#05장 통계적 추정과 가설검정

cor(iris[, -5])

cor.test(iris$Petal.Width, iris$Petal.Length)


# 12 페이지
binom.test(x = 60, n =100, p = 0.5)


qnorm(p = 0.5, mean =50, sd = 10)

qnorm(p = 0.68, mean =50, sd = 10)

qnorm(p = 0.975, mean =50, sd = 10)

qnorm(p = 0.025, mean =50, sd = 10)

qnorm(p = 0.005, mean =50, sd = 10)
pnorm(24.24171, mean = 50, sd = 10)

qnorm(p = 0.995, mean =50, sd = 10)
pnorm(75.75829, mean = 50, sd = 10)


#16페이지
binom.test(x = 65, n = 100, p = 0.5)

#17페이지
binom.test(x = 35, n = 100, p = 0.5, conf.level = 0.99)

#19페이지
binom.test(x = 60, n = 100, p = 0.5, alternative = "greater")

#20페이지
binom.test(x = 45, n = 100, p = 0.5, alternative = "less")

shapiro.test(survey$Height)
hist(survey$Height)

shapiro.test(survey$Age)
shapiro.test(iris$Petal.Length)
shapiro.test(mtcars$mpg)


qqnorm(survey$Height, col = 'skyblue')
qqline(survey$Height, col = 'tomato', lwd = 3)

qqnorm(survey$Age, col = 'skyblue')
qqline(survey$Age, col = 'tomato', lwd = 3)


# 6장 t-분포와 평균검정

v <- rt(n = 10000, df = 29)
hist(v, col = "skyblue", prob = T)

x <- seq(-4, 4, length = 200)
curve(dt(x, df = 29), -4, 4, add = T,
      col = 'tomato', lwd = 2, lty = 2)

curve(dnorm(x), -4, 4, add = T,
      col = 'red', lwd = 5, lty = 4)

qt(p = 0.975, df = 29)
qt(p = 0.995, df = 29)

pt(q = 2.04523, df = 29)
pt(q = 2.756386, df = 29)





# 고양이
library(MASS)
data(cats)
str(cats)

table(cats$Sex)

mean(cats$Bwt)
tapply(cats$Bwt, INDEX = list(Sex = cats$Sex),mean)


t.test(Bwt ~ Sex, data = cats, confi.level = 0.99)

#잠
str(sleep)

t.test(extra ~ group, data = sleep , paired = T)




