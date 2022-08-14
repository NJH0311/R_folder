#14장 회귀분석의 유형

library(car)
data(Prestige)
df <- Prestige
str(df)


table(df$type)
barplot(table(df$type), col = "orange")

hist(df$income, col = 'tomato', breaks = 20)
shapiro.test(df$income)

hist(df$education, col = 'tomato', breaks = 20)
hist(df$women, col = 'tomato', breaks = 20)
hist(df$prestige, col = 'tomato', breaks = 20)

shapiro.test(df$prestige)

plot(df[, -(5:6)],pch = 19, col = "skyblue")

lm(income ~ education, data = df)

cor(df[, -(5:6)])

model <- lm(income ~ education, data = df)
summary(model)


plot(income ~ education, data = df,
     col = "skyblue", pch = 19)
abline(model, col = "tomato", lwd = 2)

#모두 적용
model <- lm(income ~ education + women + prestige,
            data = df)
summary(model)

#프레스티지 뺴고
model <- lm(income ~ education + women,
            data = df)
summary(model)

#우먼뺴고
model <- lm(income ~ education + prestige,
            data = df)
summary(model)

#에듀케이션 뺴고
model <- lm(income ~ women + prestige,
            data = df)
summary(model)


library(stargazer)
stargazer(model, type = 'text')

par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

model <- lm(income ~ education, data = df)
plot(income ~ education, data = df,
     col = "skyblue", pch = 19)

model <- lm(income ~ education + I(education^2),
            data = df)
summary(model)
abline(model)



library(tidyverse)

model <- lm(income ~ education + I(education^2),
            data = df)
plot(income ~ education, data = df,
     col = 'skyblue',pch = 19)
with(df, lines(arrange(data.frame(education,
                                  fitted(model)),education),
           lty = 1, lwd = 3, col = 'tomato'))


# 15장 회귀모델의 설명력

df <- mtcars
str(df)
df <- mtcars[, 1:6]
str(df)
plot(df, col = 'green', pch = 19)

cor(df)

#코르레이션
library(corrgram)
corrgram(df)
model <- lm(mpg ~ ., data = df)
summary(model)

#hp, wt만 했을때
model <- lm(mpg ~ hp + wt, data = df )
summary(model)

# wt만 했을때
model <- lm(mpg ~ wt, data = df )
summary(model)


#후진선택법

model <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
mod.selected <- step(model, direction = "backward")
summary(mod.selected)


#연습문제:
#kaggle House price 데이터셋에서
# 다중 선형 회귀의 변수 선택을 통해
# 최적의 독립 변수 조합을 찾아보시오.
# 1. 전진선택법으로 찾은 조합은? R2, Ajusted R2 값은?
# 2. 후진선택법으로 찾은 조합은? R2, Ajusted R2 값은?

SalePrice ~ 1

getwd()

HOUSEP1 <- read.csv("../../rstudy/House_price/train.csv", header = T)
str(HOUSEP1)
#후진
model <- lm(SalePrice ~ 1, data = HOUSEP1)
mod.selected <- step(model, direction = "backward")
summary(mod.selected)
#전진
model <- lm(SalePrice ~ 1, data = HOUSEP1)
mod.selected <- step(model, direction = "forward")
summary(mod.selected)
#단계
model <- lm(SalePrice ~ 1, data = HOUSEP1)
mod.selected <- step(model, direction = "stepwise")
summary(mod.selected)



getwd()

HOUSEP1 <- read.csv("../../rstudy/House_price/train.csv", header = T)

str(HOUSEP1)
dim(HOUSEP1)
is.num <- c()
for (i in 1:80){
    is.num[i] <- is.numeric(HOUSEP1[,i])
}
is.num
HOUSEP1 <- HOUSEP1[, is.num]# 수치형이 아닌 컬럼 제외
HOUSEP1 <- HOUSEP1[, -1]# Id 컬럼을 제외
dim(HOUSEP1)
HOUSEP1 <- HOUSEP1[complete.cases(HOUSEP1), ]# 결측치 행 제거
dim(df)
str(df)
model <- lm(SalePrice ~ ., data = HOUSEP1 )
summary(model)

#후진
mod.selected <- step(model, direction = "backward")
summary(mod.selected)
#-------------------------------
#전진부터 못따라갔음

model <- lm(SalePrice ~ 1, data = HOUSEP1)
mod.selected <- step(model, 
                     direction = 'forward',
                     scope = list(lower = ~ 1,upper = ~ MSSubClass +LotFrontage + OverallQual + OverallCond +X1stFlrSF + X2ndFlrSF + BsmtFullBath + KitchenAbvGr + TotRmsAbvGrd + ScreenPorch + PoolArea))#......


#전진
mod.selected <- step(model, direction = "forward")
summary(mod.selected)
#단계
mod.selected <- step(model, direction = "stepwise")
summary(mod.selected)



df <-  InsectSprays
str(df)
lm(count ~ spray, data = df )
model <- lm(count ~ spray, data = df)
summary(model)

contrasts(df$spray)

df <- mtcars[, 1:6]
str(df)

df$cyl <- factor(df$cyl)
head(df)
table(df$cyl)

lm(mpg ~ ., data = df)
model <- lm(mpg ~ ., data = df)
summary(model)


# 16장 선형모델의 일반화


df <- split(iris, f = iris$Species)
df <- rbind(df$setosa, df$versicolor)
plot(df[, c(1, 5)])



library(robust)
data(breslow.dat)

df <- breslow.dat
str(df)

df <- df[, c("Base","Age","Trt","sumY")]
str(df)
dim(df)

model <- glm(sumY ~ ., data = df, family = poisson)
summary(model)

exp(coef(model))



df <- split(iris, f = iris$Species)
df <- rbind(df$setosa, df$versicolor)
plot(df[, c(3, 5)])

#df$Species <- as.integer(df$Species)
model <- glm(Species ~ Petal.Length, data = df,
    family = binomial(link = "logit"))
summary(model)


