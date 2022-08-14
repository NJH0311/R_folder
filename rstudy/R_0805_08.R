st
st <- data.frame(state.x77)
st[st$Population == max(st$Population), c(3,6)]

subset(st,
       subset = st$st$Population == max(st$Population),
       select = c(3,6))

iris[, -5]

subset(iris, select = 1:4)

set <- iris[iris$Species == 'setosa',]
ver <- iris[iris$Species == 'versicolor',]
vir <- iris[iris$Species == 'virginica',]

split(iris, f = iris$Species)


sp <- levels(iris$Species)
length(sp)
names(sp)
class(sp)

sp$setosa
sp$versucolor
sp$virginica

dim(sp$setosa)
dim(sp$versicolor)

df.2 <-  rbind(sp$setosa, sp$versicolor)
dim(df.2)


iris[,1:2]
iris[,3:4]

df.3 <- cbind(iris,[,1:2], iris[,3:4])
dim(df.3)
str(df.3)


library(readxl)
df.1 <- read_excel('성적표.xlsx',sheet = 1)
df.2 <- read_excel('성적표.xlsx',sheet = 2)

df.2$`Deep Learning`
df.1
df.2

cbind(df.1, df.2)

merge(df.1, df.2, all = T,
      by.x = c('번호','이름'),
      by.y = c('No','Name'))

colnames(df) <- c('No',
                  'name',
                  'python',
                  'r',
                  'ml',
                  'dl',
                  'cloud')


colnames(df)[6] <- '딥러닝'
colnames(df)

str(df)


df <- iris
aggregate(df[, 1:4],
          by = list(df$Species),
          FUN = mean)


df <- iris
aggregate(df[, -5],
          by = list(df$Species),
          FUN = mean)

df <- iris
aggregate(df[, -5],
          by = list(품종=df$Species),
          FUN = mean)

df <- iris
aggregate(df[, -5],
          by = list(품종=df$Species),
          FUN = sd)

library(MASS)
data("survey")
df <- survey
str(df)

df <- na.omit(df)
df <- df[complete.cases(df),]
dim(df)


hist(df$Height, breaks = 20) 
#히스토그램전체를 봐보고
hist(df[df$Sex == 'Male',]$Height, breaks = 20)
#남자만 봐보고
hist(df[df$Sex == 'Female',]$Height, breaks = 20)
#여자만 봐보고
# 둘다 바보고

mean(df[df$Sex == 'Male',]$Height)
mean(df[df$Sex == 'Female',]$Height)

aggregate(df[, c(10,12)],
          by = list(Gender =df$Sex),
          FUN = mean)


table(df$Sex)
t.test(Height ~ Sex, data = df)

boxplot(Height ~ Sex, data = df,
        col = c('orange', 'tomato'))


v <-  c(30,50,20,40,10)
v

sort(v)
sort(v,decreasing = T)

df <- data.frame(state.x77)
str(df)

sort(df$Illiteracy, decreasing = T)
sort(df, decreasing = T)

ord <- order(df$Illiteracy, df$Income, decreasing = T)
df[ord[1:10],c(3,2)]



s <- 0
for (i in 1:1000000){
x <- sample(1:10, size = 5)
s <- s + sum(x ==7)
}
s


set.seed(2022)
sample(1:10, size = 5, replace = T)


idx <- sample(1:nrow(iris), size = 50)
iris[idx,]


