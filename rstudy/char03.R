v <- c(10,20,30,40,50,60,70)
v

v[1]
v[7]

v[1:3]
v[3:6]

v[c(1,3,4,7)]
v[-1]

v[c(T,T)]

# 1에서 100까지의 수 중엣 7의 배수의 합?
n <- 1:100
n 
seq(7,100,7)
sum(seq(7,100,7))

v <- 1:100
sum(v[v%%7 == 0]) 


v <- c()
for (i in 1:10){
  v[i] <- i
}
v

iris$Sepal.Length
iris$Species

f <- factor(c(1,2,1,2),
            levels = 1:3,
            labels = c('Male','Female','TG')) 
f
levels(f)

f[1]
f[f == 'Female']
f[6] <- 'Male'
f

f[7]<-'TG'
f


v.1 <- c(1,2,3)
v.2 <- c('F','F','M')
c(v.1,v.2)

lst <- list(id = v.1 , gender = factor(v.2))
lst

lst$id
lst$gender

n <- 32

# n의 약수를 모두 출력하시오
# 반복문은 사용하지마시오.
v <- 1:n
length(v[n %% v == 0])
n%%v
n%%v==0
v[n%%v==0]
length(v[n%%v==0])


# nrow 행갯수 파악


# Petal.length가 평균보다 큰 데이터의
# Petal.Width 평균값은 얼마인가?

#iris
#iris[mean(iris.petal.Length < petal.Length)]

#iris[mean(iris.Petal.Length < petal.Length),mean(Petal.Width)]
iris
mean(iris$Petal.Length)
mean(iris[iris$Petal.Length > mean(iris$Petal.Length), "Petal.Width"])


my.fun <- function(x,y,z){
  cat(x,y,z,'\n')
  return(x+y*2+z*3)
}
my.fun
my.fun(1,2,3)

my.fun(1,2, x=3)

my.fun(z=1,2, x=3)


my.fun <- function(x,y,z=0){
  cat(x,y,z,'\n')
  return(x+y*2+z*3)
}
my.fun(1,2)


# n 의 약수 구하기

divisor <-function (n) {
  # to do
  v <- 1:n
   x <-v[ n %% v == 0]
  return (x)
}
divisor(n = 32)

divisor <-function (n) {
  # to do
  v <- 1:n
  x <-v[ n %% v == 0]
  x
}
divisor(n = 32)

divisor <-function (n) {
  # to do
  v <- 1:n
  v[ n %% v == 0]
}
divisor(n = 32)


divisor <-function (n) {
  # to do
  length((1:n)[ n %% (1:n) == 0])
}
divisor(n = 32)

divisor <-function (n) length((1:n)[ n %% (1:n) == 0])
divisor(n = 32)

div.cnt <-function (n) length((1:n)[ n %% (1:n) == 0])
div.cnt(n = 32)


f1 <- function(n) n ^ 2
sapply(1:9, FUN = f1)# n값에 제곱, 즉,, 1~9까지 제곱됨


# 1~5장 연슨문제

1.
barplot(table(iris$Species),col = 'tomato', main = '품종의 막대그래프',xlab = '품종', ylab = '개수')

2.
hist(iris$Petal.Width,col = 'tomato',main = '꽃잎의 너비에대한 히스토그램',xlab = '꽃잎의 너비', ylab = '빈도수')
mean(iris$Petal.Width)
sd(iris$Petal.Width)
var(iris$Petal.Width)

3.
hist(mtcars$hp, col = 'tomato',xlim = c(0,400),ylim = c(0,12))
plot(mtcars$hp, mtcars$mpg, pch=8, col='tomato')

4.
length(cars)
summary(cars)
length(summary(cars))

plot(cars$speed,cars$dist, pch=8, col='tomato'
     ,xlim = c(0,30),ylim = c(0,150))

5.


6.
n <- 15
order <-'다이어트'
if (n%%15 == 0){
  order <- "피자나라치킨공주"
} else if (n%%3 == 0){
  order <- "피자"
} else if (n%%5 == 0){
  order <- '치킨'  
} else {
  order <- '다이어트'
} 

order



'파이애플파이'
'다음카ㅏㅏㅏㅏㅏㅏㅏ페'
''