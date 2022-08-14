class(TRUE)
class(3)
class('Hello')

7 %% 3

7 %/% 3
7 / 3
print(2^10)
3 **1
v <-1:100
class(v)
v

sum(1:100)

score <- 88

if (score >= 90){
  grade <-"A"
} else if (score >=80){
    grade <-"B"
} else {
  grede <-"F"
}
grade

print(3.14)
cat(3.14)


n <- 15
if (n%%3 == 0 & n%%5 == 0){
  order <- "피자나라치킨공주"
} else if (n%%3 == 0){
  order <- "피자"
} else if (n%%5 == 0){
  order <- '치킨'  
} else {
  order <- '다이어트'
} 
order

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
