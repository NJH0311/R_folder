df <- iris
str(df)
class(df)
dim(df)
nrow(df)
ncol(df)
head(df)

rownames(df)
colnames(df)

v <- c(85,77,96)
v

names(v)
names(v) <-  c('Kor', 'Eng', 'Math')
names(v)

v

v['Kor']
v[c('Eng' , 'Math')]

df$Sepal.Length
df$Sepal.Width

df$Sepal.sum <- df$Sepal.Length + df$Sepal.Width

str(df)

head(df[,5:6])


df$Sepal.Sep <- ifelse( df$Sepal.sum > mean(df$Sepal.sum),"Big", "Small")
df$Sepal.Sep <-  factor(df$Sepal.Sep)
str(df)
levels(df$Sepal.Sep)
table(df$Sepal.Sep)

barplot(table(df$Sepal.Sep))

?state.x77
class(state.x77)
is.data.frame(state.x77)
st <-  as.data.frame(state.x77)
class(st)
str(st)
dim(st)



df <- iris
df$Sepal.Sum <-  df$Sepal.Length + df$Sepal.Width
write.csv(df,'my.iris.csv',row.names = F)
getwd()

df <- read.csv(df,'my.iris.csv',header = T)

library(readxl)
df <- read_excel('성적표.xlsx', sheet = 1)
str(df)
class(df)

df$평균 <- (df$파이썬 + df$머신러닝 + df$r)

df$평균 <- round(apply(df[,3:5], MARGIN = 1,mean),2)

head(df)
write.csv(df, 'score.csv', row.names = F)

df
