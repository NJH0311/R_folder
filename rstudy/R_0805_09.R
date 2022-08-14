library(palmerpenguins)
#인스톨 패키지 펭귄....

data(package = "palmerpenguins")
data("penguins")

pg <- data.frame(penguins)
str(pg)

library(VIM)
aggr(pg, numbers = T, prop = F)
pg <- na.omit(pg)
dim(pg)

str(pg)

table(pg$species)
barplot(table(pg$species))

table(pg$island)
barplot(table(pg$island))

table(pg$sex)
barplot(table(pg$sex))


str(pg[, 3:6])
summary(pg[, 3:6])


par(mfrow = c(2,2)) # 1,4 주면 일자형태로 그래프나옴
hist(pg$bill_length_mm)
hist(pg$bill_depth_mm)
hist(pg$flipper_length_mm)
hist(pg$body_mass_g)
par(mfrow = c(1,1))


par(mfrow = c(2,2)) # 1,4 주면 일자형태로 그래프나옴
hist(pg$bill_length_mm, col = c('orange','violet','pink'))
hist(pg$bill_depth_mm)
hist(pg$flipper_length_mm)
hist(pg$body_mass_g)
par(mfrow = c(1,1))


my.color <- ifelse(pg$species == 'Gentoo', 'tomato',
                   ifelse(pg$species == 'Adelie','steelblue','orange'))

plot(pg$bill_length_mm, pg$bill_depth_mm,
     pch = 19, col = my.color)


cor(pg$bill_length_mm, pg$bill_depth_mm)

cor(pg[pg$species == 'Adelie', ]$bill_length_mm,      
    pg[pg$species == 'Adelie', ]$bill_depth_mm)







