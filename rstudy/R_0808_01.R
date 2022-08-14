# 집단별로 비율 확인

library(palmerpenguins)

df <- na.omit(penguins)

#집단(종)별
table(df$species)
prop.table(table(df$species))

#섬(지역)별
table(df$island)
prop.table(table(df$island))

#성별
table(df$sex)
prop.table(table(df$sex))


table(df$island, df$species)

tapply(df$species,
       INDEX = list(df$species),
       FUN = length)


library(gmodels)
CrossTable(df$island, df$species,
           prop = F, prop.chisq = F)

?CrosTable

library(psych)
describe(df)
describe(df)[, c(1:3, 8:9)]

??ggplot
library(ggplot2)


aggregate(df[, c(3:6)],
          by = list(species = df$species),
          FUN = mean)

tapply(df$bill_length_mm,
       INDEX = list(species = df$species),
       FUN = mean)

tapply(df$bill_depth_mm,
       INDEX = list(species = df$species),
       FUN = mean)

boxplot(flipper_length_mm ~ species,
        data = df, col = 2:4)

boxplot(bill_depth_mm ~ species,
        data = df, col = 2:4)



df <- data.frame(df)
adelie <- split(df, df$species)$Adelie
adelie
outlier <- boxplot.stats(adelie$flipper_length_mm)$out
outlier
df[df$flipper_length_mm %in% outlier, ]

length(df[df$flipper_length_mm %in% outlier, ])


library(palmerpenguins)
df <- na.omit(penguins)
df <- data.frame(df)

#날개 길이 오름차순, 체질량 내림차순으로 정렬

ord <- order(df$flipper_length_mm, -df$body_mass_g)
ord
df[ord,]
head(df[ord, 5:6], n = 10)
