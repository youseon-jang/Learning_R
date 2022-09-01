df <-  data.frame(sex = c("M", "F", NA, "M", "F"), 
                  score = c(5, 4, 3, 4, NA))

df

is.na(df)
table(is.na(df))

table(is.na(df$score))
table(is.na(df$sex))

###filter 이용 방법
df %>% filter(!is.na(df$score) & !is.na(df$sex)) %>% 
  group_by(sex) %>% 
  summarise(mean = mean(score))

df %>% filter(!is.na(df$sex)) %>% 
  group_by(sex) %>% 
  summarise(mean = mean(score, na.rm = T))

df %>% filter(sex == "M" | sex == "F") %>% 
  group_by(sex) %>% 
  summarise(mean = mean(score, na.rm = T))


df_nomiss <- df %>% filter(!is.na(df$score))
df_nomiss

df_nomiss2 <- na.omit(df)
df_nomiss2

library(dplyr)
airquality

mean(airquality$Ozone, na.rm = T)

df
mean <- mean(df$score, na.rm = T)
mean

df$score <-  ifelse(df$score == NA, mean, df$score)
df


df <-  data.frame(sex = c("M", "F", NA, "M", "F"), 
                  score = c(5, 4, 3, 4, NA))

mean <- mean(df$score, na.rm = T)

df$score <- ifelse(is.na(df$score) , mean, df$score)
mean(df$score)



outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

mean(outlier$sex)
mean(outlier$score)


outlier$sex <- ifelse(outlier$sex != 1 & outlier$sex != 2, NA, outlier$sex)
outlier

outlier$sex <- ifelse(outlier$sex == 1 | outlier$sex ==2, outlier$sex, NA)
outlier

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier


outlier %>% filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean(score))

outlier %>% filter(sex == 1 | sex == 2) %>% 
  group_by(sex) %>% 
  summarise(mean(score, na.rm = T))

na.omit(outlier %>% group_by(sex) %>% 
          summarise(mean(score)))

outlier %>% filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(mean = mean(score, na.rm = T))


library(ggplot2)
mpg
mpg$hwy
table(mpg$hwy)
unique(mpg$hwy)
mpg[ , 'hwy']

boxplot(mpg$hwy)

##drv별로 hwy의 평균값, 이상치는 제외하고
mpg

boxplot(mpg$hwy)

mpg %>% arrange(desc(hwy))
mean(mpg$hwy)

mpg %>% select(hwy) %>% arrange(desc(hwy))
mpg %>% select(hwy) %>% arrange(hwy)

mpg %>% filter(hwy <= 37 | hwy >= 12) %>% 
  group_by(drv) %>% 
  summarise(mean(hwy))

mpg$hwy <- ifelse(mpg$hwy > 37 | mpg$hwy < 12, NA, mpg$hwy)
mpg %>% group_by(drv) %>% summarise(mean(hwy, na.rm = T))

str(mpg)
ggplot(mpg, aes(x = hwy, y = displ, color = "red", size = 3)) + geom_point() +
  xlim(20, 30) + ylim(2, 6)


dplyr::glimpse(mpg)

outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1, 5),
                      
                      score = c(5, 4, 3, 4, 2, 6, 9))
outlier

outlier %>% filter(sex == 1 | sex == 2) %>% 
  group_by(sex) %>% 
  summarise(mean = mean(score))


outlier$sex <- ifelse(outlier$sex == 1 | outlier$sex == 2, outlier$sex, NA)
outlier %>% filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(mean = mean(score))


outlier

mpg
mpg %>% select(hwy) %>% arrange(desc(hwy))


9*1.5


outlier$score <- ifelse(outlier$score >= 5, NA, outlier$score)
outlier


library(ggplot2)
library(dplyr)
airquality
summary(airquality)

##Solor.R 열의 평균
airquality %>% select(Solar.R)
mean <-  mean(airquality$Solar.R, na.rm = T)
mean <-  mean(airquality$Solar.R, na.rm = F
mean


airquality$Month <- as.character(airquality$Month)
airquality$Month <- as.factor(airquality$Month)

ggplot(airquality, aes(x = Month, y = Ozone, color = Month)) + geom_point()


airquality %>%  filter(is.na(Solar.R)) %>% filter(!is.na(Ozone))

ggplot

mpg
mpg <- mpg %>% mutate(평균연비 = (cty+hwy)/2)
mpg$평균연비 <- ifelse(mpg$평균연비 <= 11 | mpg$평균연비 >= 37, NA, mpg$평균연비)
table(mpg$평균연비 <= 11 | mpg$평균연비 >= 37)


mean(mpg$평균연비, na.rm = T)









