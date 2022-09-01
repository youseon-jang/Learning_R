getwd()
library(readxl)
read_excel("excel_exam.xlsx")
read.csv("csv_exam.csv")

df <- read.csv("csv_exam.csv")
df
df <- df[ , 1:5]
write.csv(df, file = "practice_0824")


read.csv("아파트(매매)__실거래가_20180513144733.csv", fileEncoding = 'CP949')

apt <- read.csv("아파트(매매)__실거래가_20180513144733.csv", fileEncoding = 'CP949')
apt
head(apt)
tail(apt)
dim(apt)
str(apt)

head(apt, 3)

dim(apt)[1] ###row
dim(apt)[2] ###col

nrow(apt)
ncol(apt)

str(apt)

View(apt)
summary(apt)

library(dplyr)
apt %>% filter("전용면적" >= 40 & "건축년도" > 1982 & "계약년월" == 201804)
apt1 <- apt %>% filter("전용면적" >= 40 & "건축년도" > 1982)
apt1
apt1 %>% arrange(apt1)
apt1 %>% arrange(desc(apt1))
summary(apt1)
dim(apt1)
mean(apt1$전용면적...)

apt1 %>% select(전용면적..., 거래금액.만원., 건축년도, 층)
##건축년도가 2000이후이고 층이 10층 이상인 건물의 거래 금액과 전용면적
apt1 %>% filter(건축년도 >= 2000 & 층 >= 10) %>% select(거래금액.만원., 전용면적...)



df <- read.csv("csv_exam.csv")
df
df %>% arrange(math, df)
df %>% arrange(class, math, desc(english))

#반은 2반, 3반이고 수학이 50점 이상인 학생들의 영어와 과학점수를
#영어는 오름차순, 과학은 내림차순으로 정렬

df %>% filter(class == c(2,3) & math >= 50) %>% select(english, science) %>% 
  arrange(english, desc(science))


df %>% mutate(total = english + math + science,
              average = total/3)

df %>% mutate(total = apply(df[ , 3:5], MARGIN = 1, sum),
              average = apply(df[ , 3:5], MARGIN = 1, mean))

df1 <- df %>% mutate(total = english + math + science,
                     average = total/3)
df1

df1 %>% mutate(result = ifelse(total>= 200, "P", "F"))

df
df %>% summarise(수학평균 = mean(math))


df %>% group_by(class) %>% summarise(수학평균 = mean(math),
                                     영어평균 = mean(english),
                                     과학평균 = mean(science),
                                     학생수 = n())


library(ggplot2)
mpg
#새로운 칼럼. 평균주행연비
mpg %>% select(manufacturer, model, cty, hwy, displ, year) %>% 
  mutate(평균연비 = (cty+hwy)/2)

#제조사 별 도시주행연비의 평균은?
mpg %>% group_by(manufacturer) %>% summarise(mean(cty))

#제조사, 모델 별 도시주행연비의 평균은?
mpg %>% group_by(manufacturer, model) %>% summarise(mean(cty))

test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

test2 <-  data.frame(id = c(1, 2, 3, 4, 5),
                     final = c(70, 83, 65, 95, 80))

test1
test2

left_join(test1, test2, by = 'id')
right_join(test1, test2)
inner_join(test1, test2)
full_join(test1, test2)


test3 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

test4 <-  data.frame(id = c(4, 5, 6, 7, 8),
                     final = c(70, 83, 65, 95, 80))

left_join(test3, test4, by = 'id')
right_join(test3, test4)
inner_join(test3, test4)
full_join(test3, test4, by = 'id')




test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

test2 <-  data.frame(id = c(1, 2, 3, 4, 5),
                     final = c(70, 83, 65, 95, 80))


test1
test2

colnames(test1)[2] <- "test"
test1

colnames(test1) <- c('id', 'test')
test1

test1 <- test1 %>% rename(test = midterm)
test1

matrix


library(dplyr)
test1 <- data.frame(id=c(1,2,3,4,5), midterm=c(60,80,70,90,85)) 
test1 <- test1 %>% rename(test = midterm)
test1

colnames(test1)[2] <- "test"
colnames(test1) <-  c("id", "test")

test1 <- test1 %>% rename(test = midterm)
test1



library(ggplot2)
mpg
airquality
str(airquality)
air <- as_tibble(airquality)
air


read.csv("csv_exam.csv")
exam <- read.csv("csv_exam.csv")
exam %>% filter(class != 1) %>% filter(english >= 90 | science >= 90)

exam %>% mutate(total = math + science + english)
exam %>% mutate(통과여부 = ifelse(math+science+english>=200, "Pass", "Fail"))

names(exam)



class1 <- data.frame(name = c("kim", "lee", "park", "ang", "min"),
                     score = c(93, 84, 87, 98, 77))

class2 <- data.frame(name = c("kang", "yun", "cho", "yang", "jung"),
                     score = c(90, 95, 75, 79, 90)) 

full_join(class1, class2)
bind_rows(class1, class2)
class <- bind_rows(class1, class2)
class <- class %>% mutate(pass = ifelse(score >= 90, "pass", "fail"))
class %>% arrange(desc(score)) %>% head(1) %>% select(일등 = score)
