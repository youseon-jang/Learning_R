#### 결측치 이상치
###결측치 : 없는 값 (빈칸) --> NA(not available)/ nan

##결측치를 꼭 처리를 해야 함(에러 방지)
##결측치가 있는지? 
##있다면 몇 개가 있는지? 
##처리를 어떻게 할지?

###결측치 처리
##filter는 해당 항목 안에서만 걸러짐

##na.omit() : NA가 있는 row를 지워줌(NA가 아닌 다른 항목들의 값도 지워짐)
##na.rm() : NA를 지워줌

df <-  data.frame(sex = c("M", "F", NA, "M", "F"), 
                  score = c(5, 4, 3, 4, NA))
df                  

is.na(df) ##결측치 확인 / TRUE FALSE로 나타남

table(is.na(df)) ##table() 함수는 개수를 세어줌

table(is.na(df$sex))
table(is.na(df$score))

mean(df$score)
sum(df$score)

###filter
df %>% filter(is.na(score)) ##score 항목이 NA인 데이터
df %>% filter(!is.na(score)) ##score 항목의 결측치 제거

df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)
sum(df_nomiss$score)

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss

###na.omit()
df_nomiss2 <- na.omit(df)
df_nomiss2

###na.rm = T
mean(df$score, na.rm = T)
sum(df$score, na.rm = T)

###연습
airquality

##NA가 모두 몇 개 있습니까?
table(is.na(airquality))

##어느 칼럼에 몇 개가 있습니까?
summary(airquality) ###NA가 있으면 개수 세서 보여줌

##오존 농도의 평균은?
mean(airquality$Ozone, na.rm = T)

###결측치 처리 방법
###방법 1 : 결측치 빼기
###방법 2 : 결측치 제외한 평균값을 NA에 집어넣음
###방법 3 : 바로 위/뒤의 행과 같은 값을 집어넣음
###방법 4 : 10명 씩 끊어서 평균을 구해서 NA에 집어넣음(그룹을 묶어서)

####이상치(outlier)
####상자 그림이 필요함 (이상치 판단을 위해)

outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

table(outlier$sex) ##table 함수는 전체로 쓰지 않고 칼럼 하나가 들어감
table(outlier$score)

##sex가 3이면 NA할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex) ##False면 그대로

##score가 5보다 크면 NA할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier

##성별 평균
mean(outlier$sex, na.rm = T)
sum(otlier$score, na.rm = T)

###성별 별 평균
##방법 1
outlier %>% filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))

##방법 2
outlier %>% filter(sex == 1 | sex == 2) %>% 
  group_by(sex) %>% 
  summarize(mean_score = mean(score, na.rm = T))

##방법 3
na.omit(outlier %>% 
          group_by(sex) %>% 
          summarise(평균 = mean(score))
        
##방법 4
outlier %>% filter(!is.na(sex)) %>% 
  group_by(sex) %>% 
  summarise(평균 = mean(score, na.rm = T))
        
### 상자 그림(이상치 찾을 때 사용)
##수염 밖에 있으면 이상치임
##IQR (1사분위와 3사분위의 사이 / 상자의 높이를 결정)
##Q1 - 1.5*IQR ~ Q3 + 1.5*IQR 를 정상으로 취급(이론적으로 이를 통해 수염 결정)
##있는 데이터를 바탕으로 수염의 길이가 결정됨

library(ggplot2)
mpg
boxplot(mpg$hwy) ##상자 그림 통계치 출력

df <- df %>% head(3)
df
rm(df)
df


mpg <- mpg %>% head(2)
mpg
rm(mpg)
mpg 
###다운받은 패키지 안에 있는 데이터셋은 변형 X
###다운받은 패키지를 detach 했다가 다시 library하면 안에 있던 데이터 복구됨

mpg$drv ##output이 vector로 나옴
unique(mpg$drv) ##중복되는 값을 한 번만 나타나도록 표현(범용적으로 사용)
table(mpg$drv) ##개수 세어주는
mpg[ , 'drv'] ##output이 tibble임(원래 mpg가 tibble)


##drv별로 hwy의 평균값, 이상치는 제외하고
boxplot(mpg$hwy)

##방법 1
mpg %>% select(hwy) %>% arrange(desc(hwy)) %>% head
mpg %>% select(drv, hwy) %>% filter(hwy < 37 & hwy > 12) %>% 
  group_by(drv) %>% summarise(평균 = mean(hwy))

##방법 2
mpg$hwy <-  ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
mpg$hwy
table(is.na(mpg$hwy))

mpg %>% 
  group_by(drv) %>% summarise(평균 = mean(hwy, na.rm = T))


####ggplot2 그래픽 패키지
library(ggplot2)
mpg
ggplot(data = mpg, aes(y = hwy,)) + geom_boxplot(color = 'skyblue')

ggplot(data = mpg, mapping = aes(x = class, y = hwy, fill = class)) + geom_boxplot()
## data = , mapping = 은 생략 가능

###산점도 그래프
ggplot(mpg, aes(x = displ, y = hwy))

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point(color = "blue")

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point(color = "blue", size = 3)

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)
##x축 범위를 3-6으로 좁힘

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6) + ylim(10, 30)
##y축 범위를 10-30으로 좁힘


ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + geom_point()
## x : 숫자, y : 숫자, color : 문자

ggplot(mpg, aes(x = displ, y = hwy, color = cyl)) + geom_point()

ggplot(mpg, aes(x = displ, y = hwy, color = class)) + geom_point()

ggplot(mpg, aes(y = hwy, color = class)) + geom_boxplot()
ggplot(mpg, aes(y = hwy, fill = class)) + geom_boxplot()
### 어떤 그래프는 변수가 하나만 들어가기도 하고 두 개가 들어갈 수 있음
### boxtplot은 1 variable로도 2 variables도 가능(color 포함 시 3개까지 가능)
### 따라서 그래프에 따른 input 변수 개수와 종류 가능 여부 파악하는 것이 중요
### discrete, continuous 자료의 유형도 파악하는 것도 중요 (cheatsheet 참고)
### discrete : 범주형 변수, continuous : 연속형 변수
##범주형 변수 : 금, 은, 동처럼 문자이지만 순서가 있는 것
##factor = category  = 범주형

ggplot(mpg, aes(x = displ, y = hwy, color = cty)) + geom_point()
## x, y, color : 숫자 (색을 그라데이션으로 연속형으로 처리함)



##library로 실행하지 않고 패키지를 한 번만 사용하고싶을 때 :: 사용
dplyr::glimpse(mpg)
str(mpg)
##str()은 console창 크기가 작으면 줄바꿈 등으로 읽기가 힘듦
##glimpse()는 str()와 다르게 console 창의 크기가 작아도 정리된 상태로 보여짐


###geom_point에도 aes 붙일 수 있음
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point(color = "red")
###산점도 그래프의 점들에 색을 넣으려면 geom_point()안에 color 넣어야 함

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point(color = drv)

ggplot(mpg, aes(x = displ, y = hwy)) + geom_point(aes(color = drv))
###위에는 안됨
###geom_point 함수 안에서 color = 칼럼명 을 하려면 aes()를 꼭 써줘야 함

ggplot(mpg, aes(x = displ)) + geom_point(aes(y = hwy, color = drv))
ggplot(mpg) + geom_point(aes(x = displ, y = hwy, color = drv))
### 같은 논리로 위 두 줄의 코드도 가능함
### ggplot() 함수 안에서도 aes()속에 넣었으니 geom_point()안에도 aes() 필요


ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) + geom_smooth(method = lm)

g <- ggplot(mpg, aes(x = displ, y = hwy))
g + geom_point()
g + geom_smooth()
###반복되는 부분은 하나의 변수에 부여해서 레이어를 쌓아가는 것이 보기 좋음

f1 <- geom_point()
g + f1
### 코드 재사용 쉽게 하는 것이 좋음


ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + geom_point()
ggplot(mpg, aes(x = displ, y = hwy, color = drv, shape = drv)) + geom_point()
### 데이터 분석 용이하게 하려고 color와 shape 사용

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(size = 4) +
  geom_smooth()


ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(size = 4) +
  geom_smooth(method = lm)
### method = lm 은 직선으로 표현됨

ggplot(mpg, aes(x = displ, y = hwy, color = drv, shape = drv)) +
  geom_point(size = 4) +
  geom_smooth(method = lm) +
  theme_update()
### theme_은 뒤의 배경 바꾸는 것

install.packages("ggthemes")
library(ggthemes)
ggplot(mpg, aes(x = displ, y = hwy, color = drv, shape = drv)) +
  geom_point(size = 4) +
  geom_smooth(method = lm) +
  theme_economist()

ggplot(mpg, aes(x = displ, y = hwy, color = drv, shape = drv)) +
  geom_point(size = 4) +
  theme_wsj()



























