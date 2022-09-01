library(ggplot2)
library(ggthemes)

ggplot(data = mpg, aes(x = displ, y= hwy, color = drv)) + 
  geom_point(size = 4) +
  theme_economist()

g <- ggplot(data = mpg, aes(x = displ, y= hwy, color = drv)) + 
  geom_point(size = 4)

g + theme_economist() + 
  labs(title = "배기량 연비 비교", x = "배기량", y = "고속도로 연비") 
###labs는 label을 붙이는 것
###ggtitle도 많이 씀

###xlab(), ylab()도 많이 씀
g + theme_economist() + 
  labs(title = "배기량 연비 비교") + 
  xlab("배기량") 


###facet
###categorical variable
###'.'은 전부 다 의미함
###'~'은 lm을 의미
###앞이 y이고 뒤가 x 임
g + facet_grid(drv ~ .)
g + facet_grid(. ~ drv)
g + facet_grid(drv ~ cyl)

### shape 추가한 것
ggplot(data = mpg, aes(x = displ, y= hwy, color = drv, shape = drv)) + 
  geom_point(size = 4) + facet_grid(drv ~ .)

g + facet_wrap( ~ class)
g + facet_wrap( ~ drv)

###facet_wrap
p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
p + facet_wrap(vars(class))
p + facet_wrap( ~ class)
p + facet_wrap( ~ drv)

g + facet_wrap( ~ class, nrow = 4)
## nrow는 나타나지는 행의 개수 정해지는 것 (7개의 항목을 어떻게 배치할 지지
## )
##nrow는 facet_wrap만 있음(facet_grid는 없음)



ggplot(data = mpg, aes(x = displ, y= hwy)) +
  geom_point(size = 4, aes(color = drv))

###그래프에 중복되는 값은 같은 자리에 계속 찍혀서 분석 어려움
###해결 방법 중 하나는 jitter 함수 이용
###다른 하나는 바이올린 그래프 이용하는 것임
ggplot(data = mpg, aes(x = displ, y= hwy)) +
  geom_point(size = 4, aes(color = drv)) +
  geom_jitter(position = 'jitter')

ggplot(data = mpg, aes(x = displ, y= hwy)) +
  geom_point(size = 4, aes(color = drv)) +
  geom_jitter(width = 0.5, height = 0.5)

###geom_line
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 1) +
  geom_line(size = .5)

###geom_bar
mpg$displ
summary(mpg$displ)
str(mpg)

###자동적으로 y 축이 count로 설정( 개수를 셈 )
###따라서 character 자료형이 적합함
ggplot(data = mpg, aes(x = drv)) +
  geom_bar()

ggplot(data = mpg, aes(x = displ)) +
  geom_bar()
##displ은 bar 그래프로 표현하는 것이 적합하지 않음

mpg$year <- as.character(mpg$year)
ggplot(data = mpg, aes(x = year)) +
  geom_bar()
##자료 타입을 바꾼 후 그래프를 그릴 수도 있음

mpg$year <- as.integer(mpg$year)
mpg

ggplot(data = mpg, aes(x = class)) +
  geom_bar(color = "red", fill = "white")

ggplot(data = mpg, aes(x = class)) +
  geom_bar(fill = "purple")

ggplot(data = mpg, aes(x = class, color = "black")) +
  geom_bar(fill = "purple")
### aes() 안에 fill/color = 에는 항목을 넣어야 함 (색상 이름을 넣는 것이 아님)
### fill/color에 넣은 항목은 numeric이면 안됨!

ggplot(data = mpg, aes(x = class, fill = drv)) +
  geom_bar()

ggplot(data = mpg, aes(x = class, fill = drv)) +
  geom_bar(position = 'dodge')
###누적이 아닌 옆으로 쪼개서 나타남

ggplot(data = mpg, aes(x = class, fill = class)) +
  geom_bar(position = 'dodge')
ggplot(data = mpg, aes(x = class, fill = class)) +
  geom_bar()
###항목(mpg$class) 별로 색이 다르게 bar chart 그려짐

ggplot(data = mpg, aes(x = drv, fill = class)) +
  geom_bar()

ggplot(data = mpg, aes(x = drv, fill = class)) +
  geom_bar(position = 'dodge')

ggplot(data = mpg, aes(x = displ)) +
  geom_bar()

ggplot(data = mpg, aes(x = displ, fill = drv)) +
  geom_bar()

ggplot(data = mpg, aes(x = displ, fill = as.factor(drv)) +
         geom_bar()

ggplot(data = mpg, aes(x = displ, fill = as.factor(drv)) +
         geom_bar(position = 'dodge')

ggplot(data = mpg, aes(x = drv, fill = as.factor(class)) +
         geom_bar(position = 'fill')
#####다시 해보기

###histogram
###bar chart와 다르게 빈칸이 없음
###도수분포다각형 : histogram에서 bar의 위 점들을 이어서 만든 것
ggplot(data = mpg, aes(x = displ)) +
  geom_bar()

ggplot(data = mpg, aes(x = displ)) + 
  geom_histogram()

ggplot(data = mpg, aes(x = class)) + 
  geom_histogram()
###x에 class를 넣으면 에러 뜸
###histogram에서 x축은 continuous variable 넣어야 함

ggplot(data = mpg, aes(x = class)) + 
  geom_histogram(stat = 'count')
## stat = 'count' 넣으면 continuous 아니어도 가능해짐

ggplot(data = mpg, aes(x = class)) +
  geom_bar()
##bar chart는 histogram과 다르므로 가능함

ggplot(data = mpg, aes(x = displ)) + 
  geom_histogram(binwidth = .1)

ggplot(data = mpg, aes(x = displ)) + 
  geom_histogram(binwidth = .5)

ggplot(data = mpg, aes(x = displ)) + 
  geom_histogram(binwidth = .8)
### bin은 막대 하나를 의미
### binwidth에 따라서 나타나는 bin의 개수 달라짐

ggplot(data = mpg, aes(x = displ)) + 
  geom_histogram(bins = 10)

ggplot(data = mpg, aes(x = displ)) + 
  geom_histogram(bins = 20)
### bin의 개수를 직접 지정할 수 있음


###연습
ggplot(diamonds, aes(x = carat, y = price, color = cut)) + geom_point()
str(diamonds)
### color에는 Ord. factor인 cut, color, clarity만 넣을 수 있음

ggplot(diamonds, aes(x = carat, y = price, color = depth)) + geom_point()

ggplot(economics, aes(x = date, y = unemploy)) + geom_line()

###geom_smooth()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(methond = 'lm')

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(methond = 'loess') ###default값임

ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_point()
###어느 한 변수가 categorical variables(범주형 변수)일 때 geom_point() 쓰면
###일직선 상으로 점들이 배열됨
###하지만 이 그래프로 평균을 구하는 것과 같은 분석하는 것은 위험
###차라리 boxplot 사용
###boxplot보다 violin chart가 더 좋음

ggplot(mpg, aes(x = drv, y = hwy)) + geom_boxplot()

##전륜(drv)에서 연비가 가장 떨어지는 것 찾아보기
library(dplyr)
mpg

mpg %>% filter(hwy < 20 & drv == 'f')

mpg %>% filter(drv == "f" & hwy < 25) %>% arrange(hwy)

###tidy

###geom_bar()
drugs <- data.frame(drug = c("a", "b", "c"),
                    effect = c(4, 9, 6))

drugs

ggplot(drugs, aes(x = drug, y = effect)) + geom_bar()
##Error! stat_count() can only have an x or y aesthetic

ggplot(drugs, aes(x = drug, y = effect)) + geom_bar(stat = 'identity')
##bar 차트는 1차원만 되는데 stat = 'identity' 입력하면 그래프 뜸 (2차원)

ggplot(drugs, aes(x = drug, y = effect)) + geom_col()
##1차원 : geom_bar() / 2차원 : geom_col()


economics
###date도 데이터 타입 중 하나임
###date time, 시계열(time series)

ggplot(economics, aes(date, unemploy / pop)) + geom_line()
## unemploy / pop은 없는 column이지만 r에서는 새로 만들어서 계산 가능

ggplot(economics, aes(date, uempmed)) + geom_line()

##구동방식(drv) 별 도시주행연비(cty)의 평균을 막대 그래프로 나타내기
ggplot(mpg, aes(x = drv, y = mean(cty))) + geom_col()

##제조사 별로, 구동방식에 따른 도시주행연비의 평균을 막대 그래프로 나타내기
str(mpg)

mpg1 <- mpg %>% group_by(manufacturer, drv) %>% summarise(mean.cty = mean(cty))
mpg1
##tibble 형태임

mpg %>% group_by(manufacturer, drv) %>% summarise(mean.cty = mean(cty)) %>%
  ggplot(aes(x = manufacturer, y = mean.cty, fill = drv)) + 
  geom_col(position = 'dodge')
### dplyr과 ggplot 같은 팀에서 개발됐기 때문에 연동됨
### 따라서 따로 dataset 저장하고 글지 않고 pipe 사용해서 그래프 그리기 가능


ggplot(mpg1, aes(x = manufacturer, y = mean.cty, fill = drv)) + 
  geom_col(position = 'dodge')

###backtick
###숫자 1 옆에 `` 임(''와 헷갈리지 X)

library(ggplot2)
library(dplyr)
mpg
summary(mpg)
ggplot(mpg, aes(x = manufacturer)) + geom_bar() + 
  facet_wrap(. ~ class, nrow = 3)


str(mpg)

unique(mpg$year)

ggplot(mpg, aes(x = as.factor(year))) + geom_bar(fill = c("red", "skyblue"))

ggplot(mpg, aes(x = as.factor(year), fill = as.factor(year))) + geom_bar()

ggplot(mpg, aes(x = class)) + geom_histogram(stat = 'count')

drugs <- data.frame(drug = c("a", "b", "c"),
                    effect = c(4, 9, 6)) 


ggplot(drugs, aes(x = drug, y = effect)) + geom_bar(stat = 'identity')

summary(mpg)

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + geom_point()+
  labs(title = '배기량에 따른 고속도로 연비 비교', x = '배기량', y = '연비') +
  facet_wrap(drv ~ .)

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + geom_point()+
  labs(title = '배기량에 따른 고속도로 연비 비교', x = '배기량', y = '연비') +
  facet_grid(drv ~ .)

install.packages("gapminder")
library(gapminder)

data()
data(package = "gapminder")


