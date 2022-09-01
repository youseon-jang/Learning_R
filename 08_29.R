library(gapminder)

gapminder

gapminder1 <- if (require("dplyr")) {
  gapminder %>%
    filter(year == 2007) %>%
    group_by(continent) %>%
    summarise(lifeExp = median(lifeExp))
  }
  
ggplot(gapminder1, aes(x = continent, y = lifeExp,
                       fill = continent)) + geom_bar(stat = 'identity')

###pipe를 이용하면 변수명 tap키로 자동완성 가능
gapminder1 %>% ggplot(aes(x = continent, y = lifeExp, fill = continent)) +
  geom_col()


###범례(legend) 지우기
gapminder1 %>% ggplot(aes(x = continent, y = lifeExp, fill = continent)) +
  geom_col() + theme(legend.position = "none")


gapminder %>% ggplot(aes(x = continent, y = lifeExp, fill = continent)) +
  geom_boxplot()

###plotly
###interactive한 그래프를 그릴 수 있음
###마우스를 점 위에 두면 값을 볼 수 있음
###Web Page로 저장하면 interactive하게 그대로 저장됨
install.packages("plotly")
library(plotly)

library(ggplot2)
mpg

ggplotly(
  mpg %>% ggplot(aes(x = displ, y = cty, color = drv)) + geom_point()
  )



library(plotly)

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School, 
               type = 'scatter', mode = 'markers',
               marker = list(size = ~Gap, opacity = 0.5))

fig <- fig %>% layout(title = 'Gender Gap in Earnings per University',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))

fig


###색상 바꾸기
install.packages("RColorBrewer")
library(RColorBrewer)

str(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)

ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar()


##방법 1: hcl 각도 조정
##Hue - Chroma - Luminance(HCL)
##scale_fill_hue() / scale_color_hue()
ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar() +
  scale_fill_hue(c = 50)

ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar() +
  scale_color_hue(h = c(30, 90), c = 20, l = 30)



##방법 2: palette
ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar() +
  scale_fill_brewer(palette = "Set1")

ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar() +
  scale_fill_brewer(palette = "Set2")

ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar() +
  scale_fill_brewer(palette = "Set3")



##방법 3: 직접 지정
ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar() +
  scale_fill_manual(values = c("red", "green", "blue"))

##'#FFFFFF'으로 색상을 정할 수 있음(헥스)
## google에 색상 선택 도구 검색
ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar() +
  scale_fill_manual(values = c("#0341fc", "#8037ed", "#a855b5"))






###산점도 그래프에서 점들에 이름 뜨게 하는 방법
install.packages("ggrepel")
library(ggrepel)

mtcars

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = 'red') +
  geom_text_repel(aes(label = rownames(mtcars)))

rownames(mtcars)

##칼럼 이름을 rowname으로 새로운 칼럼 만들기
#방법 1
mtcars %>% mutate(rowname = rownames(mtcars))

#방법 2
mtcars$rowname <- rownames(mtcars)
mtcars


ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = 'red') +
  geom_text_repel(aes(label = rowname)) +
  theme_bw()


library(ggthemes)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = 'red') +
  geom_text_repel(aes(label = rowname)) +
  theme_wsj()

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(color = 'red') +
  geom_text_repel(aes(label = rowname)) +
  theme_economist_white()





mpg %>% group_by(manufacturer) %>% 
  summarise(연비 = mean(cty),
            배기량 = mean(displ)) %>% 
 ggplot(aes(x = 연비, y = 배기량)) + geom_point() +
  geom_text_repel(aes(label = manufacturer))


mtcars

summary(mtcars)

str(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)

mtcars %>% group_by(cyl) %>% 
  summarise(drat.m = mean(drat)) %>% 
  ggplot(aes(x = cyl, y = drat.m, fill = cyl)) + geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = "Set3")


mtcars %>% ggplot(aes(x = rowname, y = mpg, color = cyl)) + geom_point() +
  geom_text_repel(aes(label = cyl)) +
  theme_clean()

mtcars %>% ggplot(aes(x = cyl, y = wt)) + geom_boxplot()  

#####복지패널 데이터
####성별에 따른 월급 차이
###ctrl + c : Run 중단



###SPSS 데이터를 읽기 위해서 foreign 패키지 필요
install.packages("foreign")
library(foreign)

library(ggplot2)
library(dplyr)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav")
raw_welfare

###복사본 만들기
###spss 형식이기 때문에 읽기 힘듦
###복사하면서 data frame 형식으로 바꿔주기
welfare <- as.data.frame(raw_welfare)

str(welfare)

glimpse(welfare)

head(welfare, 2)

tail(welfare)

summary(welfare)

dim(welfare)


#변수의 개수(column의 개수)
ncol(welfare)

dim(welfare)[2]

###변수 이름 바꾸기
#필요한 변수들만 남기기(용량 줄어듦)
welfare <- welfare %>% 
  rename(gender = h10_g3, birth = h10_g4,
         marriage = h10_g10, religion = h10_g11,
         income = p1002_8aq1, job = h10_eco9,
         region = h10_reg7) %>% 
  select(gender, birth, marriage, religion, income,
         job, region)

View(welfare)

###변수 이름 바꾸기 방법 2
welfare <- welfare %>% 
  select(gender = h10_g3, birth = h10_g4,
         marriage = h10_g10, religion = h10_g11,
         income = p1002_8aq1, job = h10_eco9,
         region = h10_reg7)


###데이터 살펴보기
str(welfare)

glimpse(welfare)

head(welfare, 2)

tail(welfare)

summary(welfare) #결측치도 확인하기

dim(welfare)

plot(welfare) #용량 너무 크면 확인하고 지워버리기

pairs(job ~ income + gender + region, data = welfare) #plot 그래프 중 일부만 

##개인적으로 데이터를 파악할 때 plot, pairs 함수 유용

boxplot(welfare)

boxplot(welfare$income)

boxplot(welfare$income, welfare$job)

###결측치 확인하기
boxplot(welfare)

sum(is.na(welfare))

colSums(is.na(welfare))

summary(welfare$income)

mean(welfare$income) #NA의 존재 확인
mean(welfare$income, na.rm = T)

range(welfare$income)
range(welfare$income, na.rm = T)

min(welfare$income, na.rm = T)
max(welfare$income, na.rm = T)


#NA는 비교 안되니까 주의
welfare$income <- ifelse(is.na(welfare$income), 0, welfare$income)

welfare$income <- ifelse(welfare$income == 0, NA, welfare$income)

welfare$income[is.na(welfare$income)] <- 0 

summary(welfare$income)


###시각화로 살펴보기
boxplot(welfare$income)

plot(welfare$income)

ggplot(welfare, aes(x = income)) + geom_density()

#gender가 숫자형이므로 density 그래프에 남/여 두 개가 나오지 않음
ggplot(welfare, aes(x = income, color = gender)) + geom_density()

ggplot(welfare, aes(x = income, color = as.factor(gender))) + geom_density()

#비슷해보이지만 density는 밀도고 freqpoly는 빈도수를 count한 것
ggplot(welfare, aes(x = income, color = as.factor(gender))) + geom_freqpoly()



###성별로 이름 붙이기
summary(welfare$gender)

welfare$gender <- ifelse(welfare$gender == 1, "Male", "Female")

#table은 data frame이 아니므로 table로 그래프를 그리면 나오지 않음
table(welfare$gender)

mode(table(welfare$gender))
typeof(table(welfare$gender))

gender <- as.data.frame(table(welfare$gender))
gender %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) + geom_col() +
  labs(x = "성별", y = "수입")

##column명 바꾸기
#방법 1
names(gender) <- c("성별", "수입")

#방법2
colnames(gender) <- c("성별", "수입")

#방법3
rename() 이용

gender


###성별 인원수
ggplot(welfare, aes(x = gender, fill = gender)) + geom_bar()

###dplyr를 이용하여 그룹별로 평균내기
welfare %>% group_by(gender) %>%
  summarise(mean = mean(income, na.rm = T))

welfare %>% filter(!is.na(income)) %>% 
  group_by(gender) %>% summarise(mean = mean(income))


welfare %>% group_by(gender) %>%
  summarise(mean = mean(income, na.rm = T)) %>% 
  ggplot(aes(x = gender, y = mean, fill = gender)) + geom_col()

 








