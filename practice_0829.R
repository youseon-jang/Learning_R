mtcars
str(mtcars)

ggplot(mtcars, aes(x = mpg, fill = cyl)) + geom_density() +
  theme(legend.position = 'none')

library(plotly)

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/school_earnings.csv")

fig <- plot_ly(data, x = ~Women, y = ~Men, text = ~School, 
               type = 'scatter', mode = 'markers',
               marker = list(size = ~Gap, opacity = 0.5))

fig <- fig %>% layout(title = 'Gender Gap in Earnings per University',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))

fig


str(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)

g <- ggplot(mtcars, aes(x = cyl, fill = cyl)) + geom_bar()

g + scale_fill_hue(c = 30)
g + scale_fill_hue(h = 20, c = 60, l = 10)

g + scale_fill_brewer("Set3")

g + scale_fill_manual(values = c("blue", "violet", "red"))

library(ggrepel)

mtcars %>% mutate(rowname = rownames(mtcars))
str(mtcars)



library(ggplot2)
library(dplyr)

#제조사 별 배기, 연비
mtcars %>% group_by(cyl) %>% 
  summarise(배기 = mean(disp),
            연비 = mean(hp)) %>% 
  ggplot(aes(x = 연비, y = 배기)) + geom_point() +
  geom_text_repel(aes(label = cyl))




install.packages("foreign")
library(foreign)

library(ggplot2)
library(dplyr)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav")

welfare <- as.data.frame(raw_welfare)


welfare <- welfare %>% 
  select(gender = h10_g3, birth = h10_g4,
         marriage = h10_g10, religion = h10_g11,
         income = p1002_8aq1, job = h10_eco9,
         region = h10_reg7)

welfare



str(welfare)
glimpse(welfare)
summary(welfare)

welfare$income <- ifelse(welfare$income == 0, NA, welfare$income)


###지역에 따른 income 차이
welfare$region <- as.character(welfare$region)
unique(welfare$region)
str(welfare)

welfare %>% select(region) %>% 
  rename(서울 = 1, 대전 = 2, 강릉 = 3, 양양 = 4, 
         경주 = 5, 전주 = 6, 진주 = 7)
         
welfare %>% group_by(region) %>% 
  summarise(mean_income = mean(income, na.rm = T)) %>% 
  ggplot(aes(x = region, y = mean_income, fill = region)) + 
  geom_col()



welfare$region <- ifelse(welfare$region == 1, "서울", 
                         ifelse(welfare$region == 2, "대전",
                                ifelse(welfare$region == 3, "강릉", 
                                       ifelse(welfare$region == 4, "양양", 
                                              ifelse(welfare$region == 5, "경주", 
                                                     ifelse(welfare$region == 6, "전주", 
                                                            ifelse(welfare$region == 7, "진주", welfare$region)))))))

welfare$region



welfare$income <- ifelse(is.na(welfare$income), 0, welfare$income )


library(ggplot2)
library(dplyr)

mpg %>% group_by(drv) %>% 
  summarise(mean = mean(cty)) %>% 
  ggplot(aes(x = drv, y = mean, fill = drv)) + geom_col()

welfare$gender <- ifelse(welfare$gender == 1, "male", "female")


library(gapminder)
ggplot(gapminder,aes(x=continent,y=lifeExp,fill=continent)) +
  geom_boxplot() + theme(legend.position = 'none')

mtcars
str(mtcars)

###gear 별 개수를 나타내는 막대 그래프
mtcars$gear <- as.factor(mtcars$gear)
ggplot(mtcars, aes(x = gear, fill = gear)) + geom_bar() +
  scale_fill_manual(values = c("red", "blue", "green"))


dim(mtcars)
length(mtcars)
col(mtcars)
ncol(mtcars)
