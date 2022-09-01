library(ggplot2)
library(dplyr)

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "red")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

ggplot(mpg, aes(displ, cty, shape = cty)) +
  geom_point()

g <- ggplot(data = mpg, aes(x = displ, y= hwy, color = drv)) + 
  geom_point(size = 4)

g + facet_grid(drv ~ .)
g + facet_grid(. ~ drv)
g + facet_grid(drv ~ cyl)


ggplot(mpg, aes(x = displ, y = hwy, color = drv, shape, drv)) +
  geom_point(size = 4) + facet_grid(drv ~ .)


g + facet_wrap( ~ class)
g + facet_wrap(vars(class), nrow = 6)

g + facet_wrap( ~ drv)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 4, aes(color = drv)) +
  geom_jitter(position = 'jitter')

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 4, aes(color = drv)) +
  geom_jitter(width = 0.5, height = 0.5)


ggplot(mpg, aes(x = drv, fill = as.factor(drv))) +
         geom_bar()

ggplot(mpg, aes(x = drv, fill = as.factor(drv))) +
         geom_bar(position = 'fill')

ggplot(mpg, aes(x = drv, fill = as.factor(class))) +
  geom_bar(position = 'dodge')
       

ggplot(data = mpg, aes(x = class)) + 
  geom_histogram(stat = 'count')



mpg
##제조사 별로, 구동방식에 따른 도시주행연비의 평균을 막대 그래프로 나타내기
str(mpg)

mpg %>% group_by(manufacturer, drv) %>% summarise(mean_cty = mean(cty)) %>% 
  ggplot(aes(x = manufacturer, y = mean_cty, fill = drv)) + geom_col(position = 'dodge')

ggplot(mpg, aes(displ, cty, size = cty)) +
  geom_point(aes(color = cty))





















