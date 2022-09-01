library(foreign)

raw_welfare <- read.spss("Koweps_hpc10_2015_beta1.sav")

welfare <- as.data.frame(raw_welfare)

welfare <- welfare %>% 
  rename( gender = h10_g3, birth = h10_g4, 
          marriage = h10_g10, religion = h10_g11, 
          income = p1002_8aq1, job = h10_eco9, 
          region = h10_reg7) %>% 
  select(gender, birth, marriage, religion, 
         income, job, region )

welfare

str(welfare)

#####나이에 따른 소득 차이
welfare <- welfare %>% 
  mutate(age = 2014 - birth + 1)

age_income <- welfare %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income, na.rm = T))

age_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age) %>% summarise(mean_income = mean(income, na.rm = T))

age_income

ggplot(age_income, aes(x = age, y = mean_income)) + geom_line()

#####연령대 별 평균소득
welfare <- welfare %>% 
  mutate(age_gen = ifelse(age < 30, "young", 
                          ifelse(age <= 50, "old", "middle")))

str(welfare)

age_gen_income <- welfare %>% 
  group_by(age_gen) %>% 
  summarise(mean_income = mean(income, na.rm = T))

age_gen_income

ggplot(age_gen_income, aes(x = age_gen, y = mean_income, fill = age_gen)) +
  geom_col() + scale_x_discrete(limits = c("young", "middle", "old"))

#####나이와 성별에 따른 소득 차이
welfare$gender <- as.character(welfare$gender)

age_gender <- welfare %>% 
  group_by(age_gen, gender) %>% 
  summarise(mean_income = mean(income, na.rm = T))

age_gender <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age_gen, gender) %>% 
  summarise(mean_income = mean(income))

age_gender

ggplot(age_gender, aes(x = age_gen, y = mean_income, fill = gender)) +
  geom_col(position = 'dodge')

##선 그래프
gen_age <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age, gender) %>% 
  summarise(mean_income = mean(income))

gen_age

ggplot(gen_age, aes(x = age, y= mean_income, color = gender)) +
  geom_line() + geom_point()

#####성별에 따른 월급 차이
gender_income_bar <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(gender) %>% 
  summarise(mean_income = mean(income))

gender_income_bar

ggplot(gender_income_bar, aes(x = gender, y = mean_income, fill = gender)) +
  geom_col()


library(gapminder)
library(dplyr)
library(ggplot2)
gapminder

str(gapminder)

gapminder1 <- gapminder %>% filter(year == 2007)
gapminder1$gdpPercap <- ifelse(gapminder1$gdpPercap < 6500, "개발도상국", 
                     ifelse(gapminder1$gdpPercap < 23000, "신흥국", "선진국"))
gapminder1

#scale_x_discrete(limits = c(""))

ggplot(gapminder1, aes(x = gdpPercap, y = lifeExp, fill = gdpPercap)) +
  geom_col() + labs(x = "국가분류", y = "기대수명") +
  scale_x_discrete(limits = c(""))


mpg
str(mpg)

unique(mpg$model)

mpg1 <- mpg %>% filter(model == "toyota tacoma 4wd") %>% group_by(model) %>% 
  summarise(범위 = range(displ))

range(mpg1$displ)

range(mpg[mpg$model == "toyota tacoma 4wd", ]$displ)


table(mpg$manufacturer)
df <- as.data.frame(table(mpg$manufacturer))

library(wordcloud2)
wordcloud2(df)
