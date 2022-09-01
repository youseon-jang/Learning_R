#####나이에 따른 소득 차이
library(foreign)
library(ggplot2)
library(dplyr)

raw_welfare <- read.spss("Koweps_hpc10_2015_beta1.sav")


welfare <- as.data.frame(raw_welfare) 

welfare <- welfare %>% 
  rename( gender = h10_g3, birth = h10_g4, 
          marriage = h10_g10, religion = h10_g11, 
          income = p1002_8aq1, job = h10_eco9, 
          region = h10_reg7) %>% 
  select(gender, birth, marriage, religion, 
         income, job, region )

####데이터 살펴보기
class(welfare$birth)

summary(welfare$birth)

qplot(welfare$birth) #사용 잘 안함

###이상치 확인
boxplot(welfare$birth)

###결측치 확인
sum(is.na(welfare$birth))


###나이 column 만들기
welfare <- welfare %>% mutate(age = (2014 - birth + 1))

welfare$age <- 2014 - welfare$birth + 1

summary(welfare$age)
str(welfare)

###데이터 정리하기
welfare$income

age_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age) %>% summarise(mean_income = mean(income))

age_income

welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income)) %>% 
  ggplot(aes(x = age, y = mean_income)) + geom_line()


###그림으로 보여주기 geom_line
welfare %>% filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(평균소득 = mean(income)) %>% 
  ggplot(aes(x = age, y = 평균소득)) +
  geom_line(color = "skyblue", size = 1) +
  geom_point(color = 'red', size = 2)

###To Add
age_income <- welfare %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income, na.rm = T))

age_income <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age) %>% summarise(mean_income = mean(income)
#둘 중 위의 방법으로 해도 그래프 그려짐
#BUT 중간중간 mean_income이 0인 없는 값들이 있어서 그래프가 끊김
#NaN은 Not a Number의 줄임말말
#따라서 아래의 방법으로 age_income을 설정하는 것이 보기 좋음음




#####연령대 별 평균소득
welfare <- welfare %>% 
  mutate(age_gen = ifelse(age < 30, "young", 
                          ifelse(age <= 50, "middle", "old")))

head(welfare)

###방법 1
welfare %>% group_by(age_gen) %>% 
  summarise(gen_mean = mean(income, na.rm = T)) %>% 
  ggplot(aes(x = age_gen, y = gen_mean, fill = age_gen)) + geom_col()

###방법 2  
welfare %>% filter(!is.na(welfare$income)) %>% 
  group_by(age_gen) %>% 
  summarise(mean_income= mean(income)) %>% 
  ggplot(aes(x = age_gen, y = mean_income, fill = age_gen)) + geom_col()

###방법 3
age_gen_income <- welfare %>% 
  group_by(age_gen) %>% 
  summarise(mean_income = mean(income, na.rm = T))

age_gen_income

ggplot(age_gen_income, aes(x = age_gen, y = mean_income, fill = age_gen)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))
##scale_x_discrete()로 x축 변수 배치 가능




#####나이와 성별에 따른 소득 차이
###방법 1
welfare$gender <- as.character(welfare$gender)

gender_income <- welfare %>% 
  group_by(age_gen, gender) %>% 
  summarise(mean_income = mean(income, na.rm = T))

gender_income

ggplot(gender_income, 
       aes(x = age_gen, y = mean_income, fill = gender)) +
  geom_col(position = 'dodge')


###방법 2
welfare %>% group_by(age_gen, gender) %>% 
  summarise(mean_income = mean(income, na.rm = T)) %>% 
  ggplot(aes(x = age_gen, y = mean_income, fill = as.character(gender))) +
  geom_col(position = 'dodge')

###summarise 이후에도 유지하려면 .groups = "drop_last"


####tidyverse
####다양한 패키지들 한 번에 설치
install.packages("tidyverse")


###선 그래프
gender_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, gender) %>% 
  summarise(mean_income = mean(income))

gender_age

ggplot(gender_age, aes(x = age, y = mean_income, color = gender)) + geom_line()


###To Add
welfare$gender <- as.character(welfare$gender)

#1
age_gender <- welfare %>% 
  group_by(age_gen, gender) %>% 
  summarise(mean_income = mean(income, na.rm = T))

#2
age_gender <- welfare %>% filter(!is.na(income)) %>% 
  group_by(age_gen, gender) %>% 
  summarise(mean_income = mean(income))

age_gender

ggplot(age_gender, aes(x = age_gen, y = mean_income, fill = gender)) +
  geom_col(position = 'dodge')


###위와 마찬가지로 1을 사용해도 그래프는 나옴
###BUT age_gender 데이터 프레임 실행하면 값이 없는 것들이 많음
###따라서 filter을 한 후에 나머지를 실행시키는 것이 좋을 것 같음



#####워드클라우드
###KoNLP(Korean Natural Language Processing)
###자연어 : 일반적인 언어
###형식어 : 1 + 2 = 2, CO2 등과 같이 형식이 정해져있는 언어

read.csv("ahn.txt", fileEncoding = "cp949")
###fileEncoding = "cp949" 윈도우에 저장해서 띄어줌(한글 지원)
Sys.setlocale("LC_ALL","korean")

text <- readLines("ahn.txt")
text <- read.table("ahn.txt", encoding = "UTF-8")
###불러올 파일이 UTF-8파일이 아니면 encoding = "UTF-8" 추가

text

exNoun <- extractNoun(text)
exNoun <- unlist(exNoun)

exNoun1 <-exNoun[nchar(exNoun) > 1]
exNoun1
exNoun1 <- as.data.frame(table(exNoun1)) %>% arrange(desc(Freq)) %>% 
  head(30)
exNoun1
wordcloud2(exNoun1)


###KoNLP설치
#java, rjava설치
install.packages("multilinguer")
library(multilinguer)
install_jdk()
2

#의존성 패키지 설치
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex",
                   "lazyyeval", "htmlwidgets", "crosstalk", "promises",
                   "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", 
                   "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions",
                   type = "binary"))



#github 설치
install.packages("remotes")

#KoNLP 설치
remotes::install_github('haven-jeon/KoNLP', 
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))

#텍스트
library(KoNLP)

extractNoun('이 문장에서 명사만 추출되었다면 성공입니다.')


useNIADic()
1

useSejongDic()

extractNoun(text)

nouns <- extractNoun(text)
nouns

nchar(nouns)
#character의 개수를 세어줌 (글자의 수를 세어줌)
#보통 1글자는 의미 없기 때문에 확인용으로 개수를 셈

nouns1 <- nouns[nchar(nouns) > 1]
###T/F 를 [] 안에 넣으면 해당 조건이 T인 값들만 나타남

table(nouns1)

textdf <- as.data.frame(table(nouns1)) %>%  arrange(desc(Freq)) %>% 
  head(30)

textdf

ggplot(textdf, aes(x = nouns1, y = Freq, fill = nouns1)) + geom_col() +
  coord_flip()
###coord_flip() 추가하면 축의 위치를 바꿔줌



###wordcloud로 표현하기
install.packages("wordcloud2")
library(wordcloud2)

wordcloud2(textdf)
###data는 단어와 빈도수가 각 칼럼에 나와있는 dataframe


news <- readLines("news.txt", encoding = "UTF-8")
news

extractNoun(news)
news1 <- unlist(extractNoun(news))
news1

news1 <- news1[nchar(news1) > 1]
news1

table(news1)

newsdf <- as.data.frame(table(news1)) %>% arrange(desc(Freq)) %>% 
  head(30)
newsdf

wordcloud2(newsdf)

newsdf <- as.data.frame(table(news1)) %>% filter(Freq >= 5)



