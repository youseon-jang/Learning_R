#빈도수 - 많이 언급되었다는 것은 긍정적 의미가 있다고 가정
#항상 빈도수가 높다고 긍정적인 것은 아님

####텍스트 마이닝 분석하기
###감성분석
##Twitter로 텍스트 마이닝 하기
package1 <- c("ggplot2", "Rcpp", "dplyr", "ggthemes", "ggmap", "devtools", "RCurl", "igraph", "rgl", "lavaan", "semPlot")
package2 <- c("twitteR", "XML", "plyr", "doBy", "RJSONIO", "tm", "RWeka", "base64enc")
list.of.packages <-  c( package1, package2)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) 

install.packages("twitter")
install.packages("POAuth")

library(twitteR)
library()

library(plyr)
library(stringr) ##문자열 처리 시 사용

# Samsung =======
# 한글 검색 원하면 
# keyword <- enc2utf8("갤럭시)
# gal_tweets <- searchTwitter(keyword, lang = "ko", n = 500)

g_tweets <- searchTwitter("samsung", lang = "en", n = 1000)
samsung_tweets <- g_tweets
save(samsung_tweets, file = "samsung_tweets.rda")

load("samsung_tweets.rda")
samsung_tweets

st <- twListToDF(samsung_tweets)
st
head(st, 1)
names(st)

st_text <- st$text
st_text

st_text <- gsub("\\W", " ", st_text) ##W는 알파벳과 숫자를 제외한 모든 문자
tail(st_text, 10)

st_df <- as.data.frame(st_text)
st_df




# Apple =======
g_tweets <- searchTwitter("apple", lang = "en", n = 1000)
apple_tweets <- g_tweets
save(samsung_tweets, file = "apple_tweets.rda")

load("apple_tweets.rda")
apple_tweets

at <- twListToDF(apple_tweets)
at
head(at, 1)
names(at)

at_text <- at$text
at_text

at_text <- gsub("\\W", " ", at_text) ##W는 알파벳과 숫자를 제외한 모든 문자
tail(at_text, 10)

at_df <- as.data.frame(at_text)
at_df


#감성사전 https://github.com/The-ECG/BigData1_1.3.3_Text-Mining ==========

pos.word <- scan("positive-words.txt", what = "character", comment.char = ";")
neg.word <- scan("negative-words.txt", what = "character", comment.char = ";")
##what = '읽을 데이터 타입'
##comment.char = 주석의 시작 문자 지정

#https://stackoverflow.com/questions/35222946/score-sentiment-function-in-r-return-always-0

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}


samsung_scores <- score.sentiment(st_text, pos.word, neg.word, .progress = "text")
samsung_scores$score
hist(samsung_scores$score)

apple_scores <- score.sentiment(at_text, pos.word, neg.word, .progress = "text")
apple_scores
hist(apple_scores$score)


a <- dim(samsung_scores)[1]
a

b <- dim(apple_scores)[1]
b

alls <- rbind( as.data.frame(cbind(type=rep("samsung",a), 
                                   score = samsung_scores[ , 1])),
               as.data.frame(cbind(type=rep("apple",b), 
                                   score = apple_scores[ , 1])))
###rep(반복 대상, 반복 횟수)
###cbind --> 칼럼 붙임
###rbind --> 행 붙임
###alls: samsung/apple 표시 칼럼과 각각의 score 칼럼으로 구성된 df
###2000 rows

str(alls)

library(ggplot2)

ggplot(alls, aes(x = as.numeric(score), color = type)) + geom_density()


#갤럭시 ===================================================
load("gal_tweets.rda")
gal_tweets

galt <- twListToDF(gal_tweets)
galt
head(galt, 1)
names(galt)

galt_text <- galt$text
galt_text

galt_text <- gsub("\\W", " ", galt_text)
galt_text

galt_df <- as.data.frame(galt_text)
galt_df

pos.kr.word <- scan("positive-words-ko-v2.txt", what = "character", comment.char = ";")
neg.kr.word <- scan("negative-words-ko-v2.txt", what = "character", comment.char = ";")


kr.score.sentiment = function(sentences, pos.kr.words, neg.kr.words, .progress='none'){
  scores = laply(sentences, function(sentence, pos.kr.words, neg.kr.words) {
    sentence = gsub('[^가-힣 ]','', sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.kr.words);
    neg.matches = match(words, neg.kr.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.kr.words, neg.kr.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);}

galt_scores <- kr.score.sentiment(galt_text, pos.kr.word, neg.kr.word, .progress = 'none')
galt_scores
hist(galt_scores$score)

#아이폰 ==========================================================
load("iphone_tweets.rda")
iphone_tweets

iphonet <- twListToDF(iphone_tweets)
iphonet
head(iphonet, 1)
names(iphonet)

iphonet_text <- iphonet$text
iphonet_text

iphonet_text <- gsub("\\W", " ", iphonet_text)
iphonet_text

iphonet_df <- as.data.frame(iphonet_text)
iphonet_df

pos.kr.word <- scan("positive-words-ko-v2.txt", what = "character", comment.char = ";")
neg.kr.word <- scan("negative-words-ko-v2.txt", what = "character", comment.char = ";")


kr.score.sentiment = function(sentences, pos.kr.words, neg.kr.words, .progress='none'){
  scores = laply(sentences, function(sentence, pos.kr.words, neg.kr.words) {
    sentence = gsub('[^가-힣 ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.kr.words);
    neg.matches = match(words, neg.kr.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.kr.words, neg.kr.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);}

iphonet_scores <- kr.score.sentiment(iphonet_text, pos.kr.word, neg.kr.word, .progress = 'none')
iphonet_scores
hist(iphonet_scores$score)

a <- dim(galt_scores)[1]
a

b <- dim(iphonet_scores)[1]
b

alls <- rbind( as.data.frame(cbind(type=rep("galaxy",a), 
                                   score = galt_scores[ , 1])),
               as.data.frame(cbind(type=rep("iphone",b), 
                                   score = iphonet_scores[ , 1])))

ggplot(alls, aes(x = as.numeric(score), color = type)) + geom_density()


####wordcloud
##갤럭시 ===================================================
library(KoNLP)
load("gal_tweets.rda")
gal_tweets

galt <- twListToDF(gal_tweets)
galt
head(galt, 1)
names(galt)

galt_text <- galt$text
galt_text

galt_text <- gt_gsub <- gsub("[^가-힣]"," " ,galt_text)
galt_text

galt_word <- extractNoun(galt_text)
galt_word <- unlist(galt_word)
galt_word <- galt_word[nchar(galt_word) > 1]

df_galt_word <- data.frame(table(galt_word))
df_galt_word
str(df_galt_word)

text_galt_word <- as.data.frame(df_galt_word) %>% arrange(desc(Freq)) %>% 
  filter(Freq < 400) %>% head(100)


library(wordcloud2)
wordcloud2(text_galt_word)


##아이폰 =========================================================
load("iphone_tweets.rda")
iphone_tweets

iphonet <- twListToDF(iphone_tweets)
iphonet
head(iphonet, 1)
names(iphonet)

iphonet_text <- iphonet$text
iphonet_text

iphonet_text <- iphonet_isub <- gsub("[^가-힣]"," " ,iphonet_text)
iphonet_text

iphonet_word <- extractNoun(iphonet_text)
iphonet_word <- unlist(iphonet_word)
iphonet_word <- iphonet_word[nchar(iphonet_word) > 1]
iphonet_word

df_iphonet_word <- table(unlist(iphonet_word))
df_iphonet_word

text_iphonet_word <- as.data.frame(df_iphonet_word) %>% arrange(desc(Freq)) %>% 
  head(50)

library(wordcloud2)
wordcloud2(text_iphonet_word)



#####단순 선형회귀 분석
#종속 변수 Y와 하나의 독립 변수 X와의 선형 상관 관계를 모델링하는 회귀분석기법
ggplot(cars, aes(x = speed, y = dist)) + geom_point()

str(cars)

##lm(linear model)
##lm(y ~ x, data = )

##데이터 살펴보기
data1 <- cars

plot(data1)

pairs(data1)

##상관 분석 
cor(data1) 
#cor():상관행렬(correlation matrix)
#상관 계수 : 0.80


cor.test(data1$speed, data1$dist)
#상관 계수 보여줌
#p-value가 작을수록 유의미함



###모형 적합
fit.cars <- lm(dist ~ speed, data = cars)
fit.cars

str(fit.cars)
names(fit.cars)

summary(fit.cars)



predict(fit.cars, 20) 
##error
##뒤 변수도 data frame 으로 만들어야 함

data.frame(speed = 20)
predict(fit.cars, data.frame(speed = 20))
predict(fit.cars, data.frame(speed = c(20, 120)))

predict(fit.cars, data.frame(speed = 20), interval = "confidence")
predict(fit.cars, data.frame(speed = c(20, 120)), interval = "confidence")

predict(fit.cars, data.frame(speed = 20), 
        interval = "confidence", level = 0.9)



ggplot(mpg, aes(x = displ, y = cty, color = drv)) + geom_point() +
  geom_smooth(method = "lm")








