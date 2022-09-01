###텍스트 마이닝 분석하기
###galaxy
library(twitteR)
library(plyr)
library(stringr)

load("gal_tweets.rda")
gal_tweets

galt <- twListToDF(gal_tweets)
galt
galt(head)

galt_text <- galt$text

galt_gsub <- gsub("[^가-힣]"," " ,galt_text)
head(galt_gsub, 10)

dt_galt_gsub <- data.frame(galt_gsub)
dt_galt_gsub


###iphone
load("iphone_tweets.rda")
iphone_tweets

iphonet <- twListToDF(iphone_tweets)
head(iphonet, 1)

iphonet_text <- iphonet$text

iphonet_gsub <- gsub("[^가-힣]"," " ,iphonet_text)
head(iphonet_gsub, 1)

dt_iphonet_gsub <- data.frame(iphonet_gsub)
dt_iphonet_gsub

###감성 사전
ko.pos.word <- scan("positive-words-ko-v2.txt", what = "character", comment.char = ";")
ko.neg.word <- scan("negative-words-ko-v2.txt", what = "character", comment.char = ";")



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


galaxy_score <- kr.score.sentiment(galt_gsub, ko.pos.word, ko.neg.word, .progress = "none")
galaxy_score$score
hist(galaxy_score$score)

iphone_score <- kr.score.sentiment(iphonet_gsub, ko.pos.word, ko.neg.word, .progress = "none")
iphone_score$score
hist(iphone_score$score)

a1 <- dim(galaxy_score)[1]
b1 <- dim(iphone_score)[1]

alls1 <- rbind( as.data.frame(cbind(type=rep("galaxy",a1), 
                                   score = galaxy_score[ , 1])),
               as.data.frame(cbind(type=rep("iphone",b1), 
                                   score = iphone_score[ , 1])))
head(alls1)

ggplot(alls1, aes(x = as.numeric(score), color = type)) + geom_density()

#############################
###수강 과목 별 나의 참여도 bar chart
library(readxl)
library(dplyr)
read_excel("수강설문조사정리.xlsx")

survey <- read_excel("수강설문조사정리.xlsx")
survey <- as.data.frame(survey)

str(survey)
glimpse(survey)
colnames(survey)
View(survey)
unique(survey$학습태도)

colnames(survey) <- c("id", "수강과목", "줌", "강의시간", "강의운영",
                      "중간과제", "강사", "참여도", "수업진도", 
                      "additional", "interest")
str(survey)

survey$참여도 <- ifelse(survey$참여도 == "매우 동의함", 5,
                      ifelse(survey$참여도 == "동의함", 4, 
                             ifelse(survey$참여도 == "보통", 3,
                                    ifelse(survey$참여도 == "동의하지 않음", 2, 1))))

survey1 <- survey %>% select(참여도, 수강과목)
survey1

ggplot(survey1, aes(x = 참여도, fill = 수강과목)) +
  geom_bar(position = 'dodge') +
  labs(y = "학생수")
  





















