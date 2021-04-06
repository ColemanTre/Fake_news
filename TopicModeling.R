library(tidyr)
library(tidytext)
library(topicmodels)
library(SentimentAnalysis)
library(glmnet)

#huggingface, gtp2, burt


fakeNews.train <- read_csv("./train.csv")
fakeNews.test <- read_csv("./test.csv")
fakeNews <- bind_rows(train=fakeNews.train, test=fakeNews.test,
                      .id="Set")


# # tokenize by n-gram
# fakeNews_w_bigrams <- fakeNews.train %>%
#   select(id,text,label) %>%
#   unnest_tokens(bigram, text, token = "ngrams", n = 2)
# 
# # which bigrams happen the most
# fakeNews_w_bigrams %>%
#   count(bigram, sort = TRUE)
# 
# # to get rid of stop words, we are using Tidyr's separate()
# bigrams_separated <- total_w_bigrams %>%
#   tidyr::separate(bigram, c("word1", "word2"), sep = " ")


# d <- tibble(txt = fakeNews.train$text)
# d %>%
#   unnest_tokens(sentence, txt, token = "sentences")
# 
# unnestfakenews <- fakeNews.train %>% unnest_tokens(sentence, fakeNews.train$text, token = "sentences")

fakenews.other <- fakeNews %>% filter(language == c('german', 'spanish', 'Missing', 'Other', 'french',
                                                    'italian', 'portuguese'))

fakenews.eng <- fakeNews %>% filter(language == 'english')

text <- fakeNews$text[-c(6548, 6558, 12938)]

sentiment1 <- analyzeSentiment(text[0:1000])
sentiment2 <- analyzeSentiment(text[1001:2000])
sentiment3 <- analyzeSentiment(text[2001:3000])
sentiment4 <- analyzeSentiment(text[3001:4000])
sentiment5 <- analyzeSentiment(text[4001:5000])
sentiment6 <- analyzeSentiment(text[5001:6000])
sentiment7 <- analyzeSentiment(text[6001:7000])
sentiment9 <- analyzeSentiment(text[7001:8000])
sentiment10 <- analyzeSentiment(text[8001:10000])
sentiment11 <- analyzeSentiment(text[10001:12000])
sentiment12 <- analyzeSentiment(text[12001:13000])
sentiment13 <- analyzeSentiment(text[13001:14000])
sentiment14 <- analyzeSentiment(text[14001:15000])
sentiment15 <- analyzeSentiment(text[15001:16000])
sentiment16 <- analyzeSentiment(text[16001:17000])
sentiment17 <- analyzeSentiment(text[17001:18000])
sentiment18 <- analyzeSentiment(text[18001:19000])
sentiment8 <- analyzeSentiment(text[19001:20000])
sentiment19 <- analyzeSentiment(text[20001:21000])
sentiment20 <- analyzeSentiment(text[21001:22000])
sentiment21 <- analyzeSentiment(text[22001:23000])
sentiment22 <- analyzeSentiment(text[23001:24000])
sentiment23 <- analyzeSentiment(text[24001:25000])
sentiment24 <- analyzeSentiment(text[25001:25997])



sentiment0 <- analyzeSentiment(text[6550:6560])

isFake.new <- bind_rows(sentiment1, sentiment2, sentiment3, sentiment4, sentiment5, sentiment6, sentiment7, sentiment8, sentiment9,
          sentiment10, sentiment11, sentiment12, sentiment13, sentiment14, sentiment15, sentiment16, sentiment17,
          sentiment18, sentiment19, sentiment20, sentiment21, sentiment22, sentiment23, sentiment24)

fakeNews2 <- fakeNews$text[-c(6548, 6558, 12938)]

#isFake.new2 <- convertToDirection(isFake.new)
isFake.sent <- data.frame(isFake.new, fakeNews[, c('Set', 'id', 'label')])
isFake.sent$label <- as.factor(isFake.sent$label)

isFake.sent.dir <- isFake.sent %>% mutate(dir = convertToDirection(isFake.sent$SentimentQDAP))

isFake.sent.dir <- isFake.sent.dir %>% mutate(bin = convertToBinaryResponse(isFake.sent$SentimentQDAP))



fake.model.sent <- train(form=label ~ WordCount + SentimentGI + NegativityGI + PositivityGI + SentimentHE + NegativityHE +
                           SentimentLM + NegativityLM + PositivityLM + RatioUncertaintyLM + SentimentQDAP + NegativityQDAP +
                           PositivityQDAP,
                    data = na.exclude(isFake.sent %>% filter(Set == "train")),
                    method="xgbTree",
                    tuneGrid = expand.grid(nrounds = 150,
                                           max_depth = 3,
                                           eta =  .5,
                                           gamma = .3,
                                           colsample_bytree = .6,
                                           min_child_weight = .6,
                                           subsample = 1),
                    trControl=trainControl(method='repeatedcv', number = 20, repeats = 2),
                    verboseIter= T)

# 
# 
# fake.model <- train(form=isFake~.,
#                     data = data %>% filter(Set == "train"),
#                     method="knn",
#                     tuneGrid = expand.grid(k=3),
#                     trControl=trainControl(method='repeatedcv', number = 20, repeats = 2),
#                     verboseIter= T)

# fake.logistic <- glm(label ~ WordCount + SentimentGI + NegativityGI + SentimentHE + NegativityHE +
#                         SentimentLM + NegativityLM  + RatioUncertaintyLM + SentimentQDAP + NegativityQDAP,
#                       data = isFake.sent,
#                       family = binomial(link = "logit"))
# 
# summary(fake.logistic)



preds <- predict(fake.model.sent, newdata = isFake.sent %>% select(-label) %>% filter(Set=='test') %>% replace(is.na(.), 0))

submission <- data.frame(id=fakeNews.test %>% pull(id),
                         label=preds)

write.csv(submission, 'submission.csv', row.names = FALSE)
