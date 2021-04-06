## Clean Workbook
library(caret)
library(tidyverse)
library(vroom)



data <- read.csv("./CleanFakeNEws.csv")



data$isFake <- as.factor(data$isFake)

#adding sentiment analysis data

data <- data[-c(6548, 6558, 12938),]

isFake.new.dir <- isFake.new %>% mutate(dir = convertToDirection(isFake.new$SentimentQDAP))

isFake.new.dir <- isFake.new.dir %>% mutate(bin = convertToBinaryResponse(isFake.new$SentimentQDAP))

isFake.new <- isFake.new %>%
  replace(is.na(.), 0)

data <- data_frame(data, isFake.new)


fake.model <- train(form=isFake ~ .,
                    data = data %>% filter(Set == "train"),
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

# fake.model <- train(form=isFake~.,
#                     data = data %>% filter(Set == "train"),
#                     method="knn",
#                     tuneGrid = expand.grid(k=3),
#                     trControl=trainControl(method='repeatedcv', number = 20, repeats = 2),
#                     verboseIter= T)

    
preds <- predict(fake.model, newdata=data %>% 
                  filter(Set=='test'))

submission <- data.frame(id=data %>% filter(Set=='test') %>%
                             pull(Id),
                         label=preds)
                         
write.csv(submission, 'submission.csv', row.names = FALSE)
