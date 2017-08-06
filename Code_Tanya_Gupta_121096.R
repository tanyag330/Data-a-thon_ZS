#set path

getwd()
setwd("/Users/home/Desktop/Datathon")

#install packages
install.packages("dplyr")
install.packages("stringr")

# load library
library(dplyr)
library(stringr)
library(data.table)
library('randomForest')
library('caret')
library('Metrics')
library('mice')


## Reading the Training data into R

zs_train <- read.csv("Training_Data.csv",header = T, stringsAsFactors = F)
zs_test <- read.csv("Testing_Data.csv",header = T, stringsAsFactors = F)

head(zs_train)
head(zs_test)

str(zs_train)
str(zs_test)

#checking data
summary(zs_train)
summary(zs_test)



#writing queries
t<-zs_train[!duplicated(zs_train[,c('Title','TRANS_CONV_TEXT')]),]
h <- left_join(zs_test, t)

sum(is.na(t))
mice_mod <- mice(t[, ], method='rf')
mice_output <- complete(mice_mod)
t <- mice_output
sum(is.na(t))

t$Title<-as.factor(t$Title)
t$TRANS_CONV_TEXT<-as.factor(t$TRANS_CONV_TEXT)


terms      <- c('I', 'me', 'my')
term_regex <- paste0('(', paste(terms, collapse = '|'), ')')

#Import the list of Keywords with first column as the keyword to match

tags <- as.character(zs_train$TRANS_CONV_TEXT)
for (i in 1:length(term_regex)) {
  for (j in 1:nrow(zs_test)) {
    #zs_train is the complete data from which you are trying to extract the text.
    if(grepl(term_regex[i],zs_test[j,1]) == 1){zs_test[j,8] <- tags[i]
    #Here is where you do an actual search
    zs_test[j,9] <- 1
    #Flag 1 to those observations where you find a match
    }
  }
}

pre<-c('Title','TRANS_CONV_TEXT')
name<-c('Output')
model_gbm<-zs_train(all.imp[,pre],all.imp[,name],method='gbm')
pred <- predict(object=model_gbm, solution[,pre])
summary(pred)



s$Output<-ifelse(pred,1,0)


zs_test["Output"] <- NA 
zs_test["Output"] <-ifelse(zs_test[j,9],1,0)

fwrite(zs_test,"sol.csv", row.names = F)