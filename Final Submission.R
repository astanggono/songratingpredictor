#Best Score Submission
setwd("C:/Users/Asta Adhira Anggono/Desktop/Columbia/Framewoks + Methods/PAC")
analysis = read.csv('analysisData.csv')
scoring = read.csv('scoringData.csv')
library(tidyr)
library(mice)
library(dplyr)

#cleaning analysis data and impute mising values
analysis_cart <- mice(analysis, method = "cart", seed = 1031)
analysis_cart <- complete(analysis_cart)

analysis_cart$genre<-gsub("\\[|\\]","",as.character(analysis_cart$genre))
analysis_cart$genre<-gsub("'","",as.character(analysis_cart$genre))

analysis_cart %<>% mutate(t2 = genre) %>% separate_rows(t2, sep = ",")
analysis_cart$t2 <- trimws(analysis_cart$t2)

analysis_cart$t2 <- sub("-","_",as.character(analysis_cart$t2))

analysis_cart$t2 <- sub("-","_",as.character(analysis_cart$t2))

analysis_cart$t2 <- sub("-","_",as.character(analysis_cart$t2))

analysis_cart$t2 <- sub("-","_",as.character(analysis_cart$t2))

analysis_cart$t2 <- sub(" ","_",as.character(analysis_cart$t2))

analysis_cart$t2 <- sub(" ","_",as.character(analysis_cart$t2))

analysis_cart$t2 <- sub(" ","_",as.character(analysis_cart$t2))

analysis_cart$t2 <- gsub("[[:punct:]]", "", as.character(analysis_cart$t2))
analysis_cart$t2

analysis_cart = analysis_cart %>% mutate(pa = 1)
analysis_cart$t2[analysis_cart$t2 == ""] <- "no_genre"

analysis_cart = analysis_cart %>%  pivot_wider(names_from = t2, values_from = pa, values_fill = 0)

analysis_model = analysis_cart %>% select(-(2:4))
analysis_model$track_explicit <- as.integer(as.logical(analysis_model$track_explicit))

#cleaning scoring data and impute missing values
library(mice)
scoring_cart <- mice(scoring, method = "cart", seed = 1031)
scoring_cart <- complete(scoring_cart)

scoring_cart$genre<-gsub("\\[|\\]","",as.character(scoring_cart$genre))
scoring_cart$genre<-gsub("'","",as.character(scoring_cart$genre))

scoring_cart %<>% mutate(t2 = genre) %>% separate_rows(t2, sep = ",")
scoring_cart$t2 <- trimws(scoring_cart$t2)

scoring_cart$t2 <- sub("-","_",as.character(scoring_cart$t2))

scoring_cart$t2 <- sub("-","_",as.character(scoring_cart$t2))

scoring_cart$t2 <- sub("-","_",as.character(scoring_cart$t2))

scoring_cart$t2 <- sub("-","_",as.character(scoring_cart$t2))

scoring_cart$t2 <- sub(" ","_",as.character(scoring_cart$t2))

scoring_cart$t2 <- sub(" ","_",as.character(scoring_cart$t2))

scoring_cart$t2 <- sub(" ","_",as.character(scoring_cart$t2))

scoring_cart$t2 <- gsub("[[:punct:]]", "", as.character(scoring_cart$t2))
scoring_cart$t2

scoring_cart = scoring_cart %>% mutate(pa = 1)
scoring_cart$t2[scoring_cart$t2 == ""] <- "no_genre"

scoring_cart = scoring_cart %>%  pivot_wider(names_from = t2, values_from = pa, values_fill = 0)

scoring_model = scoring_cart %>% select(-(2:4))
scoring_model$track_explicit <- as.integer(as.logical(scoring_model$track_explicit))

#Making both dimension of analysis and scoring data the same
#to get predictors applicable to scoring data
cols_to_keep <- intersect(colnames(analysis_model),colnames(scoring_model))
X <- analysis_model[,cols_to_keep, drop=FALSE]
Y <- scoring_model[,cols_to_keep, drop=FALSE]
X2 <- X1
X2$rating <- analysis_model$rating #putting back rating column

#split cleaned genre data
set.seed(1031)
split = createDataPartition(y=X2$rating,p = 0.7,list = F,groups = 100)
train2 = X2[split,]
test2 = X2[-split,]


start_mod = lm(rating~1, data = train2)
empty_mod = lm(rating~1, data = train2)
full_mod = lm(rating~., data = train2)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')

forest_ranger = ranger(rating ~ pop + no_genre + acousticness + rock + track_explicit + 
                         southernsoul + track_duration + loudness + energy + danceability + 
                         poprock + contemporarycountry + softrock + rhythmandblues + 
                         postgrunge + dfwrap + quietstorm + chicagorap + bluesrock + 
                         vocaljazz + dancepop + deepadultstandards + modernrock + 
                         garagerock + electronicrock + deeptalentshow + southernhiphop + 
                         rap + jumpblues + adultstandards + synthpop + electronictrap + 
                         instrumentalness + bubblegumpop + countrydawn + bubblegumdance + 
                         deepnewwave + turntablism + beachmusic + postdisco + blues + 
                         time_signature + britishsoul + trap
                       , data = train, num.trees = 1000)

#RMSE for train and test data
pred_test_fr = predict(forest_ranger, data = test, num.trees = 1000)
pred_train_fr = predict(forest_ranger, data = train, num.trees = 1000)
sqrt(mean((pred_test_fr$predictions - test$rating)^2))
sqrt(mean((pred_train_fr$predictions - train$rating)^2))

#run model on scoring data
pred_scoring_fr = predict(forest_ranger, data = scoring_model, num.trees = 1000)

#submission
submission23 = data.frame(id = scoring_model$id, rating = pred_scoring_fr)
names(submission23) = c("id","rating")
write.csv(submission23, 'submission23.csv', row.names = F)


