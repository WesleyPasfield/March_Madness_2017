setwd('~/Documents/Kaggle/MarchMadness/2017/Modeling_Data/March_Madness_2017')

library(data.table)

## Read in core information about tourney & regular season games

regSeasonComp <- fread('regularSeasonCompactResults.csv', header = T, stringsAsFactors = F)
regSeasonComp$type <- 'Regular'
tournComp <- fread('TourneyCompactResults.csv', header = T, stringsAsFactors = F)
tournComp$type <- 'Tourney'
gameStack <- as.data.frame(rbind(regSeasonComp, tournComp))

## Read in stacked data - stacked manually in excel from detailed data provided by Kaggle

regSeasonStack <- fread('regSeasonStack.csv', header = T, stringsAsFactors = F)
regSeasonStack$type <- 'Regular'
tourneyStack <- fread('tourneyStack.csv', header = T, stringsAsFactors = F)
tourneyStack$type <- 'Tourney'

## Read in KenPom data

kenpom_All <- fread('KenPom_2002_2017.csv', header = T, stringsAsFactors = F)

## Read in Team Info

teams <- fread('teams.csv', header = T, stringsAsFactors = F)
colnames(teams) <- c('team', 'team_new')
team_lookup <- fread('team_lookup.csv', header = T, stringsAsFactors = F)
colnames(team_lookup) <- c('Team', 'team_new')

## Condense down kenpom data

kenpom_con <- kenpom_All %>%
  inner_join(team_lookup, by = 'Team') %>%
  inner_join(teams, by = 'team_new') %>%
  select(team_new, team, AdjEM, AdjO, AdjD, AdjT, AdjEM_SOS, Year)
colnames(kenpom_con) <- c('team_new', 'team', 'AdjEM', 'AdjO', 'AdjD', 'AdjT', 'AdjEM_SOS', 'Season')

## Calc avgs from Kaggle Data

full_data <- as.data.frame(rbind(regSeasonStack, tourneyStack)) %>%
  select(Season, Wteam, Wscore, Wfgm, Wfga, Wfgm3, Wftm, Wor, Wdr, Wto) %>%
  group_by(Season, Wteam) %>%
  summarise(Wor = mean(Wor),
            Wdr = mean(Wdr),
            efg = sum(Wfgm + (sum(Wfgm3) * 0.5)) / sum(Wfga),
            three_pt_perc = (sum(Wfgm3) * 3) / sum(Wscore),
            to_perc = sum(Wto) / (sum(Wfga) + (sum(Wftm) * 0.44)),
            ft_perc = sum(Wftm) / sum(Wscore)) %>%
  as.data.frame()

## Merge in kenpom & kaggle data into individual games

seasonGames <- gameStack %>%
  filter(Season >= 2003) %>%
  mutate(game_id = paste0(Season, "_", Daynum, "_", Wteam, "_", Lteam)) %>%
  select(Season, game_id, Wteam, Lteam, Wscore, Lscore, Wloc, type) %>%
  left_join(kenpom_con, by = c('Season' = 'Season', 'Wteam' = 'team')) %>%
  left_join(kenpom_con, by = c('Season' = 'Season', 'Lteam' = 'team')) %>%
  left_join(full_data, by = c('Season' = 'Season', 'Wteam' = 'Wteam')) %>%
  left_join(full_data, by = c('Season' = 'Season', 'Lteam' = 'Wteam'))

## Rename columns

colnames(seasonGames) <- c('Season', 'game_id', 'Wteam', 'Lteam', 
                           'Wscore', 'Lscore', 'Wloc', 'type', 'team_name_w', 
                           'AdjEM_W', 'AdjO_W', 'AdjD_W', 'AdjT_W',
                           'AdjEM_SOS_W', 'team_name_l', 'AdjEM_L',
                           'AdjO_L', 'AdjD_L', 'AdjT_L', 'AdjEM_SOS_L',
                           'Wor_W', 'Wdr_W', 'efg_W', 'three_pt_perc_W',
                           'to_perc_W', 'ft_perc_W', 'Wor_L', 'Wdr_L', 'efg_L',
                           'three_pt_perc_L', 'to_perc_L', 'ft_perc_L')

## Calculate differences for all winning teams

seasonGamesW <- seasonGames %>%
  mutate(AdjEM_D = AdjEM_W - AdjEM_L,
         AdjO_D = AdjO_W - AdjO_L,
         AdjD_D = AdjD_W - AdjD_L,
         AdjT_D = AdjT_W - AdjT_L,
         AdjEM_SOS_D = AdjEM_SOS_W - AdjEM_SOS_L,
         Wor_D = Wor_W - Wor_L,
         Wdr_D = Wdr_W - Wdr_L,
         efg_D = efg_W - efg_L,
         three_pt_D = three_pt_perc_W - three_pt_perc_L,
         to_perc_D = to_perc_W - to_perc_L,
         ft_perc_D = ft_perc_W - ft_perc_L,
         outcome = 1) %>%
  select(Season, game_id, Wteam, Lteam, team_name_w, team_name_l, Wscore, Lscore, Wloc, type,
         AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, Wor_D, Wdr_D, efg_D,
         three_pt_D, to_perc_D, ft_perc_D, outcome)
colnames(seasonGamesW) <- c('Season', 'game_id', 'team_1', 'team_2', 'team_name_1', 'team_name_2',
                            'score_1', 'score_2', 'loc', 'type', 'AdjEM_D', 'AdjO_D', 'AdjD_D',
                            'AdjT_D', 'AdjEM_SOS_D', 'Wor_D', 'Wdr_D', 'efg_D',
                            'three_pt_D', 'to_perc_D', 'ft_perc_D', 'outcome')

## Calculate differences for all losing teams

seasonGamesL <- seasonGames %>%
  mutate(AdjEM_D = AdjEM_L - AdjEM_W,
         AdjO_D = AdjO_L - AdjO_W,
         AdjD_D = AdjD_L - AdjD_W,
         AdjT_D = AdjT_L - AdjT_W,
         AdjEM_SOS_D = AdjEM_SOS_L - AdjEM_SOS_W,
         Wor_D = Wor_L - Wor_W,
         Wdr_D = Wdr_L - Wdr_W,
         efg_D = efg_L - efg_W,
         three_pt_D = three_pt_perc_L - three_pt_perc_W,
         to_perc_D = to_perc_L - to_perc_W,
         ft_perc_D = ft_perc_L - ft_perc_W,
         outcome = 0) %>%
  select(Season, game_id, Wteam, Lteam, team_name_w, team_name_l, Wscore, Lscore, Wloc, type,
         AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, Wor_D, Wdr_D, efg_D,
         three_pt_D, to_perc_D, ft_perc_D, outcome)
colnames(seasonGamesL) <- c('Season', 'game_id', 'team_1', 'team_2', 'team_name_1', 'team_name_2',
                            'score_1', 'score_2', 'loc', 'type', 'AdjEM_D', 'AdjO_D', 'AdjD_D',
                            'AdjT_D', 'AdjEM_SOS_D', 'Wor_D', 'Wdr_D', 'efg_D',
                            'three_pt_D', 'to_perc_D', 'ft_perc_D', 'outcome')


## Stack winners & losers on top of each other

fullSeasonData <- as.data.frame(rbind(seasonGamesW, seasonGamesL))
fullSeasonData <- fullSeasonData[complete.cases(fullSeasonData),]

## Binary Model Data - calculate z scores for each variable by year 

modelData <- fullSeasonData %>%
  mutate(loc = ifelse(loc == 'A', -1, ifelse(loc == 'N', 0, ifelse(loc == 'H', 1, 0)))) %>%
  group_by(Season) %>%
  mutate(AdjEM_Z = (AdjEM_D - mean(AdjEM_D)) / sd(AdjEM_D),
         AdjO_Z = (AdjO_D - mean(AdjO_D)) / sd(AdjO_D),
         AdjD_Z = (AdjD_D - mean(AdjD_D)) / sd(AdjD_D),
         AdjT_Z = (AdjT_D - mean(AdjT_D)) / sd(AdjT_D),
         AdjEM_SOS_Z = (AdjEM_SOS_D - mean(AdjEM_SOS_D)) / sd(AdjEM_SOS_D),
         Wor_Z = (Wor_D - mean(Wor_D) / sd(Wor_D)),
         Wdr_Z = (Wdr_D - mean(Wdr_D) / sd(Wdr_D)),
         efg_Z = (efg_D - mean(efg_D) / sd(efg_D)),
         three_pt_Z = (three_pt_D - mean(three_pt_D) / sd(three_pt_D)),
         to_perc_Z = (to_perc_D - mean(to_perc_D) / sd(to_perc_D)),
         ft_perc_Z = (ft_perc_D - mean(ft_perc_D) / sd(ft_perc_D))) %>%
  as.data.frame() 

## For predictions - persist averages by year

yrData <- fullSeasonData %>%
  group_by(Season) %>%
  summarise(AdjEM_mean = mean(AdjEM_D),
            AdjEM_sd = sd(AdjEM_D),
            AdjO_mean = mean(AdjO_D),
            AdjO_sd = sd(AdjO_D),
            AdjD_mean = mean(AdjD_D),
            AdjD_sd = sd(AdjD_D),
            AdjT_mean = mean(AdjT_D),
            AdjT_sd = sd(AdjT_D),
            AdjEM_SOS_mean = mean(AdjEM_SOS_D),
            AdjEM_SOS_sd = sd(AdjEM_SOS_D),
            Wor_mean = mean(Wor_D),
            Wor_sd = sd(Wor_D),
            Wdr_mean = mean(Wdr_D),
            Wdr_sd = sd(Wdr_D),
            efg_mean = mean(efg_D),
            efg_sd = sd(efg_D),
            three_pt_mean = mean(three_pt_D),
            three_pt_sd = sd(three_pt_D),
            to_perc_mean = mean(to_perc_D),
            to_perc_sd = sd(to_perc_D),
            ft_perc_mean = mean(ft_perc_D),
            ft_perc_sd = sd(ft_perc_D)) %>%
  as.data.frame() 

## Create final binary model files for regular & tournament games
## Include all info for evaluation

modelReg <- filter(modelData, type == 'Regular')
modelTournFull <- filter(modelData, type == 'Tourney') 

## Load in model packages 

library(caret)
library(xgboost)

## set seed to make reproducible then create test & training sets

set.seed(124)

trainIndex <- createDataPartition(y=modelReg$outcome,p=0.7,list=FALSE)
modelRegTrainFull <- modelReg[trainIndex,]
modelRegTestFull <- modelReg[-trainIndex,]

## Cut down to just modeled variables

modelRegTrain <- modelRegTrainFull %>%
  select(loc, AdjEM_Z, AdjO_Z, AdjD_Z, AdjT_Z, AdjEM_SOS_Z, 
       Wor_Z, Wdr_Z, efg_Z, three_pt_Z, to_perc_Z, ft_perc_Z, outcome) 

modelRegTest <- modelRegTestFull %>%
  select(loc, AdjEM_Z, AdjO_Z, AdjD_Z, AdjT_Z, AdjEM_SOS_Z, 
         Wor_Z, Wdr_Z, efg_Z, three_pt_Z, to_perc_Z, ft_perc_Z, outcome) 

modelTourn <- modelTournFull %>%
  select(loc, AdjEM_Z, AdjO_Z, AdjD_Z, AdjT_Z, AdjEM_SOS_Z, 
         Wor_Z, Wdr_Z, efg_Z, three_pt_Z, to_perc_Z, ft_perc_Z, outcome) 


## Create matrices for performance reasons

library(Matrix)

outcomeTrain <- modelRegTrain$outcome
outcomeTest <- modelRegTest$outcome
outcomeTourney <- modelTourn$outcome

sparseTrain <- sparse.model.matrix(outcome  ~.-1 , data=modelRegTrain)
sparseTest <- sparse.model.matrix(outcome ~.-1, data=modelRegTest)
sparseTTest <- sparse.model.matrix(outcome ~.-1, data=modelTourn)
dtrain <- xgb.DMatrix(data = sparseTrain, label = outcomeTrain)
dtest <- xgb.DMatrix(data = sparseTest, label = outcomeTest)
dTtest <- xgb.DMatrix(data = sparseTTest, label = outcomeTourney)

watchlist <- list(train=dtrain, test=dtest)

xgbD <- xgboost(data = dtrain,
               eta = 0.045,
               max_depth = 6, 
               nround=250, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               eval.metric = "error",
               eval.metric = 'auc',
               eval_metric = "logloss",
               objective = "binary:logistic",
               nthread = 3,
               watchlist = watchlist
)

model <- xgb.dump(xgbD, with.stats = T)
importance <- xgb.importance(feature_names = colnames(sparseTrain), model = xgbD)

importance

## Create predictions

predsTest <- predict(xgbD, dtest)
predsTourn <- predict(xgbD, dTtest)

## Evaluate predictions

LogLoss <- function(pred, outcome){
  ll <- (-1/length(pred)) * sum (outcome * log(pred + .00001) + (1-outcome)*log(1-pred + .00001))
  print(paste("test-log-loss=", ll))
}

errorfunc <- function(pred, outcome) {
  err <- as.numeric(sum(as.integer(pred > 0.5) != outcome)) / length(outcome)
  print(paste("test-error=", err))
}

errorfunc(predsTest, outcomeTest)
LogLoss(predsTest, outcomeTest)

scoreCheckReg <- as.data.frame(cbind(modelRegTestFull, predsTest)) %>%
  mutate(miss = outcome - predsTest)

library(DescTools)
options(scipen = 999)
Desc(scoreCheckReg$miss)

conf <- filter(scoreCheckReg, predsTest >= 0.80)
Desc(conf$outcome)
errorfunc(conf$predsTest, conf$outcome)

errorfunc(predsTourn, outcomeTourney)
LogLoss(predsTourn, outcomeTourney)

scoreCheck <- as.data.frame(cbind(modelTournFull, predsTourn)) %>%
  mutate(miss = outcome - predsTourn)

confTourn <- filter(scoreCheck, predsTourn >= 0.80) %>%
  arrange(predsTourn)

Desc(scoreCheck$predsTourn)
Desc(confTourn$outcome)
errorfunc(conf$predsTourn, conf$outcome)

confTourn

## Log Regression

logmodel <- glm(outcome ~.,family=binomial(link='logit'),data=modelRegTrain)
logmodel$coefficients

logPredsTest <- predict(logmodel, modelRegTest, type = 'response')
logPredsTourn <- predict(logmodel, modelTourn, type = 'response')

errorfunc(logPredsTest, outcomeTest)
LogLoss(logPredsTest, outcomeTest)

errorfunc(logPredsTourn, outcomeTourney)
LogLoss(logPredsTourn, outcomeTourney)

## Random Forest

library(randomForest)

rfM <- randomForest(as.factor(outcome) ~., data = modelRegTrain,
                    ntree = 200, keep.forest = T,
                    replace = T, nodesize = 1, importance = T)

rfM$importance
rfM$err.rate

predsRF <- predict(rfM, modelRegTest,type = 'prob')
errorfunc(predsRF[,2], outcomeTest)
LogLoss(predsRF[,2], outcomeTest)

predsRFTourn <- predict(rfM, modelTourn, type = 'prob')
errorfunc(predsRFTourn[,2], outcomeTourney)
LogLoss(predsRFTourn[,2], outcomeTourney)

## Ensemble

PredsAvg <- (predsTest + logPredsTest + predsRF[,2]) / 3
PredsTournAvg <- (predsTourn + logPredsTourn + predsRFTourn[,2]) / 3

errorfunc(PredsAvg, outcomeTest)
LogLoss(PredsAvg, outcomeTest)

errorfunc(PredsTournAvg, outcomeTourney)
LogLoss(PredsTournAvg, outcomeTourney)

## New Data Predictions

## Read in file & convert to modeling format

library(splitstackshape)

sampleSub <- fread('sample_submission.csv', stringsAsFactors = F)
sampleSub <- cSplit(sampleSub, 'id', sep = "_")
sampleSub$pred <- NULL
colnames(sampleSub) <- c('Season', 'team_1', 'team_2')

newPredsFormat <- function(predsDF) {
  kp <- as.data.frame(kenpom_con) %>%
    select(-team_new)
  df <- as.data.frame(predsDF) %>%
    mutate(id = paste0(Season,"_",team_1,"_",team_2),
           loc = 0) %>%
    left_join(kp, by = c('Season' = 'Season', 'team_1' = 'team')) %>%
    left_join(kp, by = c('Season' = 'Season', 'team_2' = 'team')) %>%
    left_join(full_data, by = c('Season' = 'Season', 'team_1' = 'Wteam')) %>%
    left_join(full_data, by = c('Season' = 'Season', 'team_2' = 'Wteam'))
  colnames(df) <- c('Season', 'team_1', 'team_2', 'id', 'loc', 
                             'AdjEM_W', 'AdjO_W', 'AdjD_W', 'AdjT_W',
                             'AdjEM_SOS_W', 'AdjEM_L',
                             'AdjO_L', 'AdjD_L', 'AdjT_L', 'AdjEM_SOS_L',
                             'Wor_W', 'Wdr_W', 'efg_W', 'three_pt_perc_W',
                             'to_perc_W', 'ft_perc_W', 'Wor_L', 'Wdr_L', 'efg_L',
                             'three_pt_perc_L', 'to_perc_L', 'ft_perc_L')
  df <- df %>%
   mutate(AdjEM_D = AdjEM_W - AdjEM_L,
          AdjO_D = AdjO_W - AdjO_L,
          AdjD_D = AdjD_W - AdjD_L,
          AdjT_D = AdjT_W - AdjT_L,
          AdjEM_SOS_D = AdjEM_SOS_W - AdjEM_SOS_L,
          Wor_D = Wor_W - Wor_L,
          Wdr_D = Wdr_W - Wdr_L,
          efg_D = efg_W - efg_L,
          three_pt_D = three_pt_perc_W - three_pt_perc_L,
          to_perc_D = to_perc_W - to_perc_L,
          ft_perc_D = ft_perc_W - ft_perc_L) %>%
   left_join(yrData, by = c('Season' = 'Season')) %>%
   mutate(AdjEM_Z = (AdjEM_D - AdjEM_mean) / AdjEM_sd,
           AdjO_Z = (AdjO_D - AdjO_mean) / AdjO_sd,
           AdjD_Z = (AdjD_D - AdjD_mean) / AdjD_sd,
           AdjT_Z = (AdjT_D - AdjT_mean) / AdjT_sd,
           AdjEM_SOS_Z = (AdjEM_SOS_D - AdjEM_SOS_mean) / AdjEM_SOS_sd,
           Wor_Z = (Wor_D - Wor_mean) / Wor_sd,
           Wdr_Z = (Wdr_D - Wdr_mean) / Wdr_sd,
           efg_Z = (efg_D - efg_mean) / efg_sd,
           three_pt_Z = (three_pt_D - three_pt_mean) / three_pt_sd,
           to_perc_Z = (to_perc_D - to_perc_mean) / to_perc_sd,
           ft_perc_Z = (ft_perc_D - ft_perc_mean) / ft_perc_sd,
          dummy_outcome = 1) %>%
    select(id, Season, team_1, team_2, loc, AdjEM_Z, AdjO_Z, AdjD_Z, AdjT_Z, AdjEM_SOS_Z, 
           Wor_Z, Wdr_Z, efg_Z, three_pt_Z, to_perc_Z, ft_perc_Z, dummy_outcome)
  df_model <- df %>%
    select(-id, -Season, -team_1, -team_2)
  df_keep <- df %>%
    select(id, Season, team_1, team_2)
  head(df_model)
  sparseDF <- sparse.model.matrix(dummy_outcome~.-1, data=df_model)
  sparseDF <- xgb.DMatrix(data = sparseDF, label = df_model$dummy_outcome)
  df_preds <- predict(xgbD, sparseDF)
  df_preds_logs <- predict(logmodel, df_model, type = 'response')
  df_final_ens <- as.data.frame(cbind(df_preds, df_preds_logs)) %>%
    mutate(pred_ens = (df_preds + df_preds_logs) / 2)
  df_final <- as.data.frame(cbind(df_keep, cbind(df_final_ens$pred_ens, cbind(df_preds, df_preds_logs))))
  return(df_final)
}

sampleSubmission <- newPredsFormat(sampleSub)
head(sampleSubmission)
ensSub <- sampleSubmission %>%
  select(id, V1)
colnames(ensSub) <- c('id', 'pred')
write.csv(ensSub, 'ensemble_v1.csv', row.names = F)

logSub <- sampleSubmission %>%
  select(id, df_preds_logs)
colnames(logSub) <- c('id', 'pred')
write.csv(logSub, 'log_v1.csv', row.names = F)

xgbSub <- sampleSubmission %>%
  select(id, df_preds)
colnames(xgbSub) <- c('id', 'pred')
write.csv(xgbSub, 'xgb_v1.csv', row.names = F)

filter(df_final, team_1 == 1246)
filter(df_final_2, team_1 == 1246)
filter(df_final, team_2 == 1246)
filter(df_final_2, team_2 == 1246)
sum(df_final$df_preds)
library(DescTools)
options(scipen = 999)
