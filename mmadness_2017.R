setwd('~/Documents/Kaggle/MarchMadness/2017/Modeling_Data/March_Madness_2017')

library(data.table)

## Read in core information about tourney & regular season games
head(regSeasonComp)
head(regSeasonComp2017)
regSeasonComp <- fread('regularSeasonCompactResults.csv', header = T, stringsAsFactors = F)
regSeasonComp2017 <- fread('2017_Final_CompactResults.csv', header = T, stringsAsFactors = F)
regSeasonComp <- as.data.frame(rbind(regSeasonComp, regSeasonComp2017))
regSeasonComp$type <- 'Regular'
tournComp <- fread('TourneyCompactResults.csv', header = T, stringsAsFactors = F)
tournComp$type <- 'Tourney'
gameStack <- as.data.frame(rbind(regSeasonComp, tournComp))

## Automate stacking

regSeasonS <- fread('RegularSeasonDetailedResults.csv', header = T, stringsAsFactors = F)
regSeasonDet2017 <- fread('2017_Final_DetailedResults.csv', header = T, stringsAsFactors = F)
regSeasonS <- as.data.frame(rbind(regSeasonS, regSeasonDet2017))
tourneyS <- fread('TourneyDetailedResults.csv', header = T, stringsAsFactors = F)

## Create winner and loser datasets

regSeasonW <- regSeasonS %>%
  mutate(Outcome = 'Win',
         type = 'Regular') %>%
  select(Season, Daynum, Outcome, Wteam, Wscore, Wloc, Numot, Wfgm, Wfga, Wfgm3, Wfga3,
         Wftm, Wfta, Wor, Wdr, Wast, Wto, Wstl, Wblk, Wpf, type)

regSeasonL <- regSeasonS %>%
  mutate(Outcome = 'Loss',
         type = 'Regular',
         Wloc = ifelse(Wloc == 'N', 'N', ifelse(Wloc == 'A', 'H', 'A'))) %>%
  select(Season, Daynum, Outcome, Lteam, Lscore, Wloc, Numot, Lfgm, Lfga, Lfgm3, Lfga3,
         Lftm, Lfta, Lor, Ldr, Last, Lto, Lstl, Lblk, Lpf, type)

tourneyW <- tourneyS %>%
  mutate(Outcome = 'Win',
         type = 'Tourney') %>%
  select(Season, Daynum, Outcome, Wteam, Wscore, Wloc, Numot, Wfgm, Wfga, Wfgm3, Wfga3,
         Wftm, Wfta, Wor, Wdr, Wast, Wto, Wstl, Wblk, Wpf, type)  

tourneyL <- tourneyS %>%
  mutate(Outcome = 'Loss',
         type = 'Tourney') %>%
  select(Season, Daynum, Outcome, Wteam, Wscore, Wloc, Numot, Wfgm, Wfga, Wfgm3, Wfga3,
         Wftm, Wfta, Wor, Wdr, Wast, Wto, Wstl, Wblk, Wpf, type)  

colnames(regSeasonL) <- c('Season', 'Daynum', 'Outcome', 'Wteam', 'Wscore', 'Wloc', 
                          'Numot', 'Wfgm', 'Wfga', 'Wfgm3', 'Wfga3',
                          'Wftm', 'Wfta', 'Wor', 'Wdr', 'Wast', 'Wto', 
                          'Wstl', 'Wblk', 'Wpf', 'type')

colnames(tourneyL) <- c('Season', 'Daynum', 'Outcome', 'Wteam', 'Wscore', 'Wloc', 
                          'Numot', 'Wfgm', 'Wfga', 'Wfgm3', 'Wfga3',
                          'Wftm', 'Wfta', 'Wor', 'Wdr', 'Wast', 'Wto', 
                          'Wstl', 'Wblk', 'Wpf', 'type')

regSeasonStack <- as.data.frame(rbind(regSeasonW, regSeasonL))
tourneyStack <- as.data.frame(rbind(tourneyW, tourneyL))

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
  select(Season, game_id, Wteam, Lteam, team_name_w, team_name_l, Wscore, Lscore, 
         Wloc, type,
         AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, Wor_D, Wdr_D, efg_D,
         three_pt_D, to_perc_D, ft_perc_D, outcome)
colnames(seasonGamesL) <- c('Season', 'game_id', 'team_1', 'team_2', 'team_name_1', 'team_name_2',
                            'score_1', 'score_2', 'loc', 'type', 
                            'AdjEM_D', 'AdjO_D', 'AdjD_D',
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
               nround=500, 
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

predsTrain <- predict(xgbD, dtrain)
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

logPredsTrain <- predict(logmodel, modelRegTrain, type = 'response')
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

predsRFTrain <- predict(rfM, modelRegTrain, type = 'prob')

predsRF <- predict(rfM, modelRegTest,type = 'prob')
errorfunc(predsRF[,2], outcomeTest)
LogLoss(predsRF[,2], outcomeTest)

predsRFTourn <- predict(rfM, modelTourn, type = 'prob')
errorfunc(predsRFTourn[,2], outcomeTourney)
LogLoss(predsRFTourn[,2], outcomeTourney)

## Adaboost

library(fastAdaboost)
adaM <- adaboost(outcome ~., data=modelRegTrain ,10)

adaPredTrain <- predict(adaM, modelRegTrain)
adaPredTrain <- adaPredTrain$prob
adaPredTest <- predict(adaM, modelRegTest)
adaPredTest <- adaPredTest$prob

adaPredTourn <- predict(adaM, modelTourn)
adaPredTourn <- adaPredTourn$prob

errorfunc(adaPredTest[,2], outcomeTest)
LogLoss(adaPredTest[,2], outcomeTest)

errorfunc(adaPredTourn[,2], outcomeTourney)
LogLoss(adaPredTourn[,2], outcomeTourney)

## XGBoost w/o standardized vals

## Cut down to just modeled variables

modelRegTrainD <- modelRegTrainFull %>%
  select(loc, AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, 
         Wor_D, Wdr_D, efg_D, three_pt_D, to_perc_D, ft_perc_D, outcome) 

modelRegTestD <- modelRegTestFull %>%
  select(loc, AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, 
         Wor_D, Wdr_D, efg_D, three_pt_D, to_perc_D, ft_perc_D, outcome) 

modelTournD <- modelTournFull %>%
  select(loc, AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, 
         Wor_D, Wdr_D, efg_D, three_pt_D, to_perc_D, ft_perc_D, outcome) 

sparseTrainD <- sparse.model.matrix(outcome  ~.-1 , data=modelRegTrainD)
sparseTestD <- sparse.model.matrix(outcome ~.-1, data=modelRegTestD)
sparseTTestD <- sparse.model.matrix(outcome ~.-1, data=modelTournD)
dtrainD <- xgb.DMatrix(data = sparseTrainD, label = outcomeTrain)
dtestD <- xgb.DMatrix(data = sparseTestD, label = outcomeTest)
dTtestD <- xgb.DMatrix(data = sparseTTestD, label = outcomeTourney)

xgbDD <- xgboost(data = dtrainD,
                eta = 0.045,
                max_depth = 6, 
                nround=500, 
                subsample = 0.5,
                colsample_bytree = 0.5,
                eval.metric = "error",
                eval.metric = 'auc',
                eval_metric = "logloss",
                objective = "binary:logistic",
                nthread = 3,
                watchlist = watchlist
)

importanceD <- xgb.importance(feature_names = colnames(sparseTrainD), model = xgbDD)

importanceD

## Create predictions

predsTrainD <- predict(xgbDD, dtrainD)
predsTestD <- predict(xgbDD, dtestD)
predsTournD <- predict(xgbDD, dTtestD)

errorfunc(predsTestD, outcomeTest)
LogLoss(predsTestD, outcomeTest)
errorfunc(predsTournD, outcomeTourney)
LogLoss(predsTournD, outcomeTourney)


## Ensemble

## Simple Avgs

PredsAvg <- (predsTest + logPredsTest + predsRF[,2] + adaPredTest[,2] + predsTestD) / 5
PredsTournAvg <- (predsTourn + logPredsTourn + predsRFTourn[,2] + adaPredTourn[,2] + predsTournD) / 5

errorfunc(PredsAvg, outcomeTest)
LogLoss(PredsAvg, outcomeTest)

errorfunc(PredsTournAvg, outcomeTourney)
LogLoss(PredsTournAvg, outcomeTourney)

## Ensemble XGB

## Predictions as Inputs

PredsTAvg <- as.data.frame(cbind(predsTrain,cbind(predsTrainD,cbind(logPredsTrain,cbind(predsRFTrain[,2],cbind(adaPredTrain[,2], outcomeTrain))))))
PredsTeAvg <- as.data.frame(cbind(predsTest,cbind(predsTestD,cbind(logPredsTest,cbind(predsRF[,2],cbind(adaPredTest[,2], outcomeTest))))))
PredsToAvg <- as.data.frame(cbind(predsTourn,cbind(predsTournD,cbind(logPredsTourn,cbind(predsRFTourn[,2],cbind(adaPredTourn[,2], outcomeTourney))))))

## Just Log & XGB

PredsTAvgT <- PredsTAvg[,c(1:3,6)]
PredsTeAvgT <- PredsTeAvg[,c(1:3,6)]
PredsToAvgT <- PredsToAvg[,c(1:3,6)]

## Just XGB

PredsTAvgX <- PredsTAvg[,c(1:2,6)]
PredsTeAvgX <- PredsTeAvg[,c(1:2,6)]
PredsToAvgX <- PredsToAvg[,c(1:2,6)]

## Create Matrices

sparseTrainEns <- sparse.model.matrix(outcomeTrain  ~.-1 , data=PredsTAvgT)
sparseTestEns <- sparse.model.matrix(outcomeTest  ~.-1 , data=PredsTeAvgT)
sparseTournEns <- sparse.model.matrix(outcomeTourney  ~.-1 , data=PredsToAvgT)
sparseTrainEnsX <- sparse.model.matrix(outcomeTrain  ~.-1 , data=PredsTAvgX)
sparseTestEnsX <- sparse.model.matrix(outcomeTest  ~.-1 , data=PredsTeAvgX)
sparseTournEnsX <- sparse.model.matrix(outcomeTourney  ~.-1 , data=PredsToAvgX)
dtrainEns <- xgb.DMatrix(data = sparseTrainEns, label = outcomeTrain)
dtestEns <- xgb.DMatrix(data = sparseTestEns, label = outcomeTest)
dtournEns <- xgb.DMatrix(data = sparseTournEns, label = outcomeTourney)
dtrainEnsX <- xgb.DMatrix(data = sparseTrainEnsX, label = outcomeTrain)
dtestEnsX <- xgb.DMatrix(data = sparseTestEnsX, label = outcomeTest)
dtournEnsX <- xgb.DMatrix(data = sparseTournEnsX, label = outcomeTourney)

## Log + XGB model

xgbDEns <- xgboost(data = dtrainEns,
                 eta = 0.01,
                 max_depth = 3, 
                 nround=500, 
                 subsample = 0.5,
                 colsample_bytree = 0.5,
                 eval.metric = "error",
                 eval.metric = 'auc',
                 eval_metric = "logloss",
                 objective = "binary:logistic",
                 nthread = 3,
                 watchlist = watchlist
)

importanceE <- xgb.importance(feature_names = colnames(sparseTrainEns), model = xgbDEns)
importanceE

## Predictions

ensTest <- predict(xgbDEns, dtestEns)
ensTourn <- predict(xgbDEns, dtournEns)

errorfunc(ensTest, outcomeTest)
LogLoss(ensTest, outcomeTest)

errorfunc(ensTourn, outcomeTourney)
LogLoss(ensTourn, outcomeTourney)

## XGB only model

xgbDEnsX <- xgboost(data = dtrainEnsX,
                   eta = 0.01,
                   max_depth = 3, 
                   nround=500, 
                   subsample = 0.5,
                   colsample_bytree = 0.5,
                   eval.metric = "error",
                   eval.metric = 'auc',
                   eval_metric = "logloss",
                   objective = "binary:logistic",
                   nthread = 3,
                   watchlist = watchlist
)

importanceEX <- xgb.importance(feature_names = colnames(sparseTrainEnsX), model = xgbDEnsX)
importanceEX

## Predictions

ensTestX <- predict(xgbDEnsX, dtestEnsX)
ensTournX <- predict(xgbDEnsX, dtournEnsX)

errorfunc(ensTestX, outcomeTest)
LogLoss(ensTestX, outcomeTest)

errorfunc(ensTournX, outcomeTourney)
LogLoss(ensTournX, outcomeTourney)

## New Data Predictions

## Read in file & convert to modeling format

library(splitstackshape)

## For First Stage

##sampleSub <- fread('sample_submission.csv', stringsAsFactors = F)

## Second Stage

sampleSub <- fread('SampleSubmission_5050Benchmark.csv', stringsAsFactors = F)
sampleSub <- cSplit(sampleSub, 'Id', sep = "_")
sampleSub$Pred <- NULL
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
          dummy_outcome = 1)
  df_model <- df %>% select(loc, AdjEM_Z, AdjO_Z, AdjD_Z, AdjT_Z, AdjEM_SOS_Z, 
           Wor_Z, Wdr_Z, efg_Z, three_pt_Z, to_perc_Z, ft_perc_Z, dummy_outcome)
  df_model_d <- df %>% select(loc, AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, 
           Wor_D, Wdr_D, efg_D, three_pt_D, to_perc_D, ft_perc_D, dummy_outcome)
  df_keep <- df %>%
    select(id, Season, team_1, team_2)
  ## Create Model Files for XGB
  
  sparseDF <- sparse.model.matrix(dummy_outcome~.-1, data=df_model)
  sparseDF <- xgb.DMatrix(data = sparseDF, label = df_model$dummy_outcome)
  sparseDFD <- sparse.model.matrix(dummy_outcome~.-1, data=df_model_d)
  sparseDFD <- xgb.DMatrix(data = sparseDFD, label = df_model_d$dummy_outcome)
  
  ## Generate preds
  
  xgb_preds <- predict(xgbD, sparseDF)
  xgbD_preds <- predict(xgbDD, sparseDFD)
  log_preds <- predict(logmodel, df_model, type = 'response')
  
  ## Simple Ens 
  
  simpEns <- (xgb_preds + xgbD_preds + log_preds) / 3
  
  ## XGB + Log Ens
  
  dummy_outcome <- rep(1, nrow(df_model))
  ens <- as.data.frame(cbind(xgb_preds, cbind(xgbD_preds, cbind(log_preds, dummy_outcome))))
  sparseEns <- sparse.model.matrix(dummy_outcome~.-1, data=ens)
  sparseEns <- xgb.DMatrix(data = sparseEns, label = ens$dummy_outcome)
  
  xgbLEns <- predict(xgbDEns, sparseEns)
  
  ## XGB Ens
  
  ensX <- as.data.frame(cbind(xgb_preds, cbind(xgbD_preds, dummy_outcome)))
  sparseEnsX <- sparse.model.matrix(dummy_outcome~ .-1, data = ensX)
  sparseEnsX <- xgb.DMatrix(data = sparseEnsX, label = ensX$dummy_outcome)
  
  xgbEns <- predict(xgbDEnsX, sparseEnsX)

  
  df_final <- as.data.frame(cbind(df_keep$id, cbind(xgb_preds, cbind(xgbD_preds, cbind(log_preds, cbind(simpEns, cbind(xgbLEns, xgbEns)))))))
  return(df_final)
}

sampleSubmission <- newPredsFormat(sampleSub)

## CREATE FUNCTION  To save to CSV File

csv_saver <- function(name, var) {
  df <- sampleSubmission[,c(1,var)]
  colnames(df) <- c('id', 'pred')
  write.csv(df, paste0(name,'.csv'), row.names = F)
}

## Save different variations of predictions

csv_saver('xgb_z_2017', 2)
csv_saver('xgb_d_2017', 3)
csv_saver('log_2017', 4)
csv_saver('simpleEns_2017', 5)
csv_saver('xgbLEns_2017', 6)
csv_saver('xgbEns_2017', 7)

## Continuous Predictor

modelRegTrainCont <- modelRegTrainFull %>%
  mutate(score_diff = ifelse(outcome == 1, score_1 - score_2, score_2 - score_1))

modelRegTestCont <- modelRegTestFull %>%
  mutate(score_diff = ifelse(outcome == 1, score_1 - score_2, score_2 - score_1))

modelTournCont <- modelTournFull %>%
  mutate(score_diff = ifelse(outcome == 1, score_1 - score_2, score_2 - score_1))

## Scores

scoreTrain <- modelRegTrainCont$score_diff
scoreTest <- modelRegTestCont$score_diff
scoreTourney <- modelTournCont$score_diff

## Model Files

modelRegTrainCont_D <- modelRegTrainCont %>%
  select(loc, AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, 
         Wor_D, Wdr_D, efg_D, three_pt_D, to_perc_D, ft_perc_D)

modelRegTestCont_D <- modelRegTestCont %>%
  select(loc, AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, 
         Wor_D, Wdr_D, efg_D, three_pt_D, to_perc_D, ft_perc_D)

modelTournCont_D <- modelTournCont %>%
  select(loc, AdjEM_D, AdjO_D, AdjD_D, AdjT_D, AdjEM_SOS_D, 
         Wor_D, Wdr_D, efg_D, three_pt_D, to_perc_D, ft_perc_D)

## Matrixing

sparseTrainC <- sparse.model.matrix(scoreTrain  ~.-1 , data=modelRegTrainCont_D)
sparseTestC <- sparse.model.matrix(scoreTest  ~.-1 , data=modelRegTestCont_D)
sparseTournC <- sparse.model.matrix(scoreTourney  ~.-1 , data=modelTournCont_D)
dtrainC <- xgb.DMatrix(data = sparseTrainC, label = scoreTrain)
dtestC <- xgb.DMatrix(data = sparseTestC, label = scoreTest)
dtournC <- xgb.DMatrix(data = sparseTournC, label = scoreTourney)

## Model

xgbDC <- xgboost(data = dtrainC,
                    eta = 0.01,
                    max_depth = 3, 
                    nround=500, 
                    subsample = 0.5,
                    colsample_bytree = 0.5,
                    eval_metric = "rmse",
                    objective = "reg:linear",
                    nthread = 3,
                    watchlist = watchlist
)
  
importanceC <- xgb.importance(feature_names = colnames(sparseTrainC), model = xgbDC)
importanceC

cPredTest <- predict(xgbDC, dtestC)
cPredTourn <- predict(xgbDC, dtournC)

cPredFin <- as.data.frame(cbind(cPredTest, modelRegTestCont)) %>%
  mutate(miss = ceiling(scoreTest - cPredTest),
         score_diff = ifelse(outcome == 1, score_1 - score_2, score_2 - score_1),
         wrong = ifelse(sign(scoreTest) == sign(cPredTest), 0, 1)) %>%
  select(Season, score_1, score_2, team_name_1, team_name_2, AdjEM_D, cPredTest, score_diff, miss, wrong) %>%
  arrange(miss)

head(cPredFin)
tail(cPredFin)
Desc(cPredFin$wrong)

cPredTournFin <- as.data.frame(cbind(cPredTourn, modelTournCont)) %>%
  mutate(miss = ceiling(scoreTourney - cPredTourn),
         score_diff = ifelse(outcome == 1, score_1 - score_2, score_2 - score_1),
         wrong = ifelse(sign(scoreTourney) == sign(cPredTourn), 0, 1)) %>%
  select(Season, score_1, score_2, team_name_1, team_name_2, AdjEM_D, cPredTourn, score_diff, miss, wrong) %>%
  arrange(miss)

rmse <- function(actual, predict) {
  rmse <- sqrt(mean((predict -actual)^2))
  print(paste0('RMSE is: ',rmse))
}

rmse(cPredTournFin$cPredTourn, cPredTournFin$score_diff)
cor(cPredTournFin$cPredTourn, cPredTournFin$score_diff)
sd(cPredTournFin$miss)

tight_games <- filter(cPredTournFin, cPredTourn < 10 & cPredTourn > -10)
sd(tight_games$miss)
cor(tight_games$cPredTourn, tight_games$score_diff)
rmse(tight_games$cPredTourn, tight_games$score_diff)
Desc(tight_games$wrong)
Desc(tight_games$miss)



library(ggplot2)
deviation_plot <- function(df) {
  ggplot(data=df, aes(x = miss)) +
    geom_histogram(binwidth=2, colour="black", 
                   aes(y=..density.., fill=..count..)) +
    geom_vline(xintercept = mean(df[,'miss']) + (sd(df[,'miss']))) +
    geom_vline(xintercept = mean(df[,'miss']) - (sd(df[,'miss']))) +
    geom_vline(xintercept = mean(df[,'miss']) + (sd(df[,'miss']) * 2)) +
    geom_vline(xintercept = mean(df[,'miss']) - (sd(df[,'miss']) * 2))
}

deviation_plot(cPredTournFin)
deviation_plot(cPredFin)

       
