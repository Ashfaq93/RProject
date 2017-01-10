setwd("C:\\Users\\Ashfaq\\Dropbox\\DATA\\kaggle\\santander customer satisfaction ccompetition-MARCH 2016")

Trn <- read.csv("train.csv")## trained data
TestOriginal <- read.csv("test.csv")
samplesubmission <- read.csv("sample_submission.csv")

#sample data  #data split
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/1.42))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}



splits <- splitdf(Trn, seed = 777)
Trainset <- splits$trainset
Testset <- splits$testset

summary(Trainset)

## logistic regression model.
cntrl <- trainControl("cv",10,savePredictions = T)

logisModel <- train(TARGET~., data = Trainset,method = "glm",family = binomial,trControl = cntrl)

## xgboost
library(readr)
library(dplyr)
library(tidyr)
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_1 = train(
  x = as.matrix(Trainset %>%
                  select(-TARGET)),
  y = as.factor(Trainset$TARGET),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)
##feature nnames
# Features
feature.names <- names(Trn)
feature.names <- feature.names[-grep('^ID$', feature.names)]
feature.names <- feature.names[-grep('^TARGET$', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

###xGB library model
# xgboost fitting with arbitrary parameters
xgb_params_1 = list(booster = "gbtree",
  objective = "binary:logistic",                                               # binary classification
  eta = 0.05,                                                                  # learning rate
  max.depth = 7,                                                               # max tree depth
  eval_metric = "auc"                                                          # evaluation/loss metric
)

xgb_1 = xgboost(data = as.matrix(Trainset %>%
                                   select(-TARGET)),
                label = Trainset$TARGET,
                params = xgb_params_1,
                nrounds = 100,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 10                                          # stop if no improvement within 10 trees
)
# cross-validate xgboost to get the accurate measure of error
xgb_cv_2 = xgb.cv(params = xgb_params_1,
                  data = as.matrix(Trn %>%
                                     select(-TARGET)),
                  label = Trn$TARGET,
                  nrounds = 100, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10
)
###plotting AUC with XGBOOST
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()
###predict
indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.85))

data <- sparse.model.matrix(feature.formula, data = Trainset[indexes, ])
sparseMatrixColNamesTrain <- colnames(data)
dtrain <- xgb.DMatrix(data, label = train[indexes, 'TARGET'])
rm(data)
dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = train[-indexes, ]),
                      label = train[-indexes, 'TARGET'])
dtest <- sparse.model.matrix(feature.formula, data = TestOriginal)


##nnet
