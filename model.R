
setwd("/Users/sbiradar/Downloads")

library(data.table)
library(caret)
# load data - specify numeric or factor all 58 features
df1=fread(file="feature.csv",sep=",",colClasses=(rep("numeric",57)),header=TRUE,drop=c("quantity","offerdate","repeattrips"))


df1$repeater=as.factor(df1$repeater)

str(df)
dim(df)
str(df)

#library(ISLR)
library(lattice)
library(ggplot2)
library(caret)
library(MASS)    #use for LDA/QDA
library(pROC)    #to generate ROC curves and capture AUC
set.seed(2016)   #set seed to replicate results
df
str(df)
#make names for targets if not already made '1' as 'positive' class
levels(df1$repeater)<- make.names(levels(factor(df1$repeater)))
df1$repeater<-relevel(df1$repeater,"t")

# start doing data partition
inTrain <- df
table(df$repeater)
inTrain <- createDataPartition(y = df$repeater, #outcome variable
                               p = .75,              #% of training data
                               list = FALSE)
# raw train
raw <- df
install.packages("dplyr")
# test data set
#test <- fread("featuretest.csv",sep=",",colClasses=(rep("numeric",56)),header=TRUE)
library(dplyr)
# remove features where the values they take on is limited
# here we make sure to keep the target variable and only those input
# features with enough variation



# test data set

test1 <- fread("featuretest1.csv",sep=",",colClasses=(rep("numeric",55)),header=TRUE,drop=c("offerdate","quantity"))
test1[,id:=as.numeric(id)]
dim(test)
dim(df)



# specify the cross-validation approach to use

######679
testlda=data.frame(test$id,test$repeatProbability2)
write.csv(file="testlda.csv",testlda)
qda <- train(repeater~.,data=raw,method="qda",family="binomial",trControl=ctrl,metric="ROC")
qdaprobs <- predict(qda,newdata=test,type="prob")[,1]
qdaClasses <- predict(qda,newdata=test)
test$repeatProbability3=qdaprobs
#####602
qdatest=data.frame(test$id,test$repeatProbability3)
write.csv(file="qdatestcv10.csv",qdatest)
ctrl <- trainControl(method = "cv", number=10   , classProbs = TRUE , summaryFunction = twoClassSummary)
#####0.56764 /602 for qda cv10

knn <- train(repeater~ .,data=raw,method="knn"
             ,family="binomial"
             ,trControl=ctrl
             ,metric="ROC"
             ,tuneLength=20)
summary(knn)
###################using H2o for model building
library(h2o)
h2o.init(nthreads=-1,max_mem_size = "20G")
df=as.h2o(df1)
test=as.h2o(test1)
##########using glm in h2o
y <- "repeater"
x <- setdiff( names(df), y )
train_binomial <- df[ df$repeater %in% c("t", "f"), ]
h2o.setLevels( df$repeater, c("t","f") )
data_binomial <- h2o.splitFrame( train_binomial, ratios = c(.6, 0.15) )
names(data_binomial) <- c('train1', 'valid', 'test1')
data_binomial$train1
grid_id <- 'glm_grid1'
hyper_parameters <- list( alpha = c(.2,.3,.4,.6,.8,.9, .5, 1) )
df=as.h2o(df1)
model_glm_grid1 <- h2o.grid(
  algorithm = "glm", 
  grid_id = grid_id,
  hyper_params = hyper_parameters,
  training_frame = df, 
  validation_frame = data_binomial$valid, 
  x = x, 
  y = y,
  lambda_search = TRUE,
  family = "binomial",
  stopping_metric="AUC",
  stopping_tolerance=0.001,nfolds = 5,fold_assignment = "Modulo",keep_cross_validation_predictions = TRUE)      

h2o.shutdown()
h2o.init(nthreads=-1,max_mem_size = "20G")
stopping_metric <- 'AUC'
sorted_models <- h2o.getGrid(
  grid_id = grid_id, 
  sort_by = stopping_metric,
  decreasing = TRUE
)
sorted_models
best_model <- h2o.getModel(sorted_models@model_ids[[1]])
h2o.confusionMatrix(best_model, valid = TRUE, metrics = 'AUC')
best_model@model$model_summary$regularization
auc <- h2o.auc(best_model, valid = TRUE)
auc
predict.reg <- as.data.frame(h2o.predict(best_model, test))
write.csv(predict.reg,file="predictco.csv")
#########################gbm model in h2o
splits <- h2o.splitFrame(
  data = df, 
  ratios = c(0.6,0.2),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
)

response <- "repeater"

## the response variable is an integer, we will turn it into a categorical/factor for binary classification
df[[response]] <- as.factor(df[[response]])           

## use all other columns (except for the name) as predictors
predictors <- setdiff(names(df), c(response, "repeater")) 




train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

gbm2 <- h2o.gbm(x = predictors, y = response, training_frame = df,nfolds = 5,fold_assignment = "Modulo",keep_cross_validation_predictions = TRUE)    
gbm2@model_id
h2o.auc(h2o.performance(gbm2, newdata = test)) 
preds1=as.data.frame(h2o.predict(gbm1,test2))
write.csv(preds1,file="predsgbm.csv")
gbm <- h2o.gbm(x = predictors, y = response, training_frame = h2o.rbind(train, valid), nfolds = 4, seed = 0xDECAF)
gbm@model$cross_validation_metrics_summary
h2o.auc(h2o.performance(gbm, xval = TRUE))
gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.01,                                                         
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC", 
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8,                                                   
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                 
)
h2o.auc(h2o.performance(gbm, test ))

##############################
hyper_params = list( max_depth = seq(2,30,2) )
hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid1 <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

grid1                                                                      
sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)    
sortedGrid=as.h2o(sortedGrid)
h2o.exportFile(sortedGrid, path="kl.csv", force = FALSE, parts = 1)
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "AUC",
  stopping_tolerance = 1e-3
)
grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid21", 
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = df, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
 
  
  nfolds = 5,fold_assignment = "Modulo",seed=1,keep_cross_validation_predictions = TRUE) 



sortedGrid <- h2o.getGrid("final_grid21", sort_by = "auc", decreasing = TRUE)    
sortedGrid
write.csv(sortedGrid,file="kl.csv")

for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.auc(h2o.performance(gbm, valid = TRUE)))
}
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
print(h2o.auc(h2o.performance(gbm, newdata = test)))



model <- do.call(h2o.gbm,
                 ## update parameters in place
                 {
                   p <- gbm@parameters
                   p$model_id = NULL          ## do not overwrite the original grid model
                   p$training_frame = df      ## use the full dataset
                   p$validation_frame = NULL  ## no validation frame
                   p$nfolds = 5               ## cross-validation
                   p
                 }
)
test2=as.h2o(test1)
model@model$cross_validation_metrics_summary
model@model$cross_validation_metrics_summary
pred=as.data.frame(h2o.predict(model,test2))
write.csv(pred,file="predsgmb2.csv")

for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  cvgbm <- do.call(h2o.gbm,
                   ## update parameters in place
                   {
                     p <- gbm@parameters
                     p$model_id = NULL          ## do not overwrite the original grid model
                     p$training_frame = df      ## use the full dataset
                     p$validation_frame = NULL  ## no validation frame
                     p$nfolds = 5               ## cross-validation
                     p
                   }
  )
  print(gbm@model_id)
  print(cvgbm@model$cross_validation_metrics_summary[2,]) ## Pick out the "AUC" row
}

pred=as.data.frame(h2o.predict(cvgbm,test2))
write.csv(pred,file="predsuck1.csv")
prob = NULL
k=10
for (i in 1:k) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  if (is.null(prob)) prob = h2o.predict(gbm, test2)$f
  else prob = prob + h2o.predict(gbm, test2)$f
}
gbm@allparameters
prob <- prob/k
prob=as.data.frame(prob)
head(prob)
h2o.auc(h2o.predict(gbm,test))
######make submisiion
preds <- as.data.frame(h2o.predict(gbm, test))
head(preds)
gbm@model$validation_metrics@metrics$max_criteria_and_metric_scores




test1[[response]]
preds
write.csv(preds,file="predsuck.csv")
install.packages("mlr")
library(mlr)


h2o.varimp(gbm)
h2o.varimp_plot(gbm)
h2o.partialPlot(object =gbm, data =train, cols = c("offervalue","repeater"))
gbm@parameters
h2o.download_pojo(gbm)
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

plot(gbm)
plot(gbm, timestep = "duration", metric = "auc")
plot(gbm, timestep = "number_of_trees", metric = "auc")
 gbm
legend(x=15000, y=60, legend=c("gbm", "H2O.GBM"), col=c(1,2), lty=c(1,1), bty ='n')
   
     
plot(gbm, timestep = "number_of_trees", metric = "rmse")
plot(gbm, timestep = "number_of_trees", metric = "mae")






## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
hyper_params = list( max_depth = seq(2,30,2) )
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid2 <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="randomForest",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate

  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)      
grid2
sortedGrid <- h2o.getGrid("depth_grid", sort_by = "auc", decreasing = TRUE)    
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01), 
  ntrees=c(1000,2000,4000,6000,8000,10000),
  
  ## search a large space of column sampling rates per split
                                        
  
  ## search a large space of column sampling rates per tree
                               
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
                       
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "AUC",
  stopping_tolerance = 1e-3
)
grid3 <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "randomForest",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid4", 
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
                                                              
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
                                                       
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
                                                
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)

grid3

sortedGrid4 <- h2o.getGrid(grid_id = "final_grid1", sort_by = "AUC", decreasing = TRUE)    
sortedGrid3
h2o.shutdown()

for (i in 1:5) {
  rf2 <- h2o.getModel(sortedGrid3@model_ids[[i]])
  print(h2o.auc(h2o.performance(gbm, valid = TRUE)))
}
rf2 <- h2o.getModel(sortedGrid@model_ids[[1]])
print(h2o.auc(h2o.performance(rf2, newdata = test)))

print(cvrf@model$cross_validation_metrics_summary[2,]) #
preds=as.data.frame(h2o.predict(rf2,test2))
write.csv(preds,file="predsuck4.csv")
prob = NULL
k=10
for (i in 1:k) {
  rf2.1 <- h2o.getModel(sortedGrid3@model_ids[[i]])
  if (is.null(prob)) prob = h2o.predict(rf2.1, test2)$f
  else prob = prob + h2o.predict(rf2.1, test2)$f
}
gbm@allparameters
prob <- prob/k
prob=as.data.frame(prob)
write.csv(prob,file="predsuck5.csv")


args(h2o.randomForest)
rf.model=h2o.randomForest(x=predictors,y=response,stopping_metric = "AUC",max_depth=16,training_frame = train,        ## the H2O frame for training
                 validation_frame = valid, ntrees=2000,stopping_tolerance = 0.001,stopping_rounds = 2,seed=10000)          ## Stop fitting new trees when the 2-treescore_each_iteration = T,)
cvrf <- do.call(h2o.gbm,
                 ## update parameters in place
                 {
                   p <- rf.model@parameters
                   p$model_id = NULL          ## do not overwrite the original grid model
                   p$training_frame = df      ## use the full dataset
                   p$validation_frame = NULL  ## no validation frame
                   p$nfolds = 5               ## cross-validation
                   p
                 }
)



prob = NULL
k=10
for (i in 1:k) {
  <- h2o.getModel(sortedGrid@model_ids[[i]])
if (is.null(prob)) prob = h2o.predict(gbm, test2)$f
else prob = prob + h2o.predict(gbm, test2)$f
}
gbm@allparameters
prob <- prob/k



h2o.auc(h2o.performance(rf.model, test))
#hyper_params = list( max_depth = seq(1,29,2) )
hyper_params = list( ntrees = c(1000,2000,4000,6000) )
grid2 <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="randomForest",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  max_depth=16,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
                                                          
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
                                                 
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10
  
)

sortedGrid1 <- h2o.getGrid("depth_grid", sort_by = "auc", decreasing = TRUE) 
for (i in 1:5) {
  rf.model1 <- h2o.getModel(sortedGrid1@model_ids[[i]])
  print(h2o.auc(h2o.performance(rf.model1, valid = TRUE)))
}
rf.model1 <- h2o.getModel(sortedGrid1@model_ids[[1]])
print(h2o.auc(h2o.performance(rf.model, newdata = test)))





rf.model=h2o.randomForest(x=predictors,y=response,stopping_metric = "AUC",max_depth=16,training_frame = df,nfolds = 5,fold_assignment = "Modulo", ntrees=2000,stopping_tolerance = 0.001,stopping_rounds = 2,seed=1,keep_cross_validation_predictions = TRUE)          ## Stop fitting new trees when the 2-treescore_each_iteration = T,)
h2o.auc(h2o.performance(rf.model, newdata = test))

preds=as.data.frame(preds)
write.csv(preds,file='predsrf2.csv')
k=h2o.loadModel("/Users/sbiradar/Desktop/gbm/final_grid_model_2")

intrain <- createDataPartition(y = df1$repeater, p= 0.7, list = FALSE)
training <- df1[intrain,]
testing <- df1[-intrain,]


trctrl <- trainControl(method = "repeatedcv", number = 20,classProbs = TRUE,repeats =10)
set.seed(3333)
knn_fit <- train(repeater ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)




library(devtools)
install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package",force=TRUE)

library(h2oEnsemble)
df=as.h2o(df1)
learner <- c( "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper","h2o.glm.wrapper")
metalearner <- "h2o.randomForest.wrapper"
family <- "binomial"
fit <- h2o.ensemble(x = predictors, y = response, 
                    training_frame = df, 
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))
library(data.table)
perf <- h2o.ensemble_performance(fit, newdata = test)
test2=as.h2o(test1)
pred <- predict( fit, test2)
pred=as.data.frame(pred$basepred)
write.csv(pred,file="predsucks7.csv")

h2o.performance(p,test)

h2o.shutdown()

localH2o <- h2o.init(nthreads = -1, max_mem_size = "20G")
#############trying ensemble model building here ...have to manually update parameters of best models in these wrappers
install.packages("https://h2o-release.s3.amazonaws.com/h2o-ensemble/R/h2oEnsemble_0.1.9.tar.gz", repos = NULL)
install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")
library(h2oEnsemble)
h2o.glm.1 <- function(..., alpha = 0.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.2 <- function(..., alpha = 0.5) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 1.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.randomForest.1 <- function(..., ntrees = 200, nbins = 50, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.randomForest.2 <- function(..., ntrees = 200, sample_rate = 0.75, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.3 <- function(..., ntrees = 200, sample_rate = 0.85, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.4 <- function(..., ntrees = 200, nbins = 50, balance_classes = TRUE, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, balance_classes = balance_classes, seed = seed)
h2o.gbm.1 <- function(..., ntrees = 100, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, seed = seed)
h2o.gbm.2 <- function(..., ntrees = 100, nbins = 50, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.gbm.3 <- function(..., ntrees = 100, max_depth = 10, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.gbm.4 <- function(..., ntrees = 100, col_sample_rate = 0.8, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.5 <- function(..., ntrees = 100, col_sample_rate = 0.7, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.6 <- function(..., ntrees = 100, col_sample_rate = 0.6, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.7 <- function(..., ntrees = 100, balance_classes = TRUE, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, balance_classes = balance_classes, seed = seed)
h2o.gbm.8 <- function(..., ntrees = 100, max_depth = 3, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.3 <- function(..., hidden = c(500,500), activation = "RectifierWithDropout", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.4 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, balance_classes = TRUE, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, balance_classes = balance_classes, seed = seed)
h2o.deeplearning.5 <- function(..., hidden = c(100,100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.6 <- function(..., hidden = c(50,50), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.7 <- function(..., hidden = c(100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation)
#
learner <- c("h2o.glm.wrapper",
             "h2o.randomForest.1", "h2o.randomForest.2",
             "h2o.gbm.1", "h2o.gbm.6", "h2o.gbm.8"
             )
#
metalearner="h2o.randomForest.1"
fit <- h2o.ensemble(x = predictors, y = response, 
                    training_frame = df,
                    family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner
                    )

test

perf <- h2o.ensemble_performance(fit,test)

k=predict(fit,as.h2o(test1))
write.csv(as.data.frame(k$pred),file="predensemble.csv")
k$pred
##########################Some final thoughts#######trying to implement xgboost and quantile regression to obtain final model
