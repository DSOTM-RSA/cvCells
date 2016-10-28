# preProcess - Multiclass: preProcessing in caret

library(caret)
load("array-all.Rdata")

# indentifying correlated parameters (i.e not for PLS)
feats <- array.dfs[c(-1,-2,-3)] # remove x and y pos and factor
descrCor <- cor(feats)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
filteredDescr <- feats[,-highlyCorDescr]
array.all.trim<-cbind(array.dfs[1],filteredDescr)

write.csv(array.all.trim,file = "array.all.trim.csv",row.names = FALSE) # write out feature matrix to .csv

rm(array.dfs,feats,descrCor,highlyCorDescr,filteredDescr,array.all.trim) #clean up


# Part One: Using Spark ML ----

# load libraries
library(sparklyr) # first for linux: may have to move pre-process
library(rsparkling)
library(h2o)
library(dplyr)

# connect to spark instance
sc <- spark_connect(master = "local") 


# TODO: function to determine which system I'm on and set for read path 

# load feature data-set (trimmed un-correlate feats)
features_tbl <- spark_read_csv(sc, name = 'featLib', path = '~/Research/cvCells/macro-tests/in-grey-seg/array.all.trim.csv')
features_tbl <- spark_read_csv(sc, name = 'featLib', path = "~/Documents/GitArchive/cvCells/macro-tests/in-grey-seg/array.all.trim.csv")


# building a full random forest
rf_full_model <- features_tbl %>% 
  ml_random_forest(rNames_tag ~., type = "classification")

# make predictions
rf_full_predict <- sdf_predict(rf_full_model, features_tbl) %>% 
  ft_string_indexer("rNames_tag","rNames_idx") %>% collect

table(rf_full_predict$rNames_idx,rf_full_predict$prediction) # print the classification results

ft_string2idx <- features_tbl %>% # mapping of labels to indicies
  ft_string_indexer("rNames_tag", "rNames_idx") %>%
  ft_index_to_string("rNames_idx", "rNames_remap") %>%
  collect

table(ft_string2idx$rNames_idx,ft_string2idx$rNames_remap) # show mapping


# Part Two: Using H2o ML ----

partitions <- features_tbl %>% 
  sdf_partition(training = 0.75, test = 0.5, seed = 1099)

dat.H2o.train <- as_h2o_frame(sc, features_tbl)

training <- as_h2o_frame(sc, partitions$training)
test <- as_h2o_frame(sc, partitions$test)

# kmeans test
kmeans_model <- h2o.kmeans(training_frame = training, 
                           x = 2:31,
                           k = 3,
                           seed = 1)

h2o.centers(kmeans_model)
h2o.centroid_stats(kmeans_model)

# pca test
pca_model <- h2o.prcomp(training_frame = training,
                        x = 2:31,
                        k = 4,
                        seed = 1)
print(pca_model)


# RF test

y <- "rNames_tag"
x <- setdiff(names(dat.H2o.train), y)
dat.H2o.train[,y] <- as.factor(dat.H2o.train[,y])


splits <- h2o.splitFrame(dat.H2o.train, seed = 1)


rf_model <- h2o.randomForest(x = x, 
                             y = y,
                             training_frame = splits[[1]],
                             validation_frame = splits[[2]],
                             nbins = 32,
                             max_depth = 5,
                             ntrees = 20,
                             seed = 1)

h2o.confusionMatrix(rf_model, valid = TRUE)

# get variable importance
h2o.varimp_plot(rf_model)

# GBM test
gbm_model <- h2o.gbm(x = x, 
                     y = y,
                     training_frame = splits[[1]],
                     validation_frame = splits[[2]],                     
                     ntrees = 20,
                     max_depth = 3,
                     learn_rate = 0.01,
                     col_sample_rate = 0.7,
                     seed = 1)

h2o.confusionMatrix(gbm_model, valid = TRUE)


# deep learning test

dl_fit <- h2o.deeplearning(x = x, y = y,
                           training_frame = splits[[1]],
                           epochs = 15,
                           activation = "Rectifier",
                           hidden = c(21, 5, 21),
                           input_dropout_ratio = 0.7)


h2o.performance(dl_fit, newdata = splits[[2]])
h2o.performance(dl_fit)
