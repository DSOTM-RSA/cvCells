# preProcess - Multiclass: preProcessing in caret

library(caret)
load("array-all.Rdata")

# indentifying correlated parameters (i.e not for PLS)
feats <- array.dfs[-1]
descrCor <- cor(feats)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
filteredDescr <- feats[,-highlyCorDescr]
array.all.trim<-cbind(array.dfs[1],filteredDescr)

write.csv(array.all.trim,file = "array.all.trim.csv",row.names = FALSE) # write out feature matrix to .csv

rm(array.dfs,feats,descrCor,highlyCorDescr,filteredDescr,array.all.trim) #clean up


# Part One: Using Spark ML ----

# load libraries
library(sparklyr) # first for linux: may have to move pre-process
sc <- spark_connect(master = "local") # connect to spark instance

library(dplyr)


# load feature data-set (trimmed un-correlate feats)

# TODO: function to determine which system I'm on and set for read path 

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


