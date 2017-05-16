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

##############################
# Analysis One: Using Spark ML ----

# order for single use sparklyr
library(sparklyr)
sc <- spark_connect(master = "local",version="1.6.2") # connect to spark instance

library(dplyr) # load dplyr for easy data management verbs

# load feature data-set (trimmed un-correlate feats)
where <- getwd()
features_tbl <- spark_read_csv(sc, name = 'featLib', path = paste0(where,"/array.all.trim.csv"))


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


