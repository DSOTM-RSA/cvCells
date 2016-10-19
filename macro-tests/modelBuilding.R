
# Part One: Using Spark ML ----

# load libraries
library(dplyr)
library(sparklyr)

# connect to spark instance
sc <- spark_connect(master = "local")

# load feature data-set (trimmed un-correlate feats)
features_tbl <- spark_read_csv(sc, name = 'featLib', path = '~/Research/cvCells/macro-tests/in-grey-seg/array.all.trim.csv')

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


