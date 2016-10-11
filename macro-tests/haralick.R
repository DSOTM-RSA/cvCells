# cvCells
# Using shape and texture features to classify microscopy images


# Part I - Data Management ----

# load image processing lib
library(EBImage)

# species names here for individual dBase
sp.sec<-"all"

# basic book-keeping -- list all files
refs.all <-list.files(pattern = paste0("*-g-",sp.sec))
segs.all <-list.files(pattern = paste0("*-g-",sp.sec))

# for all samples in library
refs.all <-list.files(pattern = "*-g-")
segs.all <-list.files(pattern = "*-s-")

beg = 1
end = as.numeric(length(refs.all))

# short labels for meta-data
ref <-as.character(strsplit(refs.all,".tif"))
seg <-as.character(strsplit(segs.all,".tif"))



# main loop
for (i in beg:end){
  
  # read and label segmented images
  refs.array <-readImage(refs.all[i])
  seg.array <-readImage(segs.all[i])
  seg.labelled <-bwlabel(seg.array)
  
  # contruct holders for feature-results
  writer.1 <-paste0("shapes",i)
  writer.2 <-paste0("textures",i)
  
  # compute features
  fts.shp <-computeFeatures(seg.labelled,refs.array)
  fts.tex <-computeFeatures.haralick(seg.labelled,refs.array)
  
  # assigning source file ids to rownames for book-keeping
  rownames(fts.shp) <-rep(ref[i],dim(fts.shp)[1])
  
  # use assign for each feature set
  assign(writer.1,fts.shp)
  assign(writer.2,fts.tex)

}

# concenate pieces into one matrix
rm(list=ls(pattern = "^fts"))
pieces.shp <-Filter(function(x) is(x, "matrix"),mget(ls(pattern = "^shapes")))
pieces.tex <-Filter(function(x) is(x, "matrix"), mget(ls(pattern= "^textures")))

# construct lists
data.shapes <-do.call(rbind,pieces.shp)
data.textures <-do.call(rbind,pieces.tex)


# trim data
crt <-which(data.shapes[,6]>=10000) # size criteria here
data.shapes.trim <-data.shapes[crt,] # apply to shape data
data.textures.trim <-data.textures[crt,] # apply for textures

# bind rows for full array of features
array.images <-cbind(data.shapes.trim,data.textures.trim)


# Part II - Data Export ----

# libs needed
library(stringr)
library(magrittr)

rm(list = ls(pattern = "^shapes"))
rm(list = ls(pattern = "^textures"))

# to DF - change name here!!
array.dfs <-as.data.frame(array.images)

# create a column of image-names 
rNames <-rownames(array.dfs)
rNames.tag <-str_sub(rNames,-4)

array.dfs %<>% cbind(rNames.tag,.) # bind to array
save(array.dfs,file=paste0("array","-",sp.sec,".Rdata")) # export as .Rdata file
write.csv(array.dfs,file = paste0("array","-",sp.sec,".csv"),row.names = FALSE) # write out feature matrix to .csv


# Part III - Model Creation ----

# libs needed
library(FFTrees)
library(dplyr)



# load in data-sets
load("array-all.Rdata")

library(sparklyr)# Section A - Binary Classification
sp.0<-"iacu" # assign species 0
sp.1<-"ipat" # assign species 1

dat.bin <- array.dfs %>% filter(.,rNames.tag == sp.0 | rNames.tag == sp.1)

dat.bin$tagBinary <- 0
dat.bin$tagBinary[dat.bin$rNames.tag == sp.1] <-1


dat.bin.fft.mar <- FFTrees(formula = tagBinary~.,
                       data = dat.bin[,2:117],rank.method = "m") # use all features outside label

#dat.bin.fft.con <- FFTrees(formula = tagBinary~.,
#                           data = dat.bin[,2:117],rank.method = "c") # use all features outside label

# print results
dat.bin.fft.mar

# plot tree
plot(dat.bin.fft.mar, 
     main = "Dino FFT", 
     decision.names = c(sp.0, sp.1))

# plotting some of parameters used
library(caret)
library(AppliedPredictiveModeling)


# featurePlot from caret  
transparentTheme(trans = .9)
featurePlot(x = dat.bin[, 91:100], 
            y = dat.bin$rNames.tag,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(5, 2),auto.key = list(columns = 2))



# Section B - Multiclass Classification

library(caret)
load("array-all.Rdata")


# indentifying correlated parameters (i.e not PLS)
descrCor <- cor(array.dfs[-1])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .85)
filteredDescr <- descr[,-highlyCorDescr]


# or preProcessing (scaling and centering)
pp_outp <- preProcess(filteredDescr, 
                     method = c("center", "scale", "YeoJohnson"))

transformed <- predict(pp_outp, newdata = filteredDescr) # actual transformation happens here


# sparklyr
library(sparklyr)
spark_install(version = "1.6.2")

sc <- spark_connect(master = "local")
accidents <- spark_read_csv(sc, name = 'accidents', path = '~/Research/cvCells/macro-tests/in-grey-seg/array-all.csv')

# fit linear model
partitions <- accidents %>%
  sdf_partition(training = 0.75, test = 0.25, seed = 1099)

fit <- partitions$training %>%
  ml_linear_regression(response = "x_0_s_area", features = c("x_0_s_perimeter", "x_0_s_area"))

summary(fit)


# kmeans test
kmeans_model <- accidents %>%
  select(x_0_s_area, x_0_m_majoraxis) %>%
  ml_kmeans(centers = 3)

print(kmeans_model)

# predict the associated class
predicted <- sdf_predict(kmeans_model, accidents) %>%
  collect
table(predicted$rNames_tag, predicted$prediction)

library(ggplot2)
sdf_predict(kmeans_model) %>%
  collect() %>%
  ggplot(aes(x_0_s_area, x_0_m_majoraxis)) +
  geom_point(aes(x_0_m_majoraxis, x_0_s_area, col = factor(prediction + 1)),
             size = 2, alpha = 0.5) + 
  geom_point(data = kmeans_model$centers, aes(x_0_m_majoraxis, x_0_s_area),
             col = scales::muted(c("red", "green", "blue")),
             pch = 'x', size = 12) +
  scale_color_discrete(name = "Predicted Cluster",
                       labels = paste("Cluster", 1:3)) +
  labs(
    x = "x_0_m_majoraxis",
    y = "x_0_s_area",
    title = "K-Means Clustering",
    subtitle = "Use Spark.ML to predict cluster membership with this dataset."
  )


# rf model
rf_model <- accidents %>%
  ml_random_forest(rNames_tag ~ x_0_s_area + x_0_m_majoraxis, type = "classification")

rf_predict <- sdf_predict(rf_model, accidents) %>%
  ft_string_indexer("rNames_tag", "rNames_idx") %>%
  collect

table(rf_predict$rNames_idx, rf_predict$prediction)

ft_string2idx <- accidents %>%
  ft_string_indexer("rNames_tag", "rNames_idx") %>%
  ft_index_to_string("rNames_idx", "rNames_remap") %>%
  collect

table(ft_string2idx$rNames_idx,ft_string2idx$rNames_remap) # show mapping
