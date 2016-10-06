# cvCells
# Using shape and texture features to classify microscopy images


# Part I - Data Management
library(EBImage)

# species names here for dBase
sp.sec<-"ipat"

# basic book-keeping -- list all files
refs.all <-list.files(pattern = paste0("*-g-",sp.sec))
segs.all <-list.files(pattern = paste0("*-g-",sp.sec))

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
  
  #--->
  # can trim here or outside loop
  #crt = which(fts[,6]>=10000) # size criteria here
  #fts.trim = fts[crt,]
  #--->
  
  # assigning source file ids to rownames for completeness
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

# write out feature matrix
write.csv(array.images,file = paste0 ("array","-",sp.sec,".csv"),row.names = TRUE)

rm(list = ls(pattern = "^shapes"))
rm(list = ls(pattern = "^textures"))

# Write out data-sets
library(stringr)
library(magrittr)

# to DF - change name here!!
array.ipat.dfs <-as.data.frame(array.images)

# create a column of image-names 
rNames <-rownames(array.ipat.dfs)
rNames.tag <-str_sub(rNames,-4)

array.ipat.dfs %<>% cbind(rNames.tag,.)
save(array.ipat.dfs,file="array-ipat.Rdata")


# Part II - Model Creation

library(FFTrees)
library(dplyr)
library(magrittr)

# load in data-sets
load("array-bspp.Rdata")
load("array-iacu.Rdata")
load("array-snep.Rdata")
load("array-squa.Rdata")
load("array-ipat.Rdata")
join.df<-rbind(array.bspp.dfs,array.iacu.dfs,array.snep.dfs,array.squa.dfs,array.ipat.dfs)


# make binary for simple 1:1 comparison
sp.1<-"iacu"
sp.2<-"ipat"

join.df.bin <- join.df %>%  filter(.,rNames.tag == sp.1 | rNames.tag == sp.2)

join.df.bin$tagBinary <- 0
join.df.bin$tagBinary[join.df.bin$rNames.tag == sp.2] <- 1


array.fft <- FFTrees(formula = tagBinary ~.,
                        data = join.df.bin[,2:117])

array.fft

plot(array.fft, 
     main = "Dino FFT", 
     decision.names = c(sp.1, sp.2))


# function for bayesian classification
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))

apply(x,2, function(x) exp(-(x[]-mean(x)+sd(x))^2/2*sd(x)))
apply(x,2, function(x) exp(-(x[]-mean(x))^2/2*sd(x)))
apply(x,2, function(x) exp(-(x[]-mean(x)-sd(x))^2/2*sd(x)))


library(caret)
library(AppliedPredictiveModeling)

# using untrimmed data: join.df
  
transparentTheme(trans = .9)
featurePlot(x = join.df[, 91:100], 
            y = join.df$rNames.tag,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(5, 2), 
            auto.key = list(columns = 2))

