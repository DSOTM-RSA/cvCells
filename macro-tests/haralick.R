# cvCells
# Using shape and texture features to classfiy microscopy images


# Part I - Data Management
library(EBImage)

# basic book-keeping
refs.all <- list.files(pattern = "*-g-bspp")
segs.all <- list.files(pattern = "*-s-bspp")

beg = 1
end = as.numeric(length(refs.all))

# short labels for meta-data
ref <- as.character(strsplit(refs.all,".tif"))
seg <- as.character(strsplit(segs.all,".tif"))



# main loop
for (i in beg:end){
  
  # read and label images
  refs.array <-readImage(refs.all[i])
  seg.array <-readImage(segs.all[i])
  seg.labelled <-bwlabel(seg.array)
  
  # contruct holder for feature-results
  writer.1 <- paste0("shapes",i)
  writer.2 <-paste0("textures",i)
  
  # compute features
  fts.shp <- computeFeatures(seg.labelled,refs.array)
  fts.tex <- computeFeatures.haralick(seg.labelled,refs.array)
  #--->
  # can trim here or outside loop
  #crt = which(fts[,6]>=10000) # size criteria here
  #fts.trim = fts[crt,]
  #--->
  
  # assigning rownames here for completeness
  rownames(fts.shp)<-rep(ref[i],dim(fts.shp)[1])
  
  # use assign for each feature set
  assign(writer.1,fts.shp)
  assign(writer.2,fts.tex)

}

# concenate pieces into one matrix
rm(list=ls(pattern = "^fts"))
pieces.shp <- Filter(function(x) is(x, "matrix"),mget(ls(pattern = "^shapes")))
pieces.tex <- Filter(function(x) is(x, "matrix"), mget(ls(pattern= "^textures")))

# construct lists
data.shapes<-do.call(rbind,pieces.shp)
data.textures<-do.call(rbind,pieces.tex)


# trim data
crt = which(data.shapes[,6]>=10000) # size criteria here
data.shapes.trim = data.shapes[crt,] # apply to shape data
data.textures.trim = data.textures[crt,] # apply for textures

# bind rows for full array of features
array.images<-cbind(data.shapes.trim,data.textures.trim)

# write out feature matrix
write.csv(array.images,file = "array.csv",row.names = TRUE)


# Part II - Model Creation
library(stringr)
library(magrittr)

# to DF
array.dfs <- as.data.frame(array.images)

# create a column of image-names 
rNames <- rownames(array.dfs)
rNames.tag<- str_sub(rNames,-4)

array.dfs %<>% cbind(rNames.tag,.)


