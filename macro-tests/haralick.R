

library(EBImage)

bspp.grey <- readImage("01-g-bspp.tif")
bspp.seg <- readImage("01-s-bspp.tif")
bspp.seg.lab = bwlabel(bspp.seg)

#compute all features and trim down to relative size features
bspp.fts = computeFeatures(bspp.seg.lab,bspp.grey)
ind = which(bspp.fts[,6]>=10000)
bspp.fts.trim = bspp.fts[ind,]

# compute texture
bspp.har<-computeFeatures.haralick(bspp.seg.lab,bspp.grey)
bspp.har.trim = bspp.har[ind,]

obj1<-c(bspp.fts.trim,bspp.har.trim)



iacu.grey <- readImage("03-g-iacu.tif")
iacu.seg <- readImage("03-s-iacu.tif")
iacu.seg.lab = bwlabel(iacu.seg)

display(iacu.seg.lab, title='Embryos Grey')

#compute all features and trim down to relative size features
iacu.fts = computeFeatures(iacu.seg.lab,iacu.grey)
ind = which(iacu.fts[,6]>=10000)
iacu.fts.trim = iacu.fts[ind,]

# compute texture
iacu.har<-computeFeatures.haralick(iacu.seg.lab,iacu.grey)
iacu.har.trim = iacu.har[ind,]

obj2<-c(iacu.fts.trim,iacu.har.trim)





em.grey = readImage("embryos-grey.tif")
display(em.grey, title='Embryos Grey')

em.bin = readImage("embryos-bin.tif")
display(em.bin, title='binary-segmented')


em.bin.lab = bwlabel(em.bin)
display(normalize(em.bin.lab), title='labelled image')

z.colours = colorLabels(em.bin.lab)
display(z.colours, title='colored segmentation')

xx = em.grey[50:250,400:550]
display(xx)

dis <- function(df,p,xw=50,yw=50){
  xmin <- round(df[p,1]-xw,0)
  xmax <- round(df[p,1]+xw,0)
  ymin <- round(df[p,2]-yw,0)
  ymax <- round(df[p,2]+yw,0)
  
  plotter <- df[xmin:xmax,ymin:ymax]
  return(plotter)
  
}

round(ft[6,2]-50,0)

res<-dis(df=ft,p = 6)




fts = computeFeatures.shape(em.bin.lab)
fts

ft = computeFeatures(em.bin.lab, em.grey, xname="nucleus")
cat("median features are:\n")
apply(ft, 2, median)


fthar<-computeFeatures.haralick(em.bin.lab,em.grey)


cols = c('black', sample(rainbow(max(y))))
zrainbow = Image(cols[1+y], dim=dim(y))
display(zrainbow, title='Cell nuclei (recolored)')
