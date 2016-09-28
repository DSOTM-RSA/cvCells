library(EBImage)

mask<-readImage("IMG_0126_M.tif")
og <- readImage("IMG_0126.jpg")[,,1]
m <- thresh(og, 10,10,0.05)
m = opening(m, makeBrush(5, shape='disc'))
m = bwlabel(m)

fts = computeFeatures.shape(mask)
mask = bwlabel(mask)

fts = computeFeatures.shape(m)
har <- computeFeatures.haralick(m,og)

y = readImage(system.file("images", "nuclei.tif", package="EBImage"))[,,1]
x = thresh(y, 10, 10, 0.05)
x = opening(x, makeBrush(5, shape='disc'))
x = bwlabel(x)
fts = computeFeatures.shape(x)
