format(Sys.time())

em = readImage("embryos.tif")
x = em[,,1]

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
