# Old - Binary Checks using FFT (slow!) ----

# libs needed
library(FFTrees)
library(dplyr)



# load in data-sets
load("array-all.Rdata")

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






##################
# sparklyr demo - not used!
library(dplyr)
library(sparklyr)

# connect to instance
sc <- spark_connect(master = "local")
features_tbl <- spark_read_csv(sc, name = 'featLib', path = '~/Research/cvCells/macro-tests/in-grey-seg/array.all.trim.csv')

# fit linear model
partitions <- features_tbl %>%
  sdf_partition(training = 0.75, test = 0.25, seed = 1099)

fit <- partitions$training %>%
  ml_linear_regression(response = "x_0_s_area", features = c("x_0_s_perimeter", "x_0_m_majoraxis"))

summary(fit)


# kmeans test
kmeans_model <- features_tbl %>%
  select(x_0_s_area, x_0_m_majoraxis) %>%
  ml_kmeans(centers = 3)

print(kmeans_model)

# predict the associated class
predicted <- sdf_predict(kmeans_model, features_tbl) %>%
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





