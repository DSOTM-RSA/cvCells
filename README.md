# cvCells

Exploring image processing and statistical learning, to automate identification and quantification of pollen, dinocysts and cells.

Sketch Training Workflow:

- aquire images (+/- 50) per class
- center, trim and scale to common size 128128 (ImageMagick)
- sample N times random population of training data for each class
- run several model fits (R + Spark + H2O)
- plot size of training sample vs performance for estimate of performance improvement with dataset growth

Ideas (grow training set)

- synthetic data generation using PCA composite images
- SDG with domain randomization
- GAN + export neural net classification

Ideas (machine learning approaches)

- mxnet
- CNTK 



