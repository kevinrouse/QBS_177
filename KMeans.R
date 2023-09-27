
setwd("/Users/kevinrouse78/Desktop/QBS 177/guihub")
bc <- read.csv('Cancer.csv')
bc$X <- NULL
bc[,1] <- NULL
bc$diagnosis = ifelse(bc$diagnosis == "M", 'Malignant', 'Benign')
sum(is.na(bc))
kbc <- bc[,-1]
o<-kmeans(kbc,2,25)
o$cluster <- ifelse(o$cluster == 1, 2, 1)


table(o$cluster, bc$diagnosis)

(356+130)/(356+130+82+1)
reg = bc %>% 
  select('diagnosis', 'texture_mean','area_mean', 'compactness_mean', 'concavity_mean', 'concave.points_mean', 'fractal_dimension_mean', 'radius_se', 'texture_se', 'smoothness_se', 'compactness_se','concavity_se', 'concave.points_se', 'symmetry_se', 'radius_worst', 'texture_worst', 'area_worst', 'smoothness_worst', 'concavity_worst', 'concave.points_worst', 'symmetry_worst', 'fractal_dimension_worst')
reg$diagnosis = ifelse(reg$diagnosis == "Malignant", 1, 0)
 o1 <- prcomp(reg)
plot(o1)

bc$diagnosis = ifelse(bc$diagnosis == "M", 1, 2)

o$cluster <- ifelse(o$cluster == 1, 2, 1)
library(ggplot2)
my.plot <- ggplot(data=as.data.frame(o1$x), aes(x = PC1, y = -PC2, col=Diagnosis)) +
  geom_point() + ylab("PC2")
my.plot
#clusters
my.plot <- ggplot(data=as.data.frame(o1$x), aes(x = PC1, y = -PC2, col=factor(o$cluster))) +
  geom_point() + labs(color = "Cluster") +ylab("PC2")
my.plot
library(glmnet)
library(dplyr)
# from Digvijay's LASSO analysis - keep only 21 most informative predictors
lasso_vars = c('diagnosis', 'texture_mean', 'area_mean', 'compactness_mean', 'concavity_mean', 'concave.points_mean', 'fractal_dimension_mean', 'radius_se', 'texture_se', 'smoothness_se', 'compactness_se', 'concavity_se', 'concave.points_se', 'symmetry_se', 'radius_worst', 'texture_worst', 'area_worst', 'smoothness_worst', 'concavity_worst', 'concave.points_worst', 'symmetry_worst', 'fractal_dimension_worst')

reg = bc %>% 
  select('diagnosis', 'texture_mean','area_mean', 'compactness_mean', 'concavity_mean', 'concave.points_mean', 'fractal_dimension_mean', 'radius_se', 'texture_se', 'smoothness_se', 'compactness_se','concavity_se', 'concave.points_se', 'symmetry_se', 'radius_worst', 'texture_worst', 'area_worst', 'smoothness_worst', 'concavity_worst', 'concave.points_worst', 'symmetry_worst', 'fractal_dimension_worst')

reg$diagnosis = ifelse(reg$diagnosis == "M", 1, 0)
regr <-glm(diagnosis ~., family=binomial("logit"), dat =reg)
step(glm(diagnosis ~ ., family='binomial', dat =reg),direction ='backward')
bc$diagnosis <- reg$diagnosis


regr <-glmnet(diagnosis ~., family='binomial', data =bc)


I would suggest giving glmnet a try- it introduces a regularization that can help a bit and should be performant.

On the issue of 0/1 probabilities: it means your problem has separation or quasi-separation (a subset of the data that is predicted perfectly and may be running a subset of the coefficients out to infinity). That can cause problems, so you will want to look at the coefficients (especially those that are large and have large uncertainty intervals) and at data with probability scores near zero or one (or link-scores with large absolute values).

sil <-silhouette(o$cluster, dist(kbc))
cumulative_score <- mean(sil[, "sil_width"])
cumulative_score

install.packages("MLmetrics")
library("MLmetrics")


F1_Score(bc$diagnosis, o$cluster, positive = NULL)
