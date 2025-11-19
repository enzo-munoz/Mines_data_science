## QUESTION 2

library(cluster)
library(dbscan)
library(class)
library(caret)

# Préparation des données
data(iris)

X <- iris[,1:4]
X_scaled <- scale(X)

acp <- prcomp(X_scaled)

# Méthode des K-moyennes avec K = 3

set.seed(123)

km3 <- kmeans(X_scaled, centers = 3, nstart =25)

km3$size
km3$centers
head(km3$cluster)

table(Cluster = km3$cluster, Species = iris$Species)

plot(acp$x[,1], acp$x[,2],
     col = km3$cluster,
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Clustering k-means (K = 3) sur iris")
legend("topright", legend = paste("Cluster", 1:3),
       col = 1:3, pch = 19)

# Méthode K-médoïdes (PAM) avec K=3

pam3 <- pam(X_scaled, k = 3)

pam3$medoids  
pam3$clustering
pam3$objective

table(Cluster = pam3$clustering, Species = iris$Species)

plot(acp$x[,1], acp$x[,2],
     col = pam3$clustering,
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Clustering PAM (K = 3) sur iris")
legend("topright", legend = paste("Cluster", 1:3),
       col = 1:3, pch = 19)

# Classification hiéarchique par méthode de Ward

d <- dist(X_scaled, method = "euclidean")

hc <- hclust(d, method = "ward.D2")

plot(hc, labels = FALSE, main = "Classification hiérarchique (Ward) - iris")

cl_hc <- cutree(hc, k = 3)

table(Cluster = cl_hc, Species = iris$Species)

plot(acp$x[,1], acp$x[,2],
     col = cl_hc,
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Clustering hiérarchique (Ward, k = 3) sur iris")
legend("topright", legend = paste("Cluster", 1:3),
       col = 1:3, pch = 19)

# Méthode DBSCAN

kNNdistplot(X_scaled, k = 5)
abline(h = 0.6, lty = 2)

db <- dbscan(X_scaled, eps = 0.6, minPts = 5)

table(db$cluster)
table(Cluster = db$cluster, Species = iris$Species)

cols <- db$cluster
cols <- cols + 1

plot(acp$x[,1], acp$x[,2],
     col = cols,
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Clustering DBSCAN sur iris")
legend("topright",
       legend = paste("Cluster", 1:3),
       col = sort(unique(cols)), pch = 19)

# KNN
set.seed(123)

ctrl <- trainControl(method = "cv", number = 10)

grid <- expand.grid(k = seq(1, 15, by = 2))

knn_cv <- train(Species ~ .,
                data = iris,
                method = "knn",
                trControl = ctrl,
                preProcess = c("center", "scale"),
                tuneGrid = grid)

knn_cv
plot(knn_cv)

knn_cv$bestTune

pred_knn <- predict(knn_cv, newdata = iris)

conf_mat <- confusionMatrix(pred_knn, iris$Species)
conf_mat

plot(acp$x[,1], acp$x[,2],
     col = pred_knn,
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Classification KNN (CV 10-fold) sur iris")
legend("topright",
       legend = paste("Cluster", 1:3),
       col = 1:3,pch = 19)

# Classification déjà faite

plot(acp$x[,1], acp$x[,2],
     col = iris$Species,
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Espèces réelles - iris")
legend("topright", legend = levels(iris$Species),
       col = 1:3, pch = 19)


























































































































































































































