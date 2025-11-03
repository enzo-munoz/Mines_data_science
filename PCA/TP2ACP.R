if (!require(readxl)) install.packages("readxl")
library(readxl)


data <- read_excel("TP4_covC1234_DS19_20.xlsx", sheet = 1)

boxplot(data[,1:14],
        main = "Boxplots des 6 premières variables quantitatives")

data[data == 0] <- NA
data <- data[data$TYPE != "?", ]

# Remplacement des outliers par la mediane

for (col in names(data)) {
  
  # Vérifier si la colonne est numérique
  if (is.numeric(data[[col]])) {
    
    # Calculer les quartiles et l'IQR
    q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    
    # Définir les bornes des valeurs normales
    borne_inf <- q1 - 1.5 * iqr
    borne_sup <- q3 + 1.5 * iqr
    
    # Trouver les indices des outliers
    outliers <- which(data[[col]] < borne_inf | data[[col]] > borne_sup)
    
    # Calculer la médiane sans les outliers
    median_sans_outliers <- median(data[[col]][-outliers], na.rm = TRUE)
    
    # Remplacer les outliers par cette médiane
    data[[col]][outliers] <- median_sans_outliers
  }
}

boxplot(data[,1:14],
        main = "Boxplots des 6 premières variables quantitatives")

sum(is.na(data))

#Matrice de correlation

data_num <- data[, sapply(data, is.numeric)]
data_qual <- data[, !sapply(data, is.numeric)]

names(data_num) <- make.names(names(data_num))

cor_matrix <- cor(data_num, use = "pairwise.complete.obs")

heatmap(cor_matrix,
        Colv = NA, Rowv = NA,           
        col = colorRampPalette(c("blue", "white", "red"))(20),
        scale = "none", 
        margins = c(8,8),
        main = "Matrice de corrélation")




#regression pour les valeurs manquantes

for (col in names(data_num)) {
  
  # Vérifie s’il y a des valeurs manquantes dans la variable
  if (any(is.na(data_num[[col]]))) {
    
    cat("\nVariable à compléter :", col, "\n")
    
    # Récupérer les corrélations absolues avec les autres variables
    cor_values <- abs(cor_matrix[col, ])
    
    # Trier les corrélations (de la plus forte à la plus faible)
    cor_values <- sort(cor_values, decreasing = TRUE)
    
    # Choisir les 2 variables les plus corrélées (hors elle-même)
    vars_predict <- names(cor_values)[2:3]
    cat("Variables utilisées pour la régression :", vars_predict, "\n")
    
    # Construire la formule de régression
    formule <- as.formula(paste(col, "~", paste(vars_predict, collapse = "+")))
    
    # Enlever les lignes avec NA pour la variable cible (entraînement)
    data_train <- data_num[!is.na(data_num[[col]]), ]
    
    # Ajuster le modèle de régression linéaire multiple
    model <- lm(formule, data = data_train)
    
    # Identifier les lignes avec NA dans la variable cible
    idx_na <- which(is.na(data_num[[col]]))
    
    # Créer le sous-ensemble de prédiction
    data_predict <- data_num[idx_na, vars_predict]
    
    # Prédire et remplacer les valeurs manquantes
    data_num[[col]][idx_na] <- predict(model, newdata = data_predict)
  }
}

#apres la regression nous avons toujours une valeur N/A pour la colonne 14ane, on la remplace donc manuelment avec la mediane

# Calculer la médiane de la variable sans NA
mediane_14ane <- median(data_num$X14_ane, na.rm = TRUE)

# Remplacer le NA par cette médiane
data_num$X14_ane[is.na(data_num$X14_ane)] <- mediane_14ane

# Mise à jour le jeu de données complet
data <- cbind(data_qual, data_num)

colSums(is.na(data))


# Encodage Variables qualitatives

data$SAISON <- as.factor(data$SAISON)
data$TYPE <- as.factor(data$TYPE)

# série de tableau et boxplot pour inspection préliminaire


# Tableau croisé simple
table(data$SAISON, data$TYPE)

# Fréquences relatives
prop.table(table(data$SAISON, data$TYPE))

# Exemple pour la variable BTM
boxplot(BTM ~ SAISON, data = data,
        main = "Concentration de BTM selon la saison",
        xlab = "Saison", ylab = "BTM",
        col = c("skyblue", "lightcoral"))


# Exemple pour Tot_OcNoDecana
boxplot(Tot_OcNoDecana ~ TYPE, data = data,
        main = "Tot_OcNoDecana selon le type d'environnement",
        xlab = "Type d'environnement", ylab = "Tot_OcNoDecana",
        col = c("lightgreen", "gold", "lightblue"))

# Moyennes par saison
aggregate(data_num, by = list(SAISON = data$SAISON), mean)

# Moyennes par type d'environnement
aggregate(data_num, by = list(TYPE = data$TYPE), mean)

# Corrélation entre variables (vue rapide)
heatmap(cor(data_num, use = "pairwise.complete.obs"),
        Colv = NA, Rowv = NA,
        col = colorRampPalette(c("blue", "white", "red"))(20),
        main = "Corrélations entre composés chimiques")

# creation de la matrice X , on va normaliser car les grandeurs de nos données ont des unités et ordres de grandeurs differentes

X <- data_num   
dim(X)


if(!require(FactoMineR)) install.packages("FactoMineR")
library(FactoMineR)

# 2. Réaliser l’ACP sur les variables numériques
res <- PCA(data, 
           quali.sup = 1:4,  
           scale.unit = TRUE,
           graph = TRUE)


summary(res)


barplot(res$eig[,2],
        names.arg = 1:nrow(res$eig),
        main = "Pourcentage de variance expliquée par composante",
        xlab = "Composante principale",
        ylab = "Variance expliquée (%)",
        col = "skyblue")

# === 5. Cumul des variances expliquées ===
plot(cumsum(res$eig[,2]), type = "b",
     main = "Cumul de la variance expliquée",
     xlab = "Nombre de composantes",
     ylab = "Variance cumulée (%)",
     col = "red", pch = 19)

# Plan factoriel des individus
plot.PCA(res, choix = "ind", title = "Projection des individus sur le plan (1,2)")

# Coloration des individus selon la saison
plot.PCA(res, choix = "ind", habillage = 2, cex = 0.8,
         title = "Individus colorés selon la saison")

# Coloration des individus selon le type d’environnement 
plot.PCA(res, choix = "ind", habillage = 1, cex = 0.8,
         title = "Individus colorés selon le type d'environnement")

plot.PCA(res, choix = "var", title = "Cercle des corrélations des variables")
