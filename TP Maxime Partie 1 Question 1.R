library(ggplot2)
library(dplyr)
library(GGally)
library(vcd)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)

data <- read.csv("7 bank_marketing.csv", sep = ";")


# Aperçu des données
cat("=== Structure du dataset ===\n")
str(data)
cat("\n=== Résumé statistique ===\n")
summary(data)
colSums(is.na(data))

# --- Encodage des variables catégoriques ---

data <- data %>%
  mutate(across(where(is.character), as.factor))

str(data)
summary(data)
# --- 3. Distribution de la variable cible ---
ggplot(data, aes(x = class, fill = class)) +
  geom_bar() +
  labs(title = "Répartition de la variable cible (yes / no)",
       x = "Souscription à un dépôt à terme", y = "Nombre de clients") +
  theme_minimal()

# --- 4. Variables socio-démographiques ---

# Âge (boxplot + histogramme)
ggplot(data, aes(x = class, y = age, fill = class)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Âge selon la souscription", x = "Classe", y = "Âge") +
  theme_minimal()

ggplot(data, aes(x = age, fill = class)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Distribution de l'âge selon la souscription", x = "Âge", y = "Fréquence") +
  theme_minimal()

# Statut marital
ggplot(data, aes(x = marital, fill = class)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion de souscription selon le statut marital",
       x = "Statut marital", y = "Proportion") +
  theme_minimal()

# Niveau d'éducation
ggplot(data, aes(x = education, fill = class)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion de souscription selon le niveau d'éducation",
       x = "Éducation", y = "Proportion") +
  theme_minimal()

# --- 5. Variables financières ---

# Solde du compte (balance)
ggplot(data, aes(x = class, y = balance, fill = class)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Distribution du solde selon la souscription",
       x = "Classe", y = "Solde (€)") +
  theme_minimal()

ggplot(data, aes(x = balance, fill = class)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  labs(title = "Histogramme du solde bancaire selon la souscription",
       x = "Solde (€)", y = "Fréquence") +
  theme_minimal()

# Crédit logement et crédit personnel
ggplot(data, aes(x = housing, fill = class)) +
  geom_bar(position = "fill") +
  labs(title = "Souscription selon la présence d’un crédit logement",
       x = "Crédit logement", y = "Proportion") +
  theme_minimal()

ggplot(data, aes(x = loan, fill = class)) +
  geom_bar(position = "fill") +
  labs(title = "Souscription selon la présence d’un crédit personnel",
       x = "Crédit personnel", y = "Proportion") +
  theme_minimal()



# --- 6. Corrélations numériques globales ---
num_data <- data %>% select_if(is.numeric)
if (ncol(num_data) > 1) {
  print("=== Matrice de corrélation des variables numériques ===")
  print(cor(num_data, use = "complete.obs"))
  ggcorr(num_data, label = TRUE, label_size = 3)
}

# --- 7. Tests statistiques (Chi² d'indépendance) ---
cat("\n=== Tests du Chi² d'indépendance ===\n")
if (is.factor(data$marital)) print(assocstats(table(data$marital, data$class)))
if (is.factor(data$housing)) print(assocstats(table(data$housing, data$class)))
if (is.factor(data$loan)) print(assocstats(table(data$loan, data$class)))
if (is.factor(data$education)) print(assocstats(table(data$education, data$class)))
if (is.factor(data$poutcome)) print(assocstats(table(data$poutcome, data$class)))

# --- 8. Analyse synthétique ---
cat("
---------------------------------------------------------------
INTERPRÉTATION DES TENDANCES :
---------------------------------------------------------------
• Les clients sans crédit logement (housing = no) sont plus enclins à souscrire.
• Ceux avec un crédit personnel (loan = yes) souscrivent moins souvent.
• L'âge médian et le solde (balance) sont plus élevés chez les souscripteurs.
• Le succès d'une campagne précédente (poutcome = success) est un facteur clé.
• Les tests du Chi² confirment des liens significatifs entre 'marital', 'education',
  'housing', 'loan', 'poutcome' et la variable cible 'class'.
---------------------------------------------------------------
")

# --- Séparation apprentissage / test (70 / 30) ---
set.seed(123)
train_index <- createDataPartition(data$class, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]



# --- Construction de l’arbre de décision CART ---
tree_model <- rpart(class ~ ., data = train_data, method = "class", control = rpart.control(cp = 0.01))
rpart.plot(tree_model, main = "Arbre de décision initial")

# --- Validation croisée intégrée et choix du cp optimal ---
printcp(tree_model)
plotcp(tree_model, main = "Sélection du paramètre de complexité (cp)")
best_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]

# --- Élagage (arbre optimal) ---
pruned_tree <- prune(tree_model, cp = best_cp)
rpart.plot(pruned_tree, main = "Arbre de décision élagué (optimal)")

# --- Évaluation sur le jeu de test ---
pred <- predict(pruned_tree, newdata = test_data, type = "class")
conf_mat <- confusionMatrix(pred, test_data$class, positive = "yes")
print(conf_mat)

# --- Courbe ROC et AUC ---
pred_prob <- predict(pruned_tree, newdata = test_data, type = "prob")[, 2]
roc_curve <- roc(test_data$class, pred_prob)
plot(roc_curve, col = "blue", lwd = 2, main = "Courbe ROC - Arbre optimal")
auc_value <- auc(roc_curve)
cat("AUC =", auc_value, "\n")

# --- Interprétation rapide ---
cat("
---------------------------------------------------------------
INTERPRÉTATION DE L'ARBRE DE DÉCISION :
---------------------------------------------------------------
• L'arbre final est obtenu après élagage pour éviter le sur-apprentissage.
• Le paramètre de complexité (cp) a été choisi par validation croisée intégrée.
• L'arbre optimal permet de prédire efficacement la variable 'class'
  (souscription à un dépôt à terme) en fonction des variables explicatives.
• L’évaluation sur le jeu de test fournit la précision et l’AUC du modèle.
---------------------------------------------------------------
")


# ============================================================
# QUESTION 3 : Évaluation détaillée du modèle optimal
# ============================================================

# --- 3(a) Explorer les résultats de classification ---

# Prédictions déjà obtenues
pred <- predict(pruned_tree, newdata = test_data, type = "class")
pred_prob <- predict(pruned_tree, newdata = test_data, type = "prob")[, 2]

# --- i. Detailed Accuracy by Class (Precision, Recall, F1, etc.) ---
# On utilise les sorties de confusionMatrix pour extraire les métriques
conf_mat <- confusionMatrix(pred, test_data$class, positive = "yes")
print(conf_mat)

# Extraire les indicateurs principaux
accuracy <- conf_mat$overall["Accuracy"]
precision <- conf_mat$byClass["Precision"]
recall <- conf_mat$byClass["Recall"]
f1 <- conf_mat$byClass["F1"]
specificity <- conf_mat$byClass["Specificity"]
sensitivity <- conf_mat$byClass["Sensitivity"]

cat("
---------------------------------------------------------------
MÉTRIQUES DE PERFORMANCE DU MODÈLE OPTIMAL (JEU DE TEST)
---------------------------------------------------------------
Accuracy     :", round(accuracy, 3), "\n",
    "Précision    :", round(precision, 3), "\n",
    "Rappel       :", round(recall, 3), "\n",
    "F1-Score     :", round(f1, 3), "\n",
    "Sensibilité  :", round(sensitivity, 3), "\n",
    "Spécificité  :", round(specificity, 3), "\n",
    "---------------------------------------------------------------\n"
)

# --- ii. Courbe ROC et AUC ---
roc_curve <- roc(test_data$class, pred_prob)
plot(roc_curve, col = "blue", lwd = 2, main = "Courbe ROC - Modèle optimal (CART)")
auc_value <- auc(roc_curve)
cat("AUC :", round(auc_value, 3), "\n")

# --- iii. Autres indicateurs possibles ---
# On peut calculer le taux d’erreur, MCC, et Kappa
error_rate <- 1 - accuracy
kappa <- conf_mat$overall["Kappa"]

cat("
Autres mesures :
Taux d’erreur :", round(error_rate, 3), "\n",
    "Cohen’s Kappa :", round(kappa, 3), "\n"
)

# --- Visualisation complémentaire ---
# Graphe comparant la probabilité prédite et la classe réelle
ggplot(data.frame(Prob = pred_prob, Réel = test_data$class), aes(x = Réel, y = Prob, fill = Réel)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Probabilités prédites par le modèle selon la classe réelle",
       x = "Classe réelle", y = "Probabilité prédite (Yes)") +
  theme_minimal()
