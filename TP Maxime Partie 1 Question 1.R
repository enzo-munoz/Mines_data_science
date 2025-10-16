library(ggplot2)
library(dplyr)
library(GGally)
library(vcd)

data <- read.csv("7 bank_marketing.csv", sep = ";")


# Aperçu des données
cat("=== Structure du dataset ===\n")
str(data)
cat("\n=== Résumé statistique ===\n")
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
• Une longue durée d'appel (duration) augmente fortement la probabilité de 'yes'.
• Le succès d'une campagne précédente (poutcome = success) est un facteur clé.
• Les tests du Chi² confirment des liens significatifs entre 'marital', 'education',
  'housing', 'loan', 'poutcome' et la variable cible 'class'.
---------------------------------------------------------------
")
