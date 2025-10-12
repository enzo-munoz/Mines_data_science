knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(warning = FALSE)

library (tseries)
library(FinTS)
library(rugarch)
library(Metrics)
library(forecast)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)
library(moments)
library(psych)

data <- read.csv("data.csv", stringsAsFactors = FALSE)


#head(data)
#str(data)

data$temp_C <- (data$TAVG..Degrees.Fahrenheit. - 32)/1.8

#head(data[,c("TAVG..Degrees.Fahrenheit.","temp_C")])

data$Date <- as.Date(data$Date, format="%m/%d/%Y") #Convertir la colonne en objet Date

data_mensuelle <- data %>%
  mutate (mois = floor_date(Date, "month")) %>%
  group_by(mois) %>%
  summarise(T_moy = mean(temp_C, na.rm = TRUE))

head(data_mensuelle)

temperature <- (ts(data_mensuelle$T_moy, start = c(1990,1), frequency = 12))
len<- length(temperature)
moyenne <- mean(temperature, na.rm = TRUE)
mediane <- median(temperature, na.rm = TRUE)
variance <- var(temperature, , na.rm = TRUE)
asymetrie <- skewness(temperature, na.rm = TRUE)
aplatissement <- kurtosis(temperature, na.rm = TRUE)

stats_df <- data.frame(
  Statistique = c("Moyenne", "Médiane", "Variance", "Skewness (asymétrie)", "Kurtosis (aplatissement)"),
  Valeur = c(moyenne, mediane, variance, asymetrie, aplatissement)
)

knitr::kable(
  stats_df,
  caption = "Statistiques descriptives de la série temporelle",
  digits = 3
)

plot(temperature,
     main = " Température moyenne mensuelle à Lyon",
     xlab = "Année",
     ylab = "Température (°C)",
     col = "blue",
     lwd = 2)
grid()
mu<- mean(temperature)

abline(h = mu, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = "Moyenne", col = "red", lty = 2, lwd = 2, bty = "n")

months <- cycle(temperature)

df <- data.frame(Month = factor(months, levels = 1:12, labels = month.abb),
                 Temp = as.numeric(temperature))

# Tracer les boxplots par mois
ggplot(df, aes(x = Month, y = Temp)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution des températures par mois",
       x = "Mois",
       y = "Température (°C)") +
  theme_minimal()

ecart_type_par_mois <- df %>%
  group_by(Month) %>%
  summarise(Ecart_Type_Temp = sd(Temp))

# Création du graphique
ggplot(data = ecart_type_par_mois, aes(x = Month, y = Ecart_Type_Temp)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Écart type mensuel des températures",
    x = "Mois",
    y = "Écart type des températures"
  ) +
  theme_minimal()

ts_diff1 <- diff(temperature, differences = 1)

# Différenciation saisonnière
ts_diff_seas <- diff(temperature, lag = 12)

# Tests sur séries différenciées
par(mfrow = c(2, 2))
plot(temperature, main = "Série Originale", ylab = "Température", col = "blue", lwd = 1.5)
plot(ts_diff1, main = "Différenciation d = 1", ylab = "Diff(Température)", col = "red", lwd = 1.5)
abline(h = 0, lty = 2)
plot(ts_diff_seas, main = "Différenciation Saisonnière (lag=12)", 
     ylab = "Diff(Température)", col = "green4", lwd = 1.5)
abline(h = 0, lty = 2)

# ACF de la série différenciée
acf(ts_diff1, main = "ACF - Série Différenciée (d=1)", lag.max = 36)

# Test ADF sur série différenciée
adf_diff <- adf.test(ts_diff1)
cat("\nTest ADF après différenciation (d=1) :\n")
cat("p-value :", round(adf_diff$p.value, 4), "\n")

des_data<- diff(temperature, lag=12)
plot(des_data, type='o', main = "Série désaisonnalisée par différenciation (lag 12)")
mu <- mean(des_data)
sig.des_data <- sd(des_data)
abline(h=mu, col="red", lwd=2)
abline(h=mu+2*sig.des_data, col="red", lwd=2,lty=2)
abline(h=mu-2*sig.des_data, col="red", lwd=2,lty=2)
grid()

par(mfrow = c(2,1))

acf(des_data, lag.max = 36, xaxt = "n", main = "ACF")
axis(1, at = 0:36/12, labels = 0:36)
pacf(des_data, lag.max = 36, xaxt = "n", main = "PACF")
axis(1, at = 0:36/12, labels = 0:36)

adf_test <- adf.test(des_data, alternative = "stationary")


if (adf_test$p.value > 0.05) {
  interpretation_texte <- paste(
    "- p-value ≥ 0.05 → On NE REJETTE PAS H0",
    "- ✗ La série n'est pas stationnaire.",
    sep = "\n"
  )
} else {
  interpretation_texte <- paste(
    "- p-value < 0.05 → On REJETTE H0",
    "- ✓ La série est stationnaire.",
    sep = "\n"
  )
}

texte_final <- paste(
  "INTERPRÉTATION DU TEST ADF :",
  "-----------------------------",
  "- H0 : la série n'est pas Stationnaire",
  interpretation_texte,
  sep = "\n"
)

print(adf_test)
cat(texte_final)

kpss_result <- kpss.test(des_data)

cat("\nTest KPSS sur la série brute :\n")
cat("-------------------------------\n")
cat("Statistique KPSS :", round(kpss_result$statistic, 4), "\n")
cat("p-value :", round(kpss_result$p.value, 4), "\n\n")

if (kpss_result$p.value < 0.05) {
  cat("✗ La série est NON-STATIONNAIRE (p < 0.05)\n")
} else {
  cat("✓ La série est STATIONNAIRE (p ≥ 0.05)\n")
}

resultats <- data.frame(type = character(),
                        odre = integer(),
                        AIC = numeric(),
                        BIC = numeric())

for (p in 1:5){
  fit <- tryCatch({
    arima(des_data, order = c(p,0,0), method="ML")
  }, error=function(e) NULL)
  
  if(!is.null(fit)){
    resultats <- rbind(resultats, data.frame(
      type = "AR",
      ordre = p,
      AIC = AIC(fit),
      BIC = BIC(fit)
    ))
  }
}

for (q in 1:5){
  fit <- tryCatch({
    arima(des_data, order = c(0,0,q), method="ML")
  }, error=function(e) NULL)
  
  if(!is.null(fit)){
    resultats <- rbind(resultats, data.frame(
      type = "MA",
      ordre = q,
      AIC = AIC(fit),
      BIC = BIC(fit)
    ))
  }
}

print(resultats)

meilleur_AIC <- resultats[which.min(resultats$AIC),]
meilleur_BIC <- resultats[which.min(resultats$BIC),]

meilleur_AIC_AR <- resultats[resultats$type == "AR", ][which.min(resultats[resultats$type == "AR", ]$AIC), ]
meilleur_BIC_AR <- resultats[resultats$type == "AR", ][which.min(resultats[resultats$type == "AR", ]$BIC), ]

meilleur_AIC_MA <- resultats[resultats$type == "MA", ][which.min(resultats[resultats$type == "MA", ]$AIC), ]
meilleur_BIC_MA <- resultats[resultats$type == "MA", ][which.min(resultats[resultats$type == "MA", ]$BIC), ]

cat("Meilleur modèle selon AIC:", meilleur_AIC$type, "(",meilleur_AIC$ordre, ")\n",
    "Meilleur modèle selon BIC:", meilleur_BIC$type, "(",meilleur_BIC$ordre, ")\n", 
    "Meilleur modèle AR selon AIC:", meilleur_AIC_AR$type, "(",meilleur_AIC_AR$ordre, ")\n",
    "Meilleur modèle AR selon BIC:", meilleur_BIC_AR$type, "(",meilleur_BIC_AR$ordre, ")\n",
    "Meilleur modèle MA selon AIC:", meilleur_AIC_MA$type, "(",meilleur_AIC_MA$ordre, ")\n",
    "Meilleur modèle MA selon BIC:", meilleur_BIC_MA$type, "(",meilleur_BIC_MA$ordre, ")\n"
)
resultats_ARMA <- data.frame(
  p = integer(),
  q = integer(),
  AIC = numeric(),
  BIC = numeric()
)

for (p in 0:5){
  for (q in 0:5){
    if (p==0 & q==0) next
    
    fit <- tryCatch({
      arima(des_data, order = c(p,0,q), method="ML")
    }, error = function(e) NULL)
    
    if (!is.null(fit)){
      resultats_ARMA <- rbind(resultats_ARMA, data.frame(
        p = p,
        q = q,
        AIC = AIC(fit),
        BIC = BIC(fit)
      ))
    }
  }
}


meilleur_AIC_ARMA <- resultats_ARMA[which.min(resultats_ARMA$AIC),]
meilleur_BIC_ARMA <- resultats_ARMA[which.min(resultats_ARMA$BIC),]

cat("\nMeilleur modèle ARMA selon AIC: ARMA(",meilleur_AIC_ARMA$p, "0",meilleur_AIC_ARMA$q, ")\n","Meilleur modèle ARMA selon BIC: ARMA(",meilleur_BIC_ARMA$p, "0",meilleur_BIC_ARMA$q, ")") 

resultats_ARMA$p <- as.factor(resultats_ARMA$p)
resultats_ARMA$q <- as.factor(resultats_ARMA$q)

heatmap_plot <- ggplot(resultats_ARMA, aes(x = q, y = p, fill = BIC)) +
  geom_tile(color = "white", lwd = 0.5) + 
  geom_text(aes(label = round(BIC, 1)), color = "black", size = 3.5) +
  scale_fill_viridis_c(direction = -1) + 
  labs(
    title = "Heatmap du BIC pour différents modèles ARMA(p,q)",
    x = "Ordre 'q' (Moyenne Mobile)",
    y = "Ordre 'p' (Autorégressif)",
    fill = "BIC"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12)
  )

# Afficher le graphique
print(heatmap_plot)

modele_AR <- arima(des_data, order = c(4,0,0), method="ML")
modele_ARMA <- arima(des_data, order = c(5,0,5), method="ML")
AIC_ARMA = AIC(modele_ARMA)
AIC_AR = AIC(modele_AR)
print(AIC_AR)
print(AIC_ARMA)

res_AR <- residuals(modele_AR)
res_ARMA <- residuals(modele_ARMA)

par(mfrow = c(2,1))

acf(res_AR, lag.max = 36, xaxt = "n", main = "ACF des résidus AR(4)")
axis(1, at = 0:36/12, labels = 0:36)
acf(res_ARMA, lag.max = 36, xaxt = "n", main = "ACF des résidus ARMA(5,0,5)")
axis(1, at = 0:36/12, labels = 0:36)

Box.test(res_AR, lag=12, type="Ljung-Box")
Box.test(res_ARMA, lag=12, type="Ljung-Box")

hist(res_AR, main = "Histogramme des résidus AR(4)", xlab = "Résidus")
hist(res_ARMA, main = "Histogramme des résidus ARMA(5,0,5)", xlab = "Résidus")

fixed_ar_36 <- c(NA, rep(0,10), NA, NA, rep(0,11),NA, rep(0,11), NA)  
modele_subsetAR_36 <- Arima(des_data, order=c(37,0,0), include.mean=FALSE, fixed=fixed_ar_36)
nb_param_AR_36<- coef(modele_subsetAR_36)


fixed_ar_12 <- c(NA, rep(0,10), NA)  # φ1 et φ12 libres
modele_subsetAR_12 <- Arima(des_data, order=c(12,0,0), include.mean=FALSE, fixed=fixed_ar_12)
nb_param_AR_12<- coef(modele_subsetAR_12)

fixed_arma <- c(NA, rep(0,10), NA,      # AR part : φ1 et φ12 libres
                rep(0,11), NA)          # MA part : θ12 libre
modele_subsetARMA <- Arima(des_data, order=c(12,0,12), include.mean=FALSE, fixed=fixed_arma)

res_subsetAR_12 <- residuals(modele_subsetAR_12)
res_subsetAR_36 <- residuals(modele_subsetAR_36)
res_subsetARMA <- residuals(modele_subsetARMA)

par(mfrow = c(2,1))

acf(res_subsetAR_12, lag.max = 36, xaxt = "n", main = "ACF des résidus AR(12) (subset{lags: 1, 12}")
axis(1, at = 0:36/12, labels = 0:36)
acf(res_subsetAR_36, lag.max = 36, xaxt = "n", main = "ACF des résidus AR(12) (subset{lags: 1, 11, 12, 24, 36}")
axis(1, at = 0:36/12, labels = 0:36)
acf(res_subsetARMA, lag.max = 36, xaxt = "n", main = "ACF des résidus ARMA(12,0,12) (subset {AR:1,12 ; MA:12)")
axis(1, at = 0:36/12, labels = 0:36)

Box.test(res_subsetAR_12, lag=12, type="Ljung-Box") # fitdf représente le nombre de paramètre estimé et permet dans ce cas d'ajuster le degré de liberté de la loi du qi 2 pour calculer la statistique de Ljung-Box.  
Box.test(res_subsetAR_36, lag=12, type="Ljung-Box")
Box.test(res_subsetAR_36, lag=25, type="Ljung-Box")
Box.test(res_subsetARMA, lag=12, type="Ljung-Box")
length(des_data)

fixed_ar_36 <- c(NA, rep(0,10), NA, NA, rep(0,11),NA, rep(0,11), NA)  
modele_subsetAR_36 <- Arima(des_data, order=c(37,0,0), include.mean=FALSE, fixed=fixed_ar_36)

fixed_ar_12 <- c(NA, rep(0,10), NA)
modele_subsetAR_12 <- Arima(des_data, order=c(12,0,0), include.mean=FALSE, fixed=fixed_ar_12)


fixed_arma <- c(NA, rep(0,10), NA,      # AR part : φ1 et φ12 libres
                rep(0,11), NA)          # MA part : θ12 libre
modele_subsetARMA <- Arima(des_data, order=c(12,0,12), include.mean=FALSE, fixed=fixed_arma)

# Calcul des résidus
res_subsetAR_12 <- residuals(modele_subsetAR_12)
res_subsetAR_36 <- residuals(modele_subsetAR_36)
res_subsetARMA <- residuals(modele_subsetARMA)

# Tests de Ljung-Box
lb_AR12_lag12 <- Box.test(res_subsetAR_12, lag=12, type="Ljung-Box")
lb_AR12_lag24 <- Box.test(res_subsetAR_12, lag=24, type="Ljung-Box")

lb_AR36_lag12 <- Box.test(res_subsetAR_36, lag=12, type="Ljung-Box")
lb_AR36_lag24 <- Box.test(res_subsetAR_36, lag=24, type="Ljung-Box")
lb_AR36_lag36 <- Box.test(res_subsetAR_36, lag=36, type="Ljung-Box")

lb_ARMA_lag12 <- Box.test(res_subsetARMA, lag=12, type="Ljung-Box")
lb_ARMA_lag24 <- Box.test(res_subsetARMA, lag=24, type="Ljung-Box")

# Graphiques ACF avec statistiques
par(mfrow = c(3,1), mar = c(4, 4, 4, 2))

# AR(12) subset
acf(res_subsetAR_12, lag.max = 36, xaxt = "n", 
    main = "ACF des résidus AR(12) subset {lags: 1, 12}")
axis(1, at = 0:36/12, labels = 0:36)


# AR(36) subset
acf(res_subsetAR_36, lag.max = 36, xaxt = "n", 
    main = "ACF des résidus AR(37) subset {lags: 1, 12, 13, 24, 36}")
axis(1, at = 0:36/12, labels = 0:36)


# ARMA subset
acf(res_subsetARMA, lag.max = 36, xaxt = "n", 
    main = "ACF des résidus ARMA(12,0,12) subset {AR:1,12 ; MA:12}")
axis(1, at = 0:36/12, labels = 0:36)

# Graphiques des lois du Chi² séparés par ddl
par(mfrow = c(3,2), mar = c(4, 4, 3, 2))

x <- seq(0, 50, length.out = 500)

# 1. Chi² avec ddl = 10 (AR12, lag=12)
plot(x, dchisq(x, df = 12), type = "l", lwd = 2, col = "blue",
     main = expression(paste(chi^2, "(ddl=10) - AR(12) lag=12")),
     ylab = "Densité", xlab = "x")
abline(v = lb_AR12_lag12$statistic, col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c("χ²(12)", 
                  paste0("Q observé = ", round(lb_AR12_lag12$statistic, 2)),
                  paste0("p-value = ", round(lb_AR12_lag12$p.value, 4))),
       col = c("blue", "red", NA), lwd = c(2, 2, NA), lty = c(1, 2, NA),
       cex = 0.8, bty = "n")

# 2. Chi² avec ddl = 22 (AR12, lag=24)
plot(x, dchisq(x, df = 24), type = "l", lwd = 2, col = "blue",
     main = expression(paste(chi^2, "(ddl=22) - AR(12) lag=24")),
     ylab = "Densité", xlab = "x")
abline(v = lb_AR12_lag24$statistic, col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c("χ²(22)", 
                  paste0("Q observé = ", round(lb_AR12_lag24$statistic, 2)),
                  paste0("p-value = ", round(lb_AR12_lag24$p.value, 4))),
       col = c("blue", "red", NA), lwd = c(2, 2, NA), lty = c(1, 2, NA),
       cex = 0.8, bty = "n")

# 3. Chi² avec ddl = 7 (AR36, lag=12)
plot(x, dchisq(x, df = 12), type = "l", lwd = 2, col = "darkgreen",
     main = expression(paste(chi^2, "(ddl=7) - AR(37) lag=12")),
     ylab = "Densité", xlab = "x")
abline(v = lb_AR36_lag12$statistic, col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c("χ²(7)", 
                  paste0("Q observé = ", round(lb_AR36_lag12$statistic, 2)),
                  paste0("p-value = ", round(lb_AR36_lag12$p.value, 4))),
       col = c("darkgreen", "red", NA), lwd = c(2, 2, NA), lty = c(1, 2, NA),
       cex = 0.8, bty = "n")

# 4. Chi² avec ddl = 19 (AR36, lag=24)
plot(x, dchisq(x, df = 12), type = "l", lwd = 2, col = "darkgreen",
     main = expression(paste(chi^2, "(ddl=19) - AR(37) lag=24")),
     ylab = "Densité", xlab = "x")
abline(v = lb_AR36_lag24$statistic, col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c("χ²(19)", 
                  paste0("Q observé = ", round(lb_AR36_lag24$statistic, 2)),
                  paste0("p-value = ", round(lb_AR36_lag24$p.value, 4))),
       col = c("darkgreen", "red", NA), lwd = c(2, 2, NA), lty = c(1, 2, NA),
       cex = 0.8, bty = "n")

# 5. Chi² avec ddl = 31 (AR36, lag=36)
plot(x, dchisq(x, df = 36), type = "l", lwd = 2, col = "darkgreen",
     main = expression(paste(chi^2, "(ddl=31) - AR(37) lag=36")),
     ylab = "Densité", xlab = "x")
abline(v = lb_AR36_lag36$statistic, col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c("χ²(31)", 
                  paste0("Q observé = ", round(lb_AR36_lag36$statistic, 2)),
                  paste0("p-value = ", round(lb_AR36_lag36$p.value, 4))),
       col = c("darkgreen", "red", NA), lwd = c(2, 2, NA), lty = c(1, 2, NA),
       cex = 0.8, bty = "n")

# 6. Chi² avec ddl = 9 (ARMA, lag=12)
plot(x, dchisq(x, df = 12), type = "l", lwd = 2, col = "purple",
     main = expression(paste(chi^2, "(ddl=9) - ARMA lag=12")),
     ylab = "Densité", xlab = "x")
abline(v = lb_ARMA_lag12$statistic, col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c("χ²(9)", 
                  paste0("Q observé = ", round(lb_ARMA_lag12$statistic, 2)),
                  paste0("p-value = ", round(lb_ARMA_lag12$p.value, 4))),
       col = c("purple", "red", NA), lwd = c(2, 2, NA), lty = c(1, 2, NA),
       cex = 0.8, bty = "n")

# Graphique pour ARMA lag=24
par(mfrow = c(1,1), mar = c(4, 4, 3, 2))
plot(x, dchisq(x, df = 24), type = "l", lwd = 2, col = "purple",
     main = expression(paste(chi^2, "(ddl=21) - ARMA lag=24")),
     ylab = "Densité", xlab = "x")
abline(v = lb_ARMA_lag24$statistic, col = "red", lty = 2, lwd = 2)
legend("topright", 
       legend = c("χ²(21)", 
                  paste0("Q observé = ", round(lb_ARMA_lag24$statistic, 2)),
                  paste0("p-value = ", round(lb_ARMA_lag24$p.value, 4))),
       col = c("purple", "red", NA), lwd = c(2, 2, NA), lty = c(1, 2, NA),
       cex = 0.8, bty = "n")

modele_AR_1 <- arima(des_data, order = c(1,0,0), method="ML")
modele_MA_1 <- arima(des_data, order = c(0,0,1), method="ML")
AIC_AR_1 = AIC(modele_AR_1)
AIC_MA_1 = AIC(modele_MA_1)

res_AR_1 <- residuals(modele_AR_1)
res_MA_1 <- residuals(modele_MA_1)

par(mfrow = c(2,1))

acf(res_AR_1, lag.max = 36, xaxt = "n", main = "ACF des résidus AR(1)")
axis(1, at = 0:36/12, labels = 0:36)
acf(res_MA_1, lag.max = 36, xaxt = "n", main = "ACF des résidus MA(1)")
axis(1, at = 0:36/12, labels = 0:36)

Box.test(res_AR_1, lag=12, type="Ljung-Box")
Box.test(res_MA_1, lag=12, type="Ljung-Box")

hist(res_AR_1, main = "Histogramme des résidus AR(1)", xlab = "Résidus")
hist(res_MA_1, main = "Histogramme des résidus MA(1)", xlab = "Résidus")

# Sélection automatique avec auto.arima
cat("Sélection automatique du meilleur modèle ARIMA...\n\n")

fit_auto <- auto.arima(des_data,
                       seasonal = TRUE,
                       stepwise = FALSE,
                       approximation = FALSE,
                       trace = TRUE,
                       ic = "aicc")

cat("\n========================================\n")
cat("MEILLEUR MODÈLE SÉLECTIONNÉ\n")
cat("========================================\n")
print(summary(fit_auto))

# Tester plusieurs modèles manuellement
models <- list(
  "ARIMA(1,1,1)(1,1,1)[12]" = Arima(des_data, order = c(1,0,1), seasonal = c(1,1,1)),
  "ARIMA(2,1,2)(1,1,1)[12]" = Arima(des_data, order = c(2,0,2), seasonal = c(1,1,1)),
  "ARIMA(1,1,2)(2,1,1)[12]" = Arima(des_data, order = c(1,0,2), seasonal = c(2,1,1)),
  "ARIMA(2,0,2)(2,0,0)[12]" = Arima(des_data, order = c(2,0,2), seasonal = c(2,0,0))
)

# Comparaison des critères
comparison <- data.frame(
  Modèle = names(models),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC),
  AICc = sapply(models, function(m) m$aicc),
  LogLik = sapply(models, logLik)
)

comparison <- rbind(
  data.frame(
    Modèle = "Auto ARIMA",
    AIC = AIC(fit_auto),
    BIC = BIC(fit_auto),
    AICc = fit_auto$aicc,
    LogLik = logLik(fit_auto)
  ),
  comparison
)


comparison <- comparison %>% arrange(AICc)

knitr::kable(comparison, 
             caption = "Comparaison des Modèles ARIMA",
             digits = 2)

cat("\n✓ Le meilleur modèle selon AICc est :", comparison$Modèle[1], "\n")

checkresiduals(fit_auto)

residus <- residuals(fit_auto)
ljung_box <- Box.test(residus, lag = 12, type = "Ljung-Box", fitdf = 5)
cat("Test de Ljung-Box (H0 : pas d'autocorrélation) :\n")
cat("  p-value =", round(ljung_box$p.value, 4), "\n")
if (ljung_box$p.value > 0.05) {
  cat("  ✓ Les résidus sont un bruit blanc (p > 0.05)\n\n")
} else {
  cat("  ✗ Autocorrélation résiduelle détectée (p < 0.05)\n\n")
}

# Test de Shapiro-Wilk (normalité)
shapiro_test <- shapiro.test(residus)
cat("Test de Shapiro-Wilk (H0 : normalité) :\n")
cat("  p-value =", round(shapiro_test$p.value, 4), "\n")
if (shapiro_test$p.value > 0.05) {
  cat("  ✓ Les résidus suivent une loi normale (p > 0.05)\n\n")
} else {
  cat("  ✗ Les résidus ne suivent pas une loi normale (p < 0.05)\n\n")
}

# Analyse détaillée des résidus
par(mfrow = c(2, 3))

# 1. Résidus dans le temps
plot(residus, main = "Résidus du Modèle", ylab = "Résidus", col = "steelblue", type = "l")
abline(h = 0, col = "red", lty = 2, lwd = 2)

acf(residus, lag.max = 36, xaxt = "n", main = "ACF des résidus auto.arima")
axis(1, at = 0:36/12, labels = 0:36)
pacf(residus, lag.max = 36, xaxt = "n", main = "ACF des résidus auto.arima")
axis(1, at = 0:36/12, labels = 0:36)

# 4. Histogramme
hist(residus, breaks = 30, main = "Distribution des Résidus",
     xlab = "Résidus", col = "lightblue", border = "white", probability = TRUE)
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), add = TRUE, col = "red", lwd = 2)

# 5. QQ-plot
qqnorm(residus, main = "QQ-Plot des Résidus")
qqline(residus, col = "red", lwd = 2)

# 6. Résidus carrés 
plot(residus^2, main = "Résidus au Carré", ylab = "Résidus²", col = "purple", type = "h")
abline(h = mean(residus^2), col = "red", lty = 2)

# Test ARCH d’Engle
ArchTest(residus, lags = 12)
ArchTest(res_subsetARMA, lags=12)

spec_subsetARMA_ARCH <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
  mean.model     = list(armaOrder = c(12,12), include.mean = TRUE),
  distribution.model = "norm",
  fixed.pars = list(
    # AR coefficients forcés à 0 sauf φ1 et φ12
    ar2=0, ar3=0, ar4=0, ar5=0, ar6=0, ar7=0, ar8=0, ar9=0, ar10=0, ar11=0,
    # MA coefficients forcés à 0 sauf θ12
    ma1=0, ma2=0, ma3=0, ma4=0, ma5=0, ma6=0, ma7=0, ma8=0, ma9=0, ma10=0, ma11=0
  )
)

spec_subsetARMA_GARCH <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(12,12), include.mean = TRUE),
  distribution.model = "norm",
  fixed.pars = list(
    ar2=0, ar3=0, ar4=0, ar5=0, ar6=0, ar7=0, ar8=0, ar9=0, ar10=0, ar11=0,
    ma1=0, ma2=0, ma3=0, ma4=0, ma5=0, ma6=0, ma7=0, ma8=0, ma9=0, ma10=0, ma11=0
  )
)

modele_subsetARMA_ARCH <- ugarchfit(spec = spec_subsetARMA_ARCH, data = des_data)
modele_subsetARMA_GARCH <- ugarchfit(spec = spec_subsetARMA_GARCH, data = des_data)

infocriteria(modele_subsetARMA_ARCH)
infocriteria(modele_subsetARMA_GARCH)

z_subsetARMA_GARCH <- residuals(modele_subsetARMA_GARCH, standardize = TRUE)

z_subsetARMA_ARCH <- residuals(modele_subsetARMA_ARCH, standardize = TRUE)

acf(z_subsetARMA_GARCH, lag.max = 36, xaxt = "n", main ="ACF des résidus standardisés ARMA(12,12) GARCH(1,1) (subset {AR:1,12 ; MA:12)")
axis(1, at = 0:36/12, labels = 0:36)
acf(z_subsetARMA_GARCH^2, lag.max = 36, xaxt = "n", main="ACF des résidus standardisés au carré ARMA(12,12) GARCH(1,1) (subset {AR:1,12 ; MA:12)")
axis(1, at = 0:36/12, labels = 0:36)

acf(z_subsetARMA_ARCH, lag.max = 36, xaxt = "n", main ="ACF des résidus standardisés ARMA(12,12) ARCH(1) (subset {AR:1,12 ; MA:12)")
axis(1, at = 0:36/12, labels = 0:36)
acf(z_subsetARMA_ARCH^2, lag.max = 36, xaxt = "n", main="ACF des résidus standardisés au carré ARMA(12,12) ARCH(1) (subset {AR:1,12 ; MA:12)")
axis(1, at = 0:36/12, labels = 0:36)

Box.test(z_subsetARMA_GARCH,  lag=20, type="Ljung-Box")
Box.test(z_subsetARMA_GARCH^2,lag=20, type="Ljung-Box")  

Box.test(z_subsetARMA_ARCH,  lag=20, type="Ljung-Box")
Box.test(z_subsetARMA_ARCH^2,lag=20, type="Ljung-Box")

horizon <- 12
n <- length(des_data)

train <- window(des_data, end = time(des_data)[n-horizon])
test <- window(des_data, start = time(des_data)[n-horizon+1])

modele_prev_subsetARMA_ARCH <- ugarchfit(spec = spec_subsetARMA_ARCH, data = train)
prev <- ugarchforecast(modele_prev_subsetARMA_ARCH, n.ahead=horizon)

alpha <- 0.05
z <- qnorm(1-alpha/2)

prev_moy <- as.numeric(fitted(prev))
prev_sigma <- as.numeric(sigma(prev))
ic_sup <- prev_moy + z*prev_sigma
ic_inf <- prev_moy - z*prev_sigma

start_prev <- tsp(train)[2] + 1/frequency(train)
prev_ts <- ts(prev_moy, frequency=12,
              start = start_prev)
ic_sup_ts <- ts(ic_sup, frequency=12, start = start_prev)
ic_inf_ts <- ts(ic_inf, frequency=12, start = start_prev)

ts.plot(des_data, col="black", main="Prévisions ARMA(12,0,12) subset {AR:1,12 ; MA:12}-ARCH(1) avec IC à 95%")
lines(prev_ts, col="blue", lwd=2)
lines(ic_inf_ts, col="red", lty=2)
lines(ic_sup_ts, col="red", lty=2)


des_data_zoom <- window(des_data, start = c(2020, 1))

ylim_vals_subsetARMA_ARCH <- range(des_data_zoom,
                                   prev_ts,
                                   ic_inf_ts,
                                   ic_sup_ts,
                                   na.rm=TRUE)

ts.plot(des_data_zoom, col="black", 
        main="Prévisions ARMA(12,0,12) subset {AR:1,12 ; MA:12}-ARCH(1) avec IC à 95%", ylim=ylim_vals_subsetARMA_ARCH)
lines(prev_ts, col="blue")
lines(ic_inf_ts, col="red", lty=2)
lines(ic_sup_ts, col="red", lty=2)

mse <- mse(as.numeric(test), prev_moy)
mape <- mape(as.numeric(test), prev_moy)

cat("MSE : ", mse, "\n",
    "MAPE : ", mape)

prev_subsetARMA <- forecast(modele_subsetARMA, h=horizon, level=95)

prev_moy_subsetARMA <- as.numeric(prev_subsetARMA$mean)
ic_inf_subsetARMA <- as.numeric(prev_subsetARMA$lower)
ic_sup_subsetARMA <- as.numeric(prev_subsetARMA$upper)

prev_ts_subsetARMA <- ts(prev_moy_subsetARMA, frequency=12,
                         start = start_prev)
ic_sup_ts_subsetARMA <- ts(ic_sup_subsetARMA, frequency=12, start = start_prev)
ic_inf_ts_subsetARMA <- ts(ic_inf_subsetARMA, frequency=12, start = start_prev)

ts.plot(des_data, col="black", main="Prévisions ARMA(5,5) avec IC à 95%")
lines(prev_ts_subsetARMA, col="blue", lwd=2)
lines(ic_inf_ts_subsetARMA, col="red", lty=2)
lines(ic_sup_ts_subsetARMA, col="red", lty=2)

ylim_vals_subsetARMA <- range(des_data_zoom,
                              prev_ts_subsetARMA,
                              ic_inf_ts_subsetARMA,
                              ic_sup_ts_subsetARMA,
                              na.rm=TRUE)

ts.plot(des_data_zoom, col="black", 
        main="Prévisions ARMA(5,5) avec IC à 95%", ylim=ylim_vals_subsetARMA)
lines(prev_ts_subsetARMA, col="blue")
lines(ic_inf_ts_subsetARMA, col="red", lty=2)
lines(ic_sup_ts_subsetARMA, col="red", lty=2)

mse_subsetARMA <- mse(as.numeric(test), prev_moy_subsetARMA)
mape_subsetARMA <- mape(as.numeric(test), prev_moy_subsetARMA)

cat("MSE : ", mse_subsetARMA, "\n",
    "MAPE : ", mape_subsetARMA)

# Prévision avec auto.arima

modele_prev_SARIMA <- arima(train, 
                            order = c(1,0,1), 
                            seasonal = list(order = c(2,0,1), period =12), 
                            method="CSS-ML")
prev_SARIMA <- forecast(modele_prev_SARIMA, h=horizon, level=95)

prev_moy_SARIMA <- as.numeric(prev_SARIMA$mean)
ic_inf_SARIMA <- as.numeric(prev_SARIMA$lower)
ic_sup_SARIMA <- as.numeric(prev_SARIMA$upper)

prev_ts_SARIMA <- ts(prev_moy_SARIMA, frequency=12,
                     start = start_prev)
ic_sup_ts_SARIMA <- ts(ic_sup_SARIMA, frequency=12, start = start_prev)
ic_inf_ts_SARIMA <- ts(ic_inf_SARIMA, frequency=12, start = start_prev)

ts.plot(des_data, col="black", main="Prévisions ARIMA(1,0,1)(2,0,1)[12] avec IC à 95%")
lines(prev_ts_SARIMA, col="blue", lwd=2)
lines(ic_inf_ts_SARIMA, col="red", lty=2)
lines(ic_sup_ts_SARIMA, col="red", lty=2)

ylim_vals_SARIMA <- range(des_data_zoom,
                          prev_ts_SARIMA,
                          ic_inf_ts_SARIMA,
                          ic_sup_ts_SARIMA,
                          na.rm=TRUE)

ts.plot(des_data_zoom, col="black", 
        main="Prévisions ARIMA(1,0,1)(2,0,1)[12] avec IC à 95%", ylim=ylim_vals_SARIMA)
lines(prev_ts_SARIMA, col="blue")
lines(ic_inf_ts_SARIMA, col="red", lty=2)
lines(ic_sup_ts_SARIMA, col="red", lty=2)

mse_SARIMA <- mse(as.numeric(test), prev_moy_SARIMA)
mape_SARIMA <- mape(as.numeric(test), prev_moy_SARIMA)

cat("MSE : ", mse_SARIMA, "\n",
    "MAPE : ", mape_SARIMA)


performances <- data.frame(
  Model = c("subset ARMA-ARCH","subset ARMA","SARIMA"),
  MSE   = c(mse,
            mse_subsetARMA,
            mse_SARIMA),
  MAPE   = c(mape,
             mape_subsetARMA,
             mape_SARIMA)
)
print(performances)

## Bonus

packages <- c("quantmod", "tseries", "forecast", "rugarch", "FinTS", 
              "dplyr", "tidyr", "lubridate", "ggplot2", "gridExtra", 
              "moments", "psych", "knitr", "kableExtra")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

ticker <- "EURUSD=X"
getSymbols(ticker, src = "yahoo", from = "2022-01-01", to = "2024-12-31", auto.assign = TRUE)

close_prices <- Cl(get(ticker))
log_returns <- diff(log(close_prices))
log_returns <- na.omit(log_returns)

stats_desc <- data.frame(
  Statistique = c("Nombre d'observations", "Moyenne", "Écart-type", "Minimum", 
                  "Maximum", "Skewness", "Kurtosis"),
  Valeur = c(
    length(log_returns),
    round(mean(log_returns), 6),
    round(sd(log_returns), 6),
    round(min(log_returns), 6),
    round(max(log_returns), 6),
    round(moments::skewness(as.numeric(log_returns)), 4),
    round(moments::kurtosis(as.numeric(log_returns)), 4)
  )
)

kable(stats_desc, caption = "Statistiques Descriptives des Rendements Logarithmiques") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(close_prices, main = "EUR/USD - Prix de Clôture", ylab = "Taux de Change", col = "darkblue", lwd = 1.5)
plot(log_returns, main = "Rendements Logarithmiques", ylab = "Rendements", col = "steelblue", type = "h")
plot(log_returns^2, main = "Rendements² (Proxy Volatilité)", ylab = "Rendements²", col = "red3", type = "h")
hist(log_returns, breaks = 50, main = "Distribution des Rendements", 
     xlab = "Rendements", col = "lightblue", border = "white", probability = TRUE)
lines(density(log_returns), col = "red", lwd = 2)

adf_result <- adf.test(log_returns)
kpss_result <- kpss.test(log_returns)

tests_stationnarite <- data.frame(
  Test = c("Augmented Dickey-Fuller", "KPSS"),
  Statistique = c(round(adf_result$statistic, 4), round(kpss_result$statistic, 4)),
  `P-value` = c(round(adf_result$p.value, 4), round(kpss_result$p.value, 4)),
  Conclusion = c(
    ifelse(adf_result$p.value < 0.05, "✓ Stationnaire", "✗ Non-stationnaire"),
    ifelse(kpss_result$p.value >= 0.05, "✓ Stationnaire", "✗ Non-stationnaire")
  )
)

kable(tests_stationnarite, caption = "Tests de Stationnarité") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# Initialisation
serie_courante <- log_returns
max_iterations <- 50
convergence <- FALSE
iteration <- 0

# Stockage des résultats
historique_iterations <- data.frame()
modeles_estimes <- list()

cat("\n## Journal des Itérations\n\n")
cat("**Principe** : À chaque itération, on teste si les résidus nécessitent un nouveau cycle ARIMA-GARCH.\n\n")
cat("---\n\n")

while (iteration < max_iterations && !convergence) {
  iteration <- iteration + 1
  
  cat("### ITÉRATION ", iteration, "\n\n")
  
  # ==========================================
  # ÉTAPE 1 : ESTIMATION ARIMA
  # ==========================================
  
  cat("#### Étape 1.", iteration, ".A - Estimation ARIMA\n\n")
  
  fit_arima <- auto.arima(serie_courante,
                          seasonal = FALSE,
                          stepwise = FALSE,
                          approximation = FALSE,
                          ic = "aicc",
                          trace = FALSE)
  
  arima_order <- arimaorder(fit_arima)
  cat("- Modèle sélectionné : **ARIMA(", arima_order[1], ",", arima_order[2], ",", arima_order[3], ")**\n")
  cat("- AICc = ", round(fit_arima$aicc, 2), "\n\n")
  
  residus_arima <- residuals(fit_arima)
  
  # ==========================================
  # ÉTAPE 2 : TESTS SUR RÉSIDUS ARIMA
  # ==========================================
  
  cat("#### Étape 1.", iteration, ".B - Tests sur résidus ARIMA\n\n")
  
  lb_residus <- Box.test(residus_arima, type = "Ljung-Box", lag = 40)
  lb_residus2 <- Box.test(residus_arima^2, type = "Ljung-Box", lag = 40)
  arch_test <- ArchTest(residus_arima, lags = 10)
  
  tests_iter <- data.frame(
    Test = c("Ljung-Box (résidus)", "Ljung-Box (résidus²)", "ARCH"),
    `P-value` = c(round(lb_residus$p.value, 4), round(lb_residus2$p.value, 4), 
                  round(arch_test$p.value, 4)),
    Décision = c(
      ifelse(lb_residus$p.value > 0.05, "✓ OK", "✗ Autocorrélation"),
      ifelse(lb_residus2$p.value > 0.05, "✓ OK", "✗ Hétéroscédasticité"),
      ifelse(arch_test$p.value > 0.05, "✓ OK", "✗ Effet ARCH")
    )
  )
  
  print(kable(tests_iter, caption = paste("Tests Itération", iteration)) %>%
          kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE))
  
  cat("\n")
  
  # ==========================================
  # DÉCISION : GARCH NÉCESSAIRE ?
  # ==========================================
  
  if (arch_test$p.value < 0.05) {
    cat("**Décision** : Effet ARCH détecté (p = ", round(arch_test$p.value, 4), ") → **Estimation GARCH**\n\n")
    
    # ==========================================
    # ÉTAPE 3 : ESTIMATION GARCH
    # ==========================================
    
    cat("#### Étape 1.", iteration, ".C - Comparaison modèles GARCH\n\n")
    
    models_list <- list(
      list(name = "GARCH(1,1)", model = "sGARCH", order = c(1,1)),
      list(name = "GARCH(1,2)", model = "sGARCH", order = c(1,2)),
      list(name = "GARCH(2,1)", model = "sGARCH", order = c(2,1)),
      list(name = "TGARCH(1,1)", model = "fGARCH", submodel = "TGARCH", order = c(1,1)),
      list(name = "EGARCH(1,1)", model = "eGARCH", order = c(1,1)),
      list(name = "GJR-GARCH(1,1)", model = "gjrGARCH", order = c(1,1))
    )
    
    results_garch <- data.frame()
    fitted_models <- list()
    
    for (i in seq_along(models_list)) {
      model_spec <- models_list[[i]]
      
      tryCatch({
        if (model_spec$model == "fGARCH") {
          spec <- ugarchspec(
            variance.model = list(model = model_spec$model, 
                                  submodel = model_spec$submodel,
                                  garchOrder = model_spec$order),
            mean.model = list(armaOrder = c(arima_order[1], arima_order[3]), 
                              include.mean = TRUE),
            distribution.model = "norm"
          )
        } else {
          spec <- ugarchspec(
            variance.model = list(model = model_spec$model, 
                                  garchOrder = model_spec$order),
            mean.model = list(armaOrder = c(arima_order[1], arima_order[3]), 
                              include.mean = TRUE),
            distribution.model = "norm"
          )
        }
        
        fit <- ugarchfit(spec = spec, data = serie_courante, solver = "hybrid")
        
        fitted_models[[model_spec$name]] <- fit
        
        results_garch <- rbind(results_garch, data.frame(
          Modèle = model_spec$name,
          AIC = round(infocriteria(fit)[1], 4),
          BIC = round(infocriteria(fit)[2], 4),
          Convergence = ifelse(convergence(fit) == 0, "✓", "✗")
        ))
      }, error = function(e) {
        results_garch <<- rbind(results_garch, data.frame(
          Modèle = model_spec$name,
          AIC = NA,
          BIC = NA,
          Convergence = "Erreur"
        ))
      })
    }
    
    results_garch <- results_garch[order(results_garch$AIC, na.last = TRUE), ]
    
    print(kable(results_garch, caption = paste("Comparaison GARCH - Itération", iteration)) %>%
            kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE))
    
    cat("\n")
    
    best_model_name <- results_garch$Modèle[1]
    best_fit <- fitted_models[[best_model_name]]
    
    cat("- **Meilleur modèle** : ", best_model_name, " (AIC = ", round(results_garch$AIC[1], 2), ")\n\n")
    
    # ==========================================
    # ÉTAPE 4 : VALIDATION MODÈLE GARCH
    # ==========================================
    
    cat("#### Étape 1.", iteration, ".D - Validation modèle complet\n\n")
    
    residus_std_garch <- residuals(best_fit, standardize = TRUE)
    
    lb_garch <- Box.test(residus_std_garch, type = "Ljung-Box", lag = 40)
    lb_garch2 <- Box.test(residus_std_garch^2, type = "Ljung-Box", lag = 40)
    arch_garch <- ArchTest(residus_std_garch, lags = 10)
    
    tests_garch_iter <- data.frame(
      Test = c("Ljung-Box (résidus std.)", "Ljung-Box (résidus² std.)", "ARCH (résidus std.)"),
      `P-value` = c(round(lb_garch$p.value, 4), round(lb_garch2$p.value, 4), 
                    round(arch_garch$p.value, 4)),
      Décision = c(
        ifelse(lb_garch$p.value > 0.05, "✓ OK", "✗ Autocorrélation"),
        ifelse(lb_garch2$p.value > 0.05, "✓ OK", "✗ Hétéroscédasticité"),
        ifelse(arch_garch$p.value > 0.05, "✓ OK", "✗ Effet ARCH")
      )
    )
    
    print(kable(tests_garch_iter, caption = paste("Validation GARCH - Itération", iteration)) %>%
            kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE))
    
    cat("\n")
    
    # Enregistrement
    historique_iterations <- rbind(historique_iterations, data.frame(
      Iteration = iteration,
      Modele_ARIMA = paste0("ARIMA(", arima_order[1], ",", arima_order[2], ",", arima_order[3], ")"),
      Modele_GARCH = best_model_name,
      ARCH_pvalue = round(arch_garch$p.value, 4),
      LB_residus2_pvalue = round(lb_garch2$p.value, 4),
      Convergence_atteinte = ifelse(arch_garch$p.value > 0.05 && lb_garch2$p.value > 0.05, "OUI", "NON")
    ))
    
    modeles_estimes[[paste0("Iter_", iteration)]] <- list(
      arima = fit_arima,
      garch = best_fit,
      residus = residus_std_garch
    )
    
    # ==========================================
    # DÉCISION DE CONVERGENCE
    # ==========================================
    
    if (arch_garch$p.value > 0.05 && lb_garch2$p.value > 0.05) {
      cat("**CONVERGENCE ATTEINTE** : Plus d'effet ARCH résiduel !\n\n")
      cat("Modèle final : **ARIMA(", arima_order[1], ",", arima_order[2], ",", arima_order[3], 
          ") + ", best_model_name, "**\n\n")
      convergence <- TRUE
    } else {
      cat("Effets résiduels détectés → **Itération suivante nécessaire**\n\n")
      cat("Les résidus standardisés deviennent la nouvelle série à modéliser\n\n")
      serie_courante <- residus_std_garch
    }
    
  } else {
    cat("**Décision** : Pas d'effet ARCH (p = ", round(arch_test$p.value, 4), ") → **ARIMA seul suffit**\n\n")
    
    historique_iterations <- rbind(historique_iterations, data.frame(
      Iteration = iteration,
      Modele_ARIMA = paste0("ARIMA(", arima_order[1], ",", arima_order[2], ",", arima_order[3], ")"),
      Modele_GARCH = "Aucun",
      ARCH_pvalue = round(arch_test$p.value, 4),
      LB_residus2_pvalue = round(lb_residus2$p.value, 4),
      Convergence_atteinte = "OUI"
    ))
    
    modeles_estimes[[paste0("Iter_", iteration)]] <- list(
      arima = fit_arima,
      garch = NULL,
      residus = residus_arima
    )
    
    cat("**CONVERGENCE ATTEINTE** : Modèle ARIMA seul est adéquat !\n\n")
    cat("Modèle final : **ARIMA(", arima_order[1], ",", arima_order[2], ",", arima_order[3], ")**\n\n")
    convergence <- TRUE
  }
  
  cat("---\n\n")
}

# ==========================================
# SYNTHÈSE FINALE
# ==========================================

cat("# 4. Synthèse du Processus Itératif\n\n")

if (convergence) {
  cat("**Convergence atteinte après ", iteration, " itération(s)**\n\n")
} else {
  cat("**Maximum d'itérations atteint (50) sans convergence complète**\n\n")
}

print(kable(historique_iterations, caption = "Historique Complet des Itérations") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
        column_spec(6, bold = TRUE, color = ifelse(historique_iterations$Convergence_atteinte == "OUI", "green", "red")))

cat("\n")
modele_final <- modeles_estimes[[paste0("Iter_", iteration)]]

cat("## 5.1 Spécification du Modèle Final\n\n")

if (!is.null(modele_final$garch)) {
  cat("**Modèle complet retenu** : ARIMA-GARCH combiné\n\n")
  cat("```\n")
  print(modele_final$garch)
  cat("```\n\n")
} else {
  cat("**Modèle retenu** : ARIMA seul\n\n")
  cat("```\n")
  print(summary(modele_final$arima))
  cat("```\n\n")
}

cat("## 5.2 Analyse Graphique des Résidus Finaux\n\n")

residus_finaux <- modele_final$residus

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

# Série temporelle
plot(residus_finaux, main = "Résidus Finaux Standardisés", ylab = "Résidus", 
     col = "darkgreen", type = "h")
abline(h = 0, col = "red", lty = 2)

# Histogramme
hist(residus_finaux, breaks = 50, main = "Distribution des Résidus", 
     xlab = "Résidus", col = "lightgreen", border = "white", probability = TRUE)
curve(dnorm(x, mean = mean(residus_finaux), sd = sd(residus_finaux)), 
      add = TRUE, col = "red", lwd = 2)

# QQ-plot
qqnorm(residus_finaux, main = "QQ-Plot")
qqline(residus_finaux, col = "red", lwd = 2)

# ACF
acf(residus_finaux, main = "ACF des Résidus", lag.max = 30)

# ACF résidus²
acf(residus_finaux^2, main = "ACF des Résidus²", lag.max = 30, col = "purple")

# Volatilité (si GARCH)
if (!is.null(modele_final$garch)) {
  sigma_finale <- sigma(modele_final$garch)
  plot(sigma_finale, main = "Volatilité Conditionnelle Finale", 
       ylab = "σ(t)", col = "red3", lwd = 1.5)
} else {
  plot(residus_finaux^2, main = "Carrés des Résidus", 
       ylab = "Résidus²", col = "purple", type = "h")
}

cat("\n## 5.3 Tests de Diagnostic Finaux\n\n")

lb_final <- Box.test(residus_finaux, type = "Ljung-Box", lag = 40)
lb_final2 <- Box.test(residus_finaux^2, type = "Ljung-Box", lag = 40)
arch_final <- ArchTest(residus_finaux, lags = 10)

tests_finaux <- data.frame(
  Test = c("Ljung-Box (résidus)", "Ljung-Box (résidus²)", "ARCH"),
  Statistique = c(round(lb_final$statistic, 4), round(lb_final2$statistic, 4), 
                  round(arch_final$statistic, 4)),
  `P-value` = c(round(lb_final$p.value, 4), round(lb_final2$p.value, 4), 
                round(arch_final$p.value, 4)),
  Interprétation = c(
    ifelse(lb_final$p.value > 0.05, "✓ Résidus = bruit blanc", "✗ Autocorrélation résiduelle"),
    ifelse(lb_final2$p.value > 0.05, "✓ Pas d'hétéroscédasticité", "✗ Hétéroscédasticité résiduelle"),
    ifelse(arch_final$p.value > 0.05, "✓ Pas d'effet ARCH", "✗ Effet ARCH résiduel")
  )
)

print(kable(tests_finaux, caption = "Tests de Diagnostic sur le Modèle Final") %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE))

cat("\n")




