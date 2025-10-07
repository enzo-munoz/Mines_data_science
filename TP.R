## Importation des données et analyse préliminaire

#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("moments")


library(dplyr)
library(lubridate)
library(moments)
library (tseries)

data <- read.csv("C:\\Users\\hugom\\OneDrive - Aescra Emlyon Business School\\Mines de Saint-Etienne\\3A\\Science des données\\Série temporelle\\TP évalué\\data.csv")

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

class(temperature)
plot(temperature,
     main = " Température moyenne mensuelle à Lyon",
     xlab = "Année",
     ylab = "Température (°C)",
     col = "blue",
     lwd = 2)
grid()

moyenne <- mean(temperature, na.rm = TRUE)
mediane <- median(temperature, na.rm = TRUE)
variance <- var(temperature, , na.rm = TRUE)
asymetrie <- skewness(temperature, na.rm = TRUE)
aplatissement <- kurtosis(temperature, na.rm = TRUE)


cat ("Statistiques descriptives de la série temporelle :\n",
     "Moyenne    :", moyenne, " \n",
     "Médiane    :", mediane," \n",
     "Variance   :", variance, " \n",
     "Skewness   :", asymetrie, " \n",
     "Kurtosis   :", aplatissement, " \n")

#La série temporelle présente une moyenne d’environ 13,2°C et une médiane proche (12,6°C), ce qui suggère une distribution globalement symétrique. 
#La variance (47,2) indique une dispersion modérée des valeurs autour de la moyenne, avec un écart-type d’environ 7°C. 
#Le skewness proche de zéro (0,04) confirme l’absence d’asymétrie marquée, 
#tandis que le kurtosis inférieur à 3 (1,79) traduit une distribution plus aplatie que la normale, avec moins de valeurs extrêmes.

#La série présente une forte saisonnalité annuelle (été chaud, hiver froid), avec une amplitude assez stable (~20°C). 
#On peut suspecter une tendance légèrement haussière des températures maximales récentes, mais ce n’est pas flagrant visuellement. 
#La volatilité reste globalement constante, bien que ponctuellement certains étés ou hivers soient plus extrêmes.

des_data <- diff(temperature, lag = 12)

plot(des_data,
     main = "Série désaisonnalisée par différenciation (lag 12)",
     xlab = "Année",
     ylab = "Variation de température (°C)")
grid()

adf_test <- adf.test(des_data, alternative = "stationary")
kpss_test <- kpss.test(des_data)

cat("Test ADF : \n")
print(adf_test)
cat("Test KPSS : \n")
print(kpss_test) 


## Modélisation et analyse












