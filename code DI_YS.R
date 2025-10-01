
library(tidyverse)
library(tsDyn)
library(lubridate)
library(quantmod)
library(tseries)
library(moments)
library(rugarch)
library(FinTS)
library(readxl)
library(zoo)
library(forecast)  
library(lmtest) 

#import des données: 
setwd("C:\\Users\\YASMINE\\Desktop\\setar")

df<- read.csv("Nikkei225_Final.csv",sep=",")
df$Date <- as.Date(df$Date)

# Calcul du BASIS = Futures - Spot
df <- df %>%
  mutate(Basis = Last_future - Spot_Last)


## Calcul du Basis normalisé: 
df <- df %>%
  mutate(Basis_norm = (Last_future - Spot_Last) / Spot_Last)


#graph du basis et spot:
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Spot_Last, colour = "Spot_Last")) +
  geom_line(aes(y = Basis, colour = "Basis")) +
  labs(
    title = "Nikkei 225 Spot vs Basis (Daily Nearby Futures)",
    y = "Valeurs",
    colour = "Série"
  ) +
  theme_minimal()

#graph du basis normalisé: 
ggplot(df, aes(x = Date, y = Basis_norm)) +
  geom_line(color = "blue", size = 0.4) +
  labs(
    title = "Évolution du basis entre 2000 et 2025",
    x = "Date",
    y = "Basis normalisé ( (F − S) / S )"
  ) +
  theme_minimal()


#stats descriptives de la variable basis_normalisé: 
x <- df$Basis_norm

desc <- tibble(
  N      = sum(!is.na(x)),
  Mean   = mean(x, na.rm = TRUE),
  Median = median(x, na.rm = TRUE),
  Sd     = sd(x, na.rm = TRUE),
  Min    = min(x, na.rm = TRUE),
  Q1     = quantile(x, 0.25, na.rm = TRUE),
  Q3     = quantile(x, 0.75, na.rm = TRUE),
  Max    = max(x, na.rm = TRUE),
  Skew   = skewness(x, na.rm = TRUE),
  Kurt   = kurtosis(x, na.rm = TRUE)
)
print(desc)

#Test de stationnarité ADF: 

adf_result <- adf.test(df$Basis_norm, alternative = "stationary")
print(adf_result)

# ACF et PACF

acf(df$Basis_norm, main = "ACF du Basis normalisé")
pacf(df$Basis_norm, main = "PACF du Basis normalisé")

# Test Ljung-Box d'autocorrélation:
ljung <- Box.test(df$Basis_norm, lag = 20, type = "Ljung-Box")
print(ljung)

#estimation des modèles AR lag 1 à 4: 
x <- df %>%
  arrange(Date) %>%
  pull(Basis_norm) %>%
  na.omit() %>%
  ts(frequency = 1)

# Estimation AR(1) ... AR(4)
fits <- lapply(1:4, function(p) Arima(x, order = c(p,0,0)))
names(fits) <- paste0("AR(", 1:4, ")")

# Tableau AIC / BIC
tab <- tibble(
  model = names(fits),
  p     = 1:4,
  AIC   = sapply(fits, AIC),
  BIC   = sapply(fits, BIC)
) 

print(tab)

# Comparaison des AIC et BIC de chaque modèle AR(1) à AR(4):

best_idx <- which.min(sapply(fits, BIC))
best_fit <- fits[[best_idx]]
cat("\nMeilleur modèle selon BIC :", names(fits)[best_idx], "\n")
print(summary(best_fit))
summary(best_fit)

#Test des résidus:

e <- residuals(best_fit)
cat("\nLjung-Box (lag=20) sur résidus :\n")
print(Box.test(e, lag = 20, type = "Ljung-Box"))
cat("\nARCH test (lags=12) sur résidus :\n")
print(FinTS::ArchTest(e, lags = 12))
checkresiduals(best_fit)


##test de non linéarité de Hansen: 

m_order <- 4
set.seed(123)
tst <- setarTest(
  x,
  m       = m_order,     
  thDelay = 1,           
  trim    = 0.1,         
  nboot   = 20,         
  test    = "1vs"        
)
tst



