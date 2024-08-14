setwd("C:\\provaAB1")

cars <- read.csv("CarsAb1.csv", header = TRUE, sep = ";", dec = ",")

library(dplyr)

# 1.

price <- cars$Price

# Funcao para calcular as modas

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

# Calculando media, moda, median e percentis

mean <- mean(price)
mean

median <- median(price)
median

modes <- Modes(price)
modes

percentiles <- quantile(price, probs = c(0.25, 0.75, 0.80, 0.95))
pecentiles

# gerando histograma

amplitude <- diff(range(price))

Nclasses <- nclass.Sturges(price)

Ampclasses <- round(amplitude/Nclasses)

limiteclas <- seq(min(price), max(price), Ampclasses)

Freq <- table(cut(price, breaks = limiteclas, include.lowest = TRUE))

#Plotando histograma
hist(
  price,
  ylim = c(0, max(Freq) + 5),
  xlim = c(min(price) - 5, max(price) + 5),
  ylab = "Absolute Frequency", 
)

# 2. 

city <- cars$MPG.city
highway <- cars$MPG.highway

# classes para city

amplitude1 <- diff(range(city))

Nclasses1 <- nclass.Sturges(city)

Ampclasses1 <- (amplitude1/Nclasses1)

limiteclasses1 <- seq(min(city), max(city), Ampclasses1)

Freq1 <- table(cut(city,breaks = limiteclasses1,right = FALSE,include.lowest = TRUE))

# classes para highway

amplitude2 <- diff(range(highway))

Nclasses2 <- nclass.Sturges(highway)

Ampclasses2 <- amplitude2/Nclasses2

limiteclasses2 <- seq(min(highway), max(highway), Ampclasses2)

Freq2 <- table(cut(highway, breaks = limiteclasses2, right = FALSE, include.lowest = TRUE))

par(mfrow=c(1,2))

# Plotando city
hist(city,
     breaks = limiteclasses1,
     ylim = c(0, max(Freq1) + 10),
     xlim = c(min(city) - 5, max(city) + 5), 
     ylab = "Absolute frequency", 
     main = "MPG.city"
)

# Plotando highway
hist(
  highway,
  breaks = limiteclasses2,
  ylim = c(0, max(Freq2) + 10),
  xlim = c(min(highway) - 5, max(highway) + 5),
  main = "MPG.Highway",
  ylab = "Absolute Frequency"
)

# Boxplot separados

boxplot(city, horizontal = TRUE, main = "city")

boxplot(highway, horizontal = TRUE, main = "highway")

# plotando os graficos juntos

par(mfrow=c(2,1))

boxplot(city, horizontal = TRUE, main = "city")

boxplot(highway, horizontal = TRUE, main = "highway")

# 3.

# Gerar um grafico de pizza para a coluna Origin

origin <- table(cars$Origin)
origin

# calculando porcentagens
percentages <- round((origin/sum(origin))*100) 

# formando labels com porcentagens
labels <- paste(percentages, "%", sep = "")

par(mfrow=c(1,1))

# Plotando gráfico de pizza
pie(origin, main = "origin", labels = labels, col = c("lightblue", "yellow"))

# Adcionando legenda
legend("bottomright", legend = c("non-usa", "USA"), fill = c("lightblue", "yellow"), cex = 0.8)

# Gerar gráfico de barra com as colunas type e origin

# Grafico de barras com as colunas type e origin
type <- table(cars$Type)

par(mfrow=c(1,2))

lt <- length(names(type))
lo <- length(names(origin))

# Plotando type
barplot(
  type,
  ylim = c(0, max(type) + 5),
  main = "type",
  ylab = "Absolute Frequency",
  col = rainbow(lt)
)

# Plotando origin
barplot(
  origin,
  ylim = c(0, max(origin) + 5),
  main = "origin",
  ylab = "Absolute Frequency",
  col = rainbow(lo)
)

# 4)

# construindo distribuição de frequencia para a coluna MPG.city

mpgCity <- cars$MPG.city

# amplitude <- diff(range(mpgCity))
# 
# NK <- nclass.Sturges(mpgCity)
# 
# Ampclasses <- round(amplitude/NK, digits = 2)
# 
# limiteclasses <- seq(min(mpgCity), max(mpgCity), Ampclasses)

# Frequencia agrupada em 4 grupos
Freq <- table(cut(mpgCity, breaks = 4, right = FALSE, include.lowest = TRUE))

FreqAC <- cumsum(Freq) # Frequencia absoluta acumulada
FreqRel <- prop.table(Freq) # Frequencia relativa
FreqRelAC <- cumsum(FreqRel) # Frequencia relativa acumulada

# Formando tabela para TabMpgCity
TabMpgCity = cbind(Freq,FreqAC, FreqRel = round(FreqRel*100,digits = 2),
                             FreqRelAC= round(FreqRelAC*100,digits = 2))

# construindo distribuição de frequencia para a coluna MPG.highway 
# Fuel.tank.capacity

ftc <- cars$Fuel.tank.capacity

# Frequencia com classes nao equidistantes
Freq <- table(cut(ftc, breaks = quantile(ftc), right = FALSE, include.lowest = TRUE))

FreqAC <- cumsum(Freq) # Frequencia absoluta acumulada
FreqRel <- prop.table(Freq) # Frequencia relativa
FreqRelAC <- cumsum(FreqRel) # Frequencia relativa acumulada

# Formando tabela para fuel.tank.capacity
TabFtc = cbind(Freq,FreqAC, FreqRel = round(FreqRel*100,digits = 2),
                   FreqRelAC= round(FreqRelAC*100,digits = 2))

# Tabela cruzada
cTable <- table(TabMpgCity, TabFtc)
cTable