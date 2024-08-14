setwd("C:\\Atividade1Estatistica")

cars <- read.csv("mtcars.csv", header = TRUE, dec = ",", sep = ';')

# a)

mpg <- cars$mpg # atribuimos a coluna mpg à variável mpg

# Calculando minimo, max, media , mediana, desvio padrão, quartis 

mini <- min(mpg)
maxi <- max(mpg)
mpgm <- mean(mpg)
mpgme <- median(mpg)
mpgst <- sd(mpg)
q <- quantile(mpg)

# construindo histograma
amplitude <- diff(range(mpg))

Nclasses <- nclass.Sturges(mpg)

AmpClasse <- round (amplitude / Nclasses)

limitesclas <- seq (round(mini) , round(maxi) , AmpClasse)

Freq = table(cut(mpg, breaks = limitesclas, right=FALSE, ))

# plotando histograma
hist(mpg, breaks = limitesclas,
     ylab = "Frequencias absolutas",
     xlab = "mpg",
     main = "Hist mpg",
     xlim = c(round(mini) - 3, round(maxi) + 3), 
     ylim = c(0, max(Freq) + 5)
)

#plotando boxplot

boxplot(mpg, col = "Orange")

# b)

# plotando no bloxplot disp e hp

table = cbind(Displacement = cars$disp, Horsepower = cars$hp)

boxplot(table, col = "orange", varwidth = TRUE)

# c)

cQsec <- cars$qsec

#calculando quartis
q <- quantile(cQsec)

#separando os dados em seus respectivos quartis
Freq <- table(cut(cQsec, breaks = q))

# calculando

FreqAC = cumsum(Freq) # absoluta acumulada
FreqRel <- round(prop.table(Freq) * 100, digits = 2) # frequencia relativa
FreqRelAc <-  cumsum(FreqRel) # relativa acumulada

# construindo tabela

TableResult <- cbind(Freq, FreqAC = FreqAC, FreqRel = FreqRel, FreqRelAc=FreqRelAc)

TableResult 

# Gerar histograma

amplitude <- diff(range(cQsec))

Nclasses <- nclass.Sturges(cQsec)

AmpClasse <- (amplitude / Nclasses)

limitesclas <- seq(14.5, 22.9, AmpClasse)

# Plotando histograma
h <- hist(cQsec, breaks = limitesclas,
          xlim = c(min(cQsec) - 1, max(cQsec) + 1),
          ylim = c(0, max(Freq) + 3))

# d)

cHp <- cars$hp

# plotando histograma
h <- hist(cHp)

bks = h$breaks # utilizando as classes sugeridas

Freq <- table(cut(cHp, breaks = bks)) # tabela de frequencias

hist(cHp,
     breaks = bks,
     ylab = "Abs Frequencies",
     xlab = "classes",
     ylim = c(0, max(Freq) + 1), 
     xlim = c(min(cHp) - 10, max(cHp) + 10))

# e)

motor <- cars$motor

marcha <- cars$marcha

mat <- cbind(motor = motor, marcha = marcha)

v_and_manual <- subset(mat, subset = mat[, 1] == "Em forma de V" & mat[, 2] == "Manual")
v_and_auto <- subset(mat, subset = mat[, 1] == "Em forma de V" & mat[, 2] == "Automatico")

# FreqMotor <- table(cars$motor)
# 
# pie(FreqMotor, col = c("yellow", "orange")) # plotando dados qualitativos
# 
# hp <- cars$hp
# 
# amplitude <- diff(range(hp))
# 
# Nclasses <- nclass.Sturges(hp)
# 
# AmpClasse <- round (amplitude / Nclasses)
# 
# limitesclas <- seq (min(hp) , max(hp) + AmpClasse, AmpClasse-1)
# 
# FreqHp = table(cut(hp, breaks = limitesclas, right=FALSE))
# FreqHp
# 
# labels <- c("[52,98)", "[98,144)", "[144,190)", "[190,236)", "[236,282)", "[328,374)")
# 
# pie(FreqHp, col = rainbow(length(labels)), main = "Horse Power", labels = labels)

# f) 

motor <- data.frame(cars$motor) # coluna qualitativa

library(dplyr)

FreqMotor <- motor %>%
  count(cars.motor)

labels <- c("Forma V", "Reto")

# plotando coluna quantitativa
barplot(c(FreqMotor[1,2], FreqMotor[2,2]),
        main = "Motores",
        col = c("yellow", "orange"),
        ylim = c(0, 20),
        names.arg = labels
)

FreqHp <- table(cars$hp) # coluna quatitativa

# plotando coluna quantitativa
barplot(FreqHp,
        ylab = "Absolute Frequency",
        xlab = "Horse power",
        ylim = c(0, max(FreqHp)),
)

# g)

install.packages('tidyverse')
library(tidyverse)

# escolhendo colunas
hp <- cars$hp
qsec <- cars$qsec

c <- cor(hp, qsec) # calculando correlacao
c

scatter.smooth(hp, qsec)

# Agrupando em quartis
quantile_hp <- table(cut(hp, breaks = quantile(hp), include.lowest = TRUE))
quantile_qsec <- table(cut(qsec, breaks = quantile(qsec), include.lowest = TRUE))

dt$quartil <- ntile(dt, 4)

# dt %>% 
#    group_by(quartil)
# 
# table(dt)

# h)

install.packages('e1071')
library(e1071)

hp <- cars$hp

# gerando histograma

amplitude <- diff(range(hp))

Nclasses <- nclass.Sturges(hp)

AmpClasse <- (amplitude / Nclasses)

limitesclas <- seq (min(hp) , max(hp), AmpClasse)

Freq = table(cut(hp, breaks = limitesclas, right=FALSE))

# plotando histograma
hist(
  hp,
  breaks = limitesclas,
  ylim = c(0, max(Freq) + 10),
  xlim = c(min(hp) - 10, max(hp) + 10),
  ylab = "Frequencia absoluta",
  main = "Horse Power",
)

# Calculo ap2
ap2 <- (mean(hp) - median(hp)/ sd(hp))
ap2

# Calculo de skewness
assimetria <- skewness(hp)
assimetria
