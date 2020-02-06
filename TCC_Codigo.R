install.packages("readxl")
install.packages("rlang")
install.packages("AMR")
install.packages("dplyr")
install.packages("Metrics")
install.packages("HH")
install.packages("ROCR")
install.packages("rpart")
install.packages("randomForest")
install.packages("MASS")
install.packages("psych")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("party")
install.packages("rpart.plot")

library(readxl)
library(rlang)
library(AMR)
library(dplyr)
library(Metrics)
library(HH)
library(ROCR)
library(rpart)
library(randomForest)
library(MASS)
library(psych)
library(ggplot2)
library(ggpubr)
library(party)
library(rpart.plot)

setwd("//GABRIEL/Users/Gabriel/Desktop/TCC/Código")

base <- read_excel("C:/Users/Gabriel/Desktop/TCC/Bases/Petrobras/Base_TCC .xlsx", sheet = "Base")

##Análise Exploratória##

#Quantitativas
summary(base)

boxplot(base$PETR4_Volume_Dia_Anterior,
        data=base,
        col="dark blue",
        border="black"
)


boxplot(base$PETR4_Volume_Dia_Anterior~base$PETR4_Var_Resposta,
        data=base,
        col="dark blue",
        border="black",
        xlab="PETR4_Var_Resposta",
        ylab="PETR4_Volume_Dia_Anterior"
)

hist(base$PETR4_Volume_Dia_Anterior,
     col="dark blue",
     border="black",
     main = "",
     xlab="PETR4_Volume_Dia_Anterior",
     ylab="Frequência"
)


#Gráfico de Correlação das Qualitativas

pairs.panels(base[3:11])


#Qualitativas
base %>% freq(PETR4_Var_Resposta)
base %>% freq(PETR4_Var_Negativa_2_dias)
base %>% freq(PETR4_Var_Positiva_2_dias)
base %>% freq(`PETR4_Var_Maior_1%`)
base %>% freq(Petroleo_Brent_Var_Positiva_2_dias)
base %>% freq(Petroleo_Brent_Var_Negativa_2_dias)
base %>% freq(IBOV_Var_Positiva_2_dias)
base %>% freq(IBOV_Var_Negativa_2_dias)
base %>% freq(DOW_JONES_Var_Positiva_2_dias)
base %>% freq(DOW_JONES_Var_Negativa_2_dias)
base %>% freq(`Mês Eleitoral`)
base %>% freq(`Mês Pré-Eleitoral`)
base %>% freq(`Mês Pós-Eleitoral`)

graf <- ggplot(base,aes(x=base$Mes_Pos_Eleitoral))+geom_histogram()+facet_grid(~PETR4_Var_Resposta)

graf +labs(title = "", x="Mes_Pos_Eleitoral",y="Frequência" )


##Modelo##


treino <- base[1:1820,2:25]
teste <- base[1821:2600,2:25]

modelo <- glm(PETR4_Var_Resposta ~.,family=binomial(link='logit'),data=treino)

summary(modelo)
vif(modelo)

modelo_p <- glm(PETR4_Var_Resposta ~PETR4_Var_Abertura_Hoje_Dia_Anterior +DOW_JONES_Var_Dia_Anterior+Var_Cambio_Nominal_Dia_Anterior+Petroleo_Brent_Var_Positiva_2_dias,family=binomial(link='logit'),data=treino)

summary(modelo_p)
vif(modelo_p)

modelo_teste <- predict(modelo_p,teste,type = "response")

fitted.results <- ifelse(modelo_teste > 0.5,1,0)
fitted.results

#Análise do Resultado
  
table(teste$PETR4_Var_Resposta,fitted.results )

metrics <- prediction(modelo_teste, teste$PETR4_Var_Resposta)

## Acuracia
perf <- performance(metrics, measure='acc')
plot(perf)

## ROC
perf <- performance(metrics, measure="tpr",
                    x.measure="fpr")

auc.perf = performance(metrics, measure = "auc")
auc.perf@y.values

plot(perf)
abline(a=0,b=1)

########################
#Árvore de Treino-ctree#
########################

output.tree <- ctree(
  PETR4_Var_Resposta ~PETR4_Var_Abertura_Hoje_Dia_Anterior +DOW_JONES_Var_Dia_Anterior+Var_Cambio_Nominal_Dia_Anterior+Petroleo_Brent_Var_Positiva_2_dias,
  data = treino )

output.tree

plot(output.tree,type = "simple")

nodes(output.tree, 3)
  
  ctree.predict = predict(output.tree,teste)
  ctree.predict_f <- ifelse(ctree.predict > 0.5,1,0)
  
  table(ctree.predict_f, teste$PETR4_Var_Resposta)


pred = prediction(ctree.predict, teste$PETR4_Var_Resposta)
roc = performance(pred, measure="tpr", x.measure="fpr")
plot(roc, col="black", lwd=2) 
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=2)

auc = performance(pred, 'auc')
slot(auc, 'y.values')