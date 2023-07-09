#Titanic Analise Exploratória, considerando sobreviventes
#definição do diretorio
setwd("C:/Users/Ana Januário/Desktop/Titanic")

#leitura dos dados
dados<- read.csv("train_titanic_kaggle.csv", sep=';')

#visualização do formato dos dados
str(dados)
#temos que fazer algumas mudanças para deixar os dados corretos
#Age
dados$Age<-as.numeric(dados$Age)

#Fare
dados$Fare <- gsub(",", ".", dados$Fare)
dados$Fare<- as.numeric(dados$Fare)

dados$Survived<-as.factor(dados$Survived)

table(dados$Pclass)

dados$Pclass<-ifelse(dados$Pclass==1, '1st class', ifelse(dados$Pclass==2, '2nd class', '3rd class'))
dados$Pclass<- as.factor(dados$Pclass)
table(dados$Pclass)

#Variável Embarqued
table(dados$Embarked)
# verificamos uma classe de NA
# Neste caso, preenchemos os valores omisssos com a classe com maior numero de observação
dados[dados$Embarked== '', "Embarked"]<-'S'
table(dados$Embarked) #agora sim, vamos transforma-lo em factor
dados$Embarked<- as.factor(dados$Embarked)

#verificando NAs
colSums(is.na(dados))

#vamos lidar com os valores omissos da idade
# Existem varias formas de lidar com os valores omissos:
# - Eliminar a observação, reduz a quantidade de informação
# - Substituir pela média ou mediana. Pode enviesar a amostra 
# - Substituir por um modelo regressivo, um pouco mais complexo, 
# mas pode ser melhor que as demais opção

#vamos substituir os NAs da idade por um modelo regressivo
#problema modelos regressivos lineares assumem pressupostos
# Mas aqui vamos ignorar os prossupostos 
# vamos apenas criar um modelo para a variável sem considerar os outliers

upper.whisker.Age<-boxplot.stats(dados$Age)$stats[5]
outlier.titanic<- dados$Age < upper.whisker.Age
dados_sem_outlieridade<-dados[outlier.titanic,]

#Para compor as variáveis da regressão, usamos aquelas que 
#apresentarem significancia na avaliação univariada

summary(lm(dados_sem_outlieridade$Age  ~ dados_sem_outlieridade$Survived)) #Não sig
summary(lm(dados_sem_outlieridade$Age  ~ dados_sem_outlieridade$Pclass)) #Sig
summary(lm(dados_sem_outlieridade$Age  ~ dados_sem_outlieridade$Sex)) #marginalmente sig
summary(lm(dados_sem_outlieridade$Age  ~ dados_sem_outlieridade$SibSp)) #sig
summary(lm(dados_sem_outlieridade$Age  ~ dados_sem_outlieridade$Parch)) #sig
summary(lm(dados_sem_outlieridade$Age  ~ dados_sem_outlieridade$Fare)) #sig
summary(lm(dados_sem_outlieridade$Age  ~ dados_sem_outlieridade$Embarked)) #2 categorias sig


#Aplicar a transformação log(1+x) nos valores da idade
dados_sem_outlieridade$Age <- log1p(dados_sem_outlieridade$Age) #não queremos idade negativas

Age.equation= "Age ~ Pclass + Sex +SibSp + Parch +  Fare + Embarked"

#construir o modelo
Age.model<-lm(
  formula = Age.equation,
  data = dados_sem_outlieridade #dados sem outliers
)

#as linhas com NAs na idade
Age.row<-dados[
  is.na(dados$Age),
  c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")
]

#predição da idade, considerando nosso modelo
Age.predictions<-predict(Age.model, newdata = Age.row)

# Aplicar a transformação inversa exp(x-1) nas previsões para obter os valores corrigidos
Age.predictions <- expm1(Age.predictions)

#vamos adicionar as idades preditas pelo modelo na nossa base de dados
dados[is.na(dados$Age), "Age"] <- Age.predictions

#verificar se ainda há NA
table(is.na(dados$Age))
summary(dados$Age)

#para ver os dados
View(dados)

#vamos dar uma olhada no sobrevivencia
#como vimos anteriormente podemos usar graficos para ver melhor os dados
#vamos construir uma tabela que vai servir de base para o piechart
tabela_survive<-table(dados$Survived)

pie(tabela_survive)

#já vimos que o piechart padrão é bem feio
# Podemos usar uma biblioteca mais vocacionada para graficos como
#é o caso da famosa ggplot2!

#install.packages("ggplot2")
library(ggplot2)

dfs<-data.frame(
  Survived = c(0, 1),
  Count = c(sum(dados$Survived == 0), sum(dados$Survived == 1))
)
dfs$Survived<-as.factor(dfs$Survived)

#primeira construção do pie chart
grafico_pie <- ggplot(dfs, aes(x = "", y = Count, fill = Survived)) +
  geom_col(width = 1) +
  coord_polar(theta = "y")

#Trocar as cores do piechart
grafico_pie +
  scale_fill_manual(values = c("#ff4500", "#2E8B57"))

#vamos add label, titulo e a percentagem de sobrevivencia
grafico_pie +
  geom_text(aes(label = paste0(Count, " (", scales::percent(Count/sum(Count)), ")")),
            position = position_stack(vjust = 0.5), colour = "black", fontface = "bold") +
  scale_fill_manual(values = c("#ff4500", "#2E8B57")) +
  labs(title = "Gráfico de pizza - sobrevivência no Titanic", x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


#se for muito avesso a piechart e preferir barchart
# Adicionando a variável "Cor" com as cores correspondentes a cada categoria
dfs$Cor <- ifelse(dfs$Survived == 0, "#ff4500", "#2E8B57")
dfs$Percentagem <- with(dfs, round(dfs$Count / sum(dfs$Count) * 100, 0))


ggplot(data = dfs, aes(x = Survived, y = Count, fill = Cor)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  geom_text(aes(label = paste0(Percentagem, "%")), 
            position = position_stack(vjust = 0.5), colour = "black", fontface = "bold")+
  labs(title = "Gráfico de barras - % de sobrevivencia no Titanic")

#portanto, mais de 60% das pessoas a bordo do Titanic, morreram

#Vamos ver a sobrevivencia por homens e mulheres
#haviamos visto anteriormente que a amostra quanto ao genero era desbalanceada
#comprovado por teste de proporção
chisq.test(table(dados$Sex))
#verificação dos resíduos
chisq.test(table(dados$Sex))$res
#portanto, temos que terem mente que temos mais homens do que mulheres na amostra

tabela1<-table(dados$Sex, dados$Survived)
tabela1_ggplot<- as.data.frame(tabela1)

tabela1_ggplot$percentage<- paste0(as.data.frame(round(prop.table(table(dados$Sex, dados$Survived))*100,2))$Freq,"%")

colnames(tabela1_ggplot) <- c("Sex", "Survived", "Count", "Percentage")


ggplot(tabela1_ggplot, aes(x = Sex, y = Count, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#ff4500", "#2E8B57")) +
  labs(x = "Sex", y = "Count", title = "Sobrevivencia por genero") +
  theme_minimal() +
  geom_text(aes(label = Percentage), 
            position = position_dodge(width = 0.7),
            vjust = -0.5, 
            fontface = "bold")

#se preferir trabalhar com barras empilhadas
#pode utilizar a função position = "stack"

ggplot(tabela1_ggplot, aes(x = Sex, y = Count, fill = Survived)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#ff4500", "#2E8B57")) +
  labs(x = "Sex", y = "Count", title = "Sobrevivencia por genero") +
  theme_minimal() +
  geom_text(aes(label = Percentage), 
            position = position_stack(vjust = 0.5),
            fontface = "bold")


#a quantidade de homens que sobreviveu é menor 
#quando comparado com a quantidade de mulher que sobreviveu,
#nem sugerir nada, sem antes efetuar tratamento como Reamostragem, ponderação, ou etc.

#verificar a sobrevivencia pela idade
grafico_hist <- ggplot(dados, aes(x = Age, fill = Survived))

grafico_hist + geom_histogram(binwidth = 5, aes(fill = Survived), position = "identity")

grafico_hist + 
  geom_histogram(binwidth = 5, position = "identity") +
  scale_fill_manual(values = c("#ff4500", "#2E8B57")) +
  labs(x = "Age", y = "Count", title = "Histograma da Idade por sobrevivencia") +
  theme_minimal()
#com relação a idade, não parece haver bem um padrão

#se preferir o grafico com linhas
grafico_hist_lin <- ggplot(data = dados, aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#ff4500", "#2E8B57")) +
  labs(x = "Age", y = "Density", title = " Densidade da Idade por Sobrevivencia") +
  theme_minimal()

grafico_hist_lin

#Vamos testar se há diferença significativa nas distribuições das idades  
#entre grupos através do teste estatístico de Wilcoxon-Mann-Whitney

# Separando as idades dos sobreviventes e não sobreviventes
idade_sobreviventes <- dados$Age[dados$Survived == 1]
idade_nao_sobreviventes <- dados$Age[dados$Survived == 0]
# Teste de Wilcoxon-Mann-Whitney
wilcox.test(idade_sobreviventes, idade_nao_sobreviventes)
#Não se rejeita H0, ou seja, não há diferença nas distribuições das idades 
#entre os grupos de sobreviventes e não sobreviventes


#Sobrevivencia pela classe
tabela2<-table(dados$Pclass, dados$Survived)
chisq.test(tabela2)
#data-frame para o ggplot
tabela2_ggplot<- as.data.frame(tabela2)
#criar a coluna de percentagem de sobrevivencia
tabela2_ggplot$percentage<- paste0(as.data.frame(round(prop.table(table(dados$Pclass, dados$Survived))*100,2))$Freq,"%")

colnames(tabela2_ggplot) <- c("PClass", "Survived", "Count", "Percentage")

ggplot(tabela2_ggplot, aes(x = PClass, y = Count, fill = Survived)) + #defino variáveis de interesse
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + #defino o meu tipo de barchart, ver definição geom_bar no help
  scale_fill_manual(values = c("#ff4500", "#2E8B57")) +       #defino as cores
  labs(x = "PClass", y = "Count", title = "Sobrevivencia por Classe") + #minha label no gráfico
  theme_minimal() +                                             #o tema que quero usar
  geom_text(aes(label = Percentage), 
            position = position_dodge(width = 0.5),             #o texto dentro das barras
            vjust = -0.5, 
            fontface = "bold")

#também não podemos afirmar nada, pois se trata de uma variável desbalanceada

#será que conseguimos criar um modelo que descreva a sobrevivencia no titanic
#neste caso de variáveis dicotomicas podemos empregar a tecnica para modelo logistico


#-------------------------------------------------------------------------------------#
###                       Regressão Logistica                                       ###
#-------------------------------------------------------------------------------------#

#Primeiro passo, ajustar o Modelo nulo
fit0 <- glm(Survived ~ 1,        # Y ~ lista de variaveis explicativas. ~1 indicia que so se considera a constante (modelo nulo) 
            data=dados,                     # nome do conjunto de dados
            family=binomial(link=logit))    # distribuicao de Y e funcao de ligacao
# -- coeficientes do modelo e sua significancia
#o modelo logistico pode ser expresso com
#log(p/(1-p)) = β0 + β1x
#portando podemos confirmar se nossa variável está codificada de forma correta
summary(fit0)

ifelse(round((342/549),9) == round(exp(fit0$coefficients),9), "Está correto", "Não está correto")
#portanto, nossa codificação está correta e o intercepto é significativo

#Modelo simples

fit1 <- glm(dados$Survived ~ dados$Pclass, 
            data=dados,family=binomial(link=logit))
summary(fit1)

dados$Sex<- as.factor(dados$Sex)
fit2 <- glm(dados$Survived ~ dados$Sex, 
            data=dados,family=binomial(link=logit))
summary(fit2)

fit3 <- glm(dados$Survived ~ dados$Age, 
            data=dados,family=binomial(link=logit))
summary(fit3) #não sig

fit4 <- glm(dados$Survived ~ dados$SibSp, 
            data=dados,family=binomial(link=logit))
summary(fit4)  #não sig

fit5 <- glm(dados$Survived ~ dados$Parch, 
            data=dados,family=binomial(link=logit))
summary(fit5)


fit6 <- glm(dados$Survived ~ dados$Fare, 
            data=dados,family=binomial(link=logit))
summary(fit6)

fit7 <- glm(dados$Survived ~ dados$Embarked, 
            data=dados,family=binomial(link=logit))
summary(fit7)

#modelo multivariado
#embora Age não tenha apresentado significancia individual testamos add
#com as demais variáveis e foi verificado sig em conjunto

fitMULT1<-glm(dados$Survived ~ Pclass+Sex+Parch+Fare+Embarked+Age,
              data=dados,family=binomial(link=logit))

summary(fitMULT1)

#percebemos que a categoria EmbarkedQ   não é significativa estatisticamente, poderiamos 
#tentar agrupar as categorias dessa variável
#mas na verdade não entendo bem o que ela significa
#vamos remover a categoria e ver como se comporta
#ATENÇÃO, remover variáveis significativas de um modelo não é o recomendado
#melhor é entender o que a variável significa e trata-la com atenção

fitMULT2<-glm(dados$Survived ~ Pclass+Sex+Parch+Fare+Age,
              data=dados,family=binomial(link=logit))
summary(fitMULT2)

#Fare deve sair, não é significativo no modelo


fitMULT3<-glm(Survived ~ Pclass+Sex+Parch+Age,
              data=dados,family=binomial(link=logit))
summary(fitMULT3)


#pseudo coeficiente de determinação 
(n<-length(dados$Survived)) # nº observações da variavel resposta
(R2N<-(1-exp((fitMULT3$dev-fitMULT3$null)/n))/(1-exp(-fitMULT3$null/n)))  #R2=47% é um bom coeficiente para um modelo logistico


# ------ avaliar pressuposto de linearidade (para covariavel Age) -----

# -- Metodo lowess
plot(lowess(predict(fitMULT3)~dados$Age), 
     type="l", main = "Linearidade idade", xlab="idade", ylab="logOdds")

#A idade não tem aspecto linear, portanto não atende ao pressuposto

# -- Metodo GAM
#install.packages("gam")
library(gam)
fitgam <- gam(Survived ~ Pclass+Sex+Parch+s(Age), 
             family=binomial("logit"),
             data=dados)
plot(fitgam)
#A linearidade não anda a ser o forte por aqui
#

#vamos tentar uma transformação para ver se ajuda com a linearidade
# Instalar e carregar o pacote "MASS" para usar a função boxcox()
#PS eu já havia tentando o log e o sqrt, mas continuou não linear
#install.packages("MASS")
library(MASS)

# Calcular o lambda ótimo para a transformação de Box-Cox

bc_result <- boxcox(dados$Age ~ 1, plotit = FALSE)
lambda <- bc_result$x[which.max(bc_result$y)]

# Aplicar a transformação de Box-Cox na variável Age
dados$Age_transformed <- (dados$Age^lambda - 1) / lambda



fitMULT4<-glm(Survived ~ Pclass+Sex+Parch+Fare+Embarked+Age_transformed,
              data=dados,family=binomial(link=logit))
summary(fitMULT4)


fitMULT5<-glm(Survived ~ Pclass+Sex+Parch+Fare+Age_transformed,
              data=dados,family=binomial(link=logit))
summary(fitMULT5)


fitMULT6<-glm(Survived ~ Pclass+Sex+Parch+Age_transformed,
              data=dados,family=binomial(link=logit))
summary(fitMULT6)


#linearidade gam
fitgam <- gam(Survived ~ Pclass+Sex+Parch+s(Age_transformed), 
              family=binomial("logit"),
              data=dados)
plot(fitgam)
#deu uma melhorada, mas não é grande coisa

#
#Interações entre a svariáveis
#idade e classe
fitINT1<-glm(Survived ~  Pclass + Sex+ Parch + Age_transformed + Pclass:Age_transformed, data = dados, family = binomial(link = logit))
summary(fitINT1)

#nesse caso a interação não foi sig

fitINT2<-glm(Survived ~ Pclass + Sex+ Parch + Age_transformed + Sex:Age_transformed, data = dados, family = binomial(link = logit))
summary(fitINT2)
#Sig
#relevância do efeito da idade transformada na sobrevivência podem ser mais bem 
#capturadas pela interação do que pelas variáveis individuais isoladamente.
#vamos continuar avaliando as demais interações

fitINT3<-glm(Survived ~ Pclass + Sex+ Parch + Age_transformed + Parch:Age_transformed, data = dados, family = binomial(link = logit))
summary(fitINT3)
#não sig


fitINT4<-glm(Survived ~ Pclass + Sex+ Parch + Age_transformed + Pclass:Parch, data = dados, family = binomial(link = logit))
summary(fitINT4)
#marginalmente sig


fitINT5<-glm(Survived ~ Pclass + Sex+ Parch + Age_transformed + Sex:Parch, data = dados, family = binomial(link = logit))
summary(fitINT5)
#Significativo


fitINT6<-glm(Survived ~ Pclass + Sex+ Parch + Age_transformed + Sex:Pclass, data = dados, family = binomial(link = logit))
summary(fitINT6)
#uma classe é sig


# Obter o valor do AIC para cada modelo
AIC_values <- c(AIC(fitINT2), AIC(fitINT4), AIC(fitINT5), AIC(fitINT6))
# Obter o valor do BIC para cada modelo
BIC_values <- c(BIC(fitINT2), BIC(fitINT4), BIC(fitINT5), BIC(fitINT6))
# Verificar os valores do AIC e BIC
AIC_values
BIC_values

#neste caso os menores valores de AIC e BIC coincidiram com o modelo 6
#existem outros fatores que devem ser avaliados
#Mas vamos parar por aqui
(R2N<-(1-exp((fitINT6$dev-fitINT6$null)/n))/(1-exp(-fitINT6$null/n)))



#Bondade do ajustamento
# -- Teste de Hosmer e Lemeshow (quando ha pelo menos uma covariavel quantitativa)
#install.packages("generalhoslem")
library(generalhoslem)             # ativar pacote necessario
?generalhoslem

(hl<-logitgof(dados$Survived,       # valores observados y (dependente)
              fitted(fitINT6),     # valores ajustados y^ 
              g = 8))          # numero de classes a considerar
hl$expected                        # valores esperados

#o modelo não tá com bom ajuste


# ----------------- Capacidade discriminativa (curva ROC) -------------------
library(Epi)
ROC(form=Survived ~ Pclass+Sex+Parch+Age+Sex:Pclass,
    data=dados, 
    plot="ROC",
    PV=T,
    MX=T,
    AUC=T)

#nada mal, segundo nosso modelo conseguimos classificar a sobrevivencia 
#com uma sensibilidade de 81%, vamos testar com a matriz de confusão

#matriz de confusão
Yest<-fitINT6$fitted.values>0.284
table(Yest) #modelo estima
Yobs<-fitINT6$model$Survived
table(Yobs)  #o que foi observado na pratica
table(Yobs,Yest)
#Acerto
(419+278)/(419+278+64+130)
#De fato o nosso modelo acertou 78% da sobrevivencia

#Vamos fazer a predição nos dados de teste

titanic.test<- read.csv("test_titanic_kaggle.csv", sep = ';',  header= T)
str(titanic.test)
#temos que add a coluna survive
titanic.test$Survived <- NA
#Temos que adequar as variáveis de interesse
str(dados)

#Poderia ter feito tudo junto como fiz no codigo anterior, mas vou fazer separadamente

table(titanic.test$Embarked) #não há NA
titanic.test$Embarked<- as.factor(titanic.test$Embarked)

titanic.test$Pclass<-ifelse(titanic.test$Pclass==1, '1st class', ifelse(titanic.test$Pclass==2, '2nd class', '3rd class'))
titanic.test$Pclass<- as.factor(titanic.test$Pclass)
table(titanic.test$Pclass)

titanic.test$Sex<- as.factor(titanic.test$Sex)

titanic.test$Fare<-gsub(",", ".", titanic.test$Fare)
titanic.test$Fare<-as.numeric(titanic.test$Fare)
sum(is.na(titanic.test$Fare))
#Vou substituir pela mediana por ser apenas 
library(fBasics)
basicStats(titanic.test$Fare)
#mediana=14.454200
titanic.test$Fare[is.na(titanic.test$Fare)]<-14.454200
table(is.na(titanic.test$Fare))

#com a idade faremos uso do modelo
titanic.test$Age<-as.numeric(titanic.test$Age)
sum(is.na(titanic.test$Age))

#vamos aplicar o mesmo modelo ja construido

#as linhas com NAs na idade
Age.row2<-titanic.test[
  is.na(titanic.test$Age),
  c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked")
]

#predição da idade, considerando nosso modelo
Age.predictions2<-predict(Age.model, newdata = Age.row2)

titanic.test[is.na(titanic.test$Age), "Age"] <- Age.predictions2
basicStats(titanic.test$Age) #sem valores negativos
table(is.na(titanic.test$Age))

#agora vamos fazer a coluna Age_transformed que está presente em nosso modelo
titanic.test$Age_transformed <- (titanic.test$Age^lambda - 1) / lambda


#agora sim... Vamos fazer a previsão de sobrevivencia com nosso modelo fitINT6

Survived<-predict(fitINT6, newdata = titanic.test)
#vamos usar como ponto de corte da sobrevivencia o valor da curva ROC

resultRL<- as.factor(ifelse(Survived > 0.284,  "1", "0"))
table(resultRL)

PassengerId <- titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<- resultRL
tail(output.df)

write.csv(output.df, file="kaggle_submission4RL.csv", row.names = FALSE)

#se a base de dados não alterou com nosso modelo atingimos uma taxa de acerto de 76%
