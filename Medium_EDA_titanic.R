#Analise exploratória de dados - dataset Titanic

#definição do diretório
setwd("C:/Users/Ana Januário/Desktop/Titanic")

#carregamento dos dados de treino utilizando read.csv
titanic.train <-read.csv("train_titanic_kaggle.csv", sep=';', header= T)
titanic.test<- read.csv("test_titanic_kaggle.csv", sep = ';',  header= T)
#Para a leitura dar certo, certifique-se que os dados estão no diretório 
#indicado para o trabalho

#Verificar os dados em formato tabular
View(titanic.train)
View(titanic.test)

# Verificar se as duas listas de nomes de colunas são iguais,
# Metodo do olhometro - manual
names(titanic.train)
names(titanic.test)

#ou metodo automático
# Criar uma variável com os nomes das colunas de cada dataset
train_columns <- colnames(titanic.train)
test_columns <- colnames(titanic.test)

# Imprimir o resultado para cada nome de coluna
for (column_name in train_columns) {
  identical_name <- column_name %in% test_columns
  print(paste(column_name, identical_name, sep = ": "))
}

#portanto, concluimos que a variável survived não está presente no dataset de test

#vamos remover a coluna survived do dataset de treino para esta analise inicial

df.train<- titanic.train[, -which(names(titanic.train) == "Survived")]

#indicação de qual dataset a informação é proveniente
df.train$isTrainSet <- TRUE
titanic.test$isTrainSet <- FALSE

#combinar os dados
titanic.full<- rbind(df.train, titanic.test)

#verificar se o numero de linhas por dataset está igual
table(titanic.full$isTrainSet)

#verificar a estrutura dos dados
str(titanic.full)

#converter idade em numérico
titanic.full$Age<- as.numeric(titanic.full$Age)
#A variável Fare tem um porém, nela os valores usam , ou inves de .
#temos que substitui-los
titanic.full$Fare <- gsub(",", ".", titanic.full$Fare)
titanic.full$Fare<- as.numeric(titanic.full$Fare)

#Variáveis categóricas
#tratar a variável sexo
table(titanic.full$Sex) #não temos NA, nem problemas de escrita
titanic.full$Sex<- as.factor(titanic.full$Sex)

#Variável Embarqued
table(titanic.full$Embarked)
# Nesse caso também não temos problema de escrita, mas verificamos uma classe de NA
# Neste caso, preenchemos os valores omisssos com a classe com maior numero de observação
titanic.full[titanic.full$Embarked== '', "Embarked"]<-'S'
table(titanic.full$Embarked) #agora sim, vamos transforma-lo em factor
titanic.full$Embarked<- as.factor(titanic.full$Embarked)

#Variável PClass
#essa variável pode ser interpretada como primeira classe, segunda classe e terceira classe
#então vamos apenas coloca-la como factor
titanic.full$Pclass<- as.factor(titanic.full$Pclass)
table(titanic.full$Pclass) 

#verificando NAs
colSums(is.na(titanic.full))

#vamos esquecer do valores omissos por enquanto e vamos iniciar a análise estatistica

#Informações basicas
summary(titanic.full)

#Vamos supor que eu queira saber o desvio padrão da idade
#podemos usar a função stdev
# mas atenção que essa função não tolera omissos
stdev(titanic.full$Age, na.rm=TRUE)

#ou ainda podemos utilizar o pacote fBasics e usufruir da função basicStats
#install.packages("fBasics")
library(fBasics)
basicStats(titanic.full$Age)

#Analise univariada
#distribuição das variáveis numericas
par(mfrow = c(1,2))
hist(titanic.full$Age, main = "Histograma da variável Age (idade)",
     xlab= " Age (idade)", ylab= "Frequência")

hist(titanic.full$Fare, main = "Histograma da variável Fare (Valor da passagem)",
     xlab= " Fare (Valor da passagem)", ylab= "Frequência")

#teste de normalidade
#install.packages("nortest") #caso não tenha instalado
library(nortest) #chama o pacote

shapiro.test(titanic.full$Age) #teste de shapiro-wilk
lillie.test(titanic.full$Age) #teste de Kolmogorov-Smirnov com a correção de Lilliefors

#variáveis categoricas
#vamos criar um pie chart para a variável sex

table_sex<-table(titanic.full$Sex)
table_sex
table_percent<-round(table_sex/sum(table_sex)*100, 2) #vamos apresentar em percentagem

#Definir cores para cada categoria
cores <- c("female" = "#F08080", "male" = "#4682B4")

#produzir o piechart com a função pie
pie(table_sex, col = cores, radius=1, lty=1, border="white",
    labels = paste(table_percent, "%"), main = "Pie chart por % de indivíduos por sexo")

# Adicionar a legenda ao gráfico
legend("topright", legend = names(table_sex), fill = cores, title = "Sex")

#teste estatistico para verificar se as frequências observadas 
#são significativamente diferentes das frequências esperadas. 
qui.quad<-chisq.test(table_sex)
qui.quad$residuals
#Temos menos mulheres do que homens

#Pclass
#criar label, nomes inspiradas no Kaggle
Pclass_label <- ifelse(titanic.full$Pclass == "1", "Alta",
                       ifelse(titanic.full$Pclass == "2", "Média", "Baixa"))

pclass_table<-table(Pclass_label)
pclass_table
cores <- c("#DAA520", "#8B4513", "#BDB76B")

barplot(pclass_table, main = "Observação por classe", col = cores)

#teste estatistico para classes
qui.quad.class<-chisq.test(pclass_table)
qui.quad.class
qui.quad.class$residuals
#também apresenta classe desbalanceadas

#Embarqued
table(titanic.full$Embarked)

#####
#Analise bivariada
#Age e Sex

boxplot(titanic.full$Age~ titanic.full$Sex, xlab= "Sex", ylab="Age", main = "Idade por Sexo")
#Aparentemente as medias e medianas são iguais
#Mann-Whitney para verificação de igualdade das distribuições das idade entre os grupos

wilcox.test(Age ~ Sex, data=titanic.full)
#p-value = 0.0584 Ao nivel de 5% de significancia não rejeitamos a hipotese h0
#o que sugere que a distribuição da idade pelo genero são identicas

# Criar histograma com as duas classes para verificar distribuição
hist(titanic.full$Age[titanic.full$Sex == "male"], col = "blue", main = "Distribuição de idade por gênero", xlab = "Age")
hist(titanic.full$Age[titanic.full$Sex == "female"], col = "pink", add = TRUE)


par(mfrow = c(2,2))
boxplot(titanic.full$Age~ titanic.full$Sex, xlab= "Sex", ylab="Age", main = "Idade por Sexo")

boxplot(titanic.full$Age~ titanic.full$Pclass, main= "Classe por idade",
        xlab="Pclass", ylab="Age")

boxplot(titanic.full$Fare~ titanic.full$Pclass, main= "Preço da passagem por classe",
        xlab="Pclass", ylab="Fare")

boxplot(titanic.full$Fare~ titanic.full$Sex, main= "Preço da passagem por genero",
        xlab="Sex", ylab="Fare")

# Associação entre as variáveis
str(titanic.full)

#Associação entre as variávels categoricas

assoc_tab <- table(titanic.full$Pclass, titanic.full$Sex)
chisq.test(assoc_tab)
assoc_tab1 <- table(titanic.full$Embarked, titanic.full$Sex)
chisq.test(assoc_tab1)
assoc_tab2 <- table(titanic.full$Embarked, titanic.full$Pclass)
chisq.test(assoc_tab2)

#Associações entre as variáveis numéricas
cor(titanic.full$Age, titanic.full$Fare, use="complete.obs") #Como não tratamos NA tivemos que completar

#portanto a correlação é fraca
