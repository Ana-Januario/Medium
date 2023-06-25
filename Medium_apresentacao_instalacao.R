#Apoio ao Medium
#R e RStudio: apresentação e instalação

#Não se esqueça de definir o diretorio
#Pode definir manualmente na janela section ou usar o comando setwd
?setwd
setwd("C:/Users/meu_user/Desktop/pasta_R")

#com o diretório definido podemos iniciar as analises

#somar dois números
#Vamos supor que eu queira somar 10+5
10 + 5

#Declarar variáveis utilizando operador de atribuição (<- ou =)
x <- 10
y <- 5

#podemos fazer a soma pelo operador
x+y
#ou usar a função somar já predefinida no r
sum(x,y)
#para mais informações sobre a função sum
?sum

#Construindo uma função em r
calcular_area <- function(comprimento, largura) { #atribuindo nomes do componentes da função
  area <- comprimento * largura #definindo como os componentes se relacionam
  return(area) #o que queremos que a função retorne
}

#aplicar a função para um retangulo de comprimento 5 e largura 3
resultado <- calcular_area(5, 3)
print(resultado) #ou simplesmente
resultado

#loop
for (i in 1:5) {
  print(i)
}

#loop para fazer operações
for (i in 1:5) {
  quadrado <- i^2
  print(quadrado)
}

