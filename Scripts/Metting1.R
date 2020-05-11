# Nesse primero encontro, conhecemos um pouco sobre R e Rstudio
# e exploramos algumas funcoes basicas do programa e suas 
# aplicabilidades

# Mas primeiro discutimos alguns passos de logica que deve ser 
# pensado antes de comecar a programar:

### Reunir as informações que são necessarias
### Dar nome para essas informacoes
### Decompor o nosso problema em partes
### Colocar essas partes em uma sequencia logica

# CRIANDO UM OBJETO

# Para criar um objeto podemos usar o comando '<-' (Atalho:Alt-)
# Por exemplo:

a <- 2 # Atribuimos o numero 8 ao objeto 'a'

# CRIANDO UM VETOR

b <- c(3,7,8,3, 52, 9, 16) # Atribuimos um conjunto de números 
                           # ao objeto 'b'

# Aqui utilizamos a funão c() para reunir um conjunto de 
# informacoes de um mesmo tipo (número, caracter) em um vetor

# USO DO R COMO CALCULADORA

# Discutimos um pouco sobre o uso do R em funções basicas de calculo
# Essas operacoes podem ser executadas tanto no script quanto no console

c <- 2+6
d <- b^a
e <- a+c
f <- a*b
g <- b/2
h <- b-2 # Essas funcoes podem ser testadas com mais complexidade

# USO SOCIAL DO R

## EXEMPLO DA MEDIA

# Podemos utilizar funções prontas disponibilizadas em pacotes

media1 <- mean(b) 

## A funcao 'mean' calcula a media aritmetica dos dados

# Podemos escrever todo o nosso codigo para uma tarefa

media2 <- sum(b)/length(b)

## A funcao 'sum' soma os valores de um conjunto de dados
## A funcao 'length' contabiliza a quantidade de informacao 
## de um vetor

# Podemos criar funcoes para executar uma tarefa

media3 <- function(media){ # Usamos 'function' para criar uma funcao
  soma <- sum(media) # Usamos 'sum' para somar os valores
  comprimento <- length(media) # Usamos 'length' para saber o tamanho do objeto
  media <- soma/comprimento # Salvamos a razao entre soma e comprimento em media
  return(media) # Usamos a funcao 'return' para retornar o resultado da funcao
}

teste.media <- media3(media = b) # Testando a funcao

# USANDO A FUNCAO HELP

# Para consultar o help de uma funcao usamos '?'

?seq
?log

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # EXERCICIO # # # # # # # # # # # #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Utilize a funcao '?' para encontrar o help das seguintes
# funcoes e crie vetores diversos usando os argumentos de
# cada uma das funcoes

# seq
# rep

# Crie um dado do tipo matriz usando as funcoes cbind e rbind
# Use o help em caso de duvidas sobre como usar



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# INSTALANDO PACOTES

install.packages("") # Necessario o uso de aspas

# CARREGANDO PACOTES

library()

# OBSERVACAO SOBRE O USO DE FUNCOES

# Cada funcao contem os seus argumentos e o uso deles especifica
# a tarefa a ser executada pelo programa

# Vamos ver o exemplo do log

# Use ?log para entender os seus argumentos

l1 <- log(100) # calcula o log neperiano ou log natural

# Ao calcular o log sem especificar a base = log natural

l2 <- log(x = 100, base = 10) # especificando os argumentos
l3 <- log(base = 10, x = 100) # especificando os argumentos

# Ao especificar o argumento inserido, não importa a ordem de
# entrada

l4 <- log(100,10) # sem especificar os argumentos
l5 <- log(10,100) # sem especificar os argumentos

# Sem especificar o argumento inserido, depenendo da ordem de
# entrada pode mudar o resultado da funcao

# FUNCOES ACUMULADAS USADAS NO PRIMEIRO ENCONTRO

c()
mean()
sum()
length()
function()
sec()
rep()
log()
cbind()
rbind()
install.packages()
library()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # DESAFIO # # # # # # # # # # # #  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Criar uma funcao para calcular a correlacao entre dois
# conjunto de dados (variaveis)

# Use as funcoes: function, mean, sum, sqrt, return

# Passo 1: Descreva a logica da resolucao como um exercicio
# Passo 2: Crie a funcao

# O conjunto de dados para essa atividade é referente a taxa 
# de morte por covid-19 atualizado para o dia 01-05-2020

# Os dados já foram tratados e todos os municípios sem o calculo
# da taxa de morte foram retirados

# Verifique se já uma correlacao entre a populacao estimada e 
# a taxa de obitos por covid-19.

# NA PROXIMA REUNIAO IREMOS DISCUTIR A LOGICA DE PROGRAMACAO

# Teste o seu trabalho usando a funcao cor() -- use o help

dado.cor <- read.table(file = "./Dados_desafio/covid19_01-05-2020.txt", header = T)
cor(x = dado.cor$estimated_population_2019, y = dado.cor$death_rate)

# Lógica Felipe
# pegar cada valor de x e subtrair pela média
# mesmo y
# dividir pela raiz quadrada

# Lógica Pablo

# CIMA
# subtrair cada valor de x pela média de x
# subtrair cada valor de y pela média de y
# multiplica o resultado das duas subtrações
# somatorio do resultado da multiplicação = result parte cima

# BAIXO
# 1 - subtrair cada valor de x pela média de x ^2
# 2 - subtrair cada valor de y pela média de y ^2
# somatorio de 1
# somatorio de 2
# multiplica os somatorios 1 e 2
# tira a raiz da multiplicação = result parte de baixo

# parte de cima/parte de baixo = função de correlação

x <- dado.cor$estimated_population_2019
y <- dado.cor$death_rate

func.cor <- function(x,y){
  #cima
  mediax <- mean(x)
  mediay <- mean(y)
  subx <- x-mediax
  suby <- y-mediay
  multi1 <- subx*suby
  soma.cima <- sum(multi1)
  #baixo
  elevax <- subx^2
  elevay <- suby^2
  somax <- sum(elevax)
  somay <- sum(elevay)
  mult.somas <- somax*somay
  raiz <- sqrt(mult.somas)
  r <- soma.cima/raiz
  return(r)
}

func.cor(x = x, y = y)

func.cor2 <- function(x,y){
  cima <- sum((x-mean(x))*(y-mean(y)))
  baixo <- sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2))
  r <- cima/baixo
  return(r)
}

func.cor2(x,y)

















