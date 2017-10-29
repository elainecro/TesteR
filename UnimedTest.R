start.time <- Sys.time()


library(xlsx)
  
#Realizando a leitura do dataset
dataset <- read.xlsx("DataSet.xlsx",sheetIndex=1,header=TRUE, encoding="UTF-8")

#Se houverem dados NA, realizo a limpeza
dataset <- na.omit(dataset)

#class(dataset)
#head(dataset)

#Iniciando um cálculo de idade dos clientes
dataAtual <- format(Sys.time(), "%Y-%m-%d")
dataset$IDADE <- as.integer((as.Date(dataAtual) - as.Date(format(as.Date(dataset$DATA_NASCIMENTO), "%Y-%m-%d")))/365)

#Criando uma coluna com total dos Valores de 01 a 04
dataset$VLR_TOTAL <- (dataset$VALOR_01 + dataset$VALOR_02 + dataset$VALOR_03 + dataset$VALOR_04)

#Sumarizando os dados para ter uma noção
summary(dataset)
boxplot(dataset$VLR_TOTAL, col='blue')
quartil1 <- 914.3
media <- 29948.1
mediana <- 2714.3
quartil3 <- 18083.4
abline(h=mediana, col="green")
abline(h=quartil1, col="red")
abline(h=quartil3, col="red")
#guardando as informações de quartis para utilização posterior


#avaliando se temos uma distribuição normal nas colunas de valores
shapiro.test(dataset$VALOR_01 + dataset$VALOR_02 + dataset$VALOR_03 + dataset$VALOR_04 + dataset$VLR_TOTAL)
#Como temos um p-value menor que 0.05 então sabemos que não temos uma distribuição normal.

# Teste de hipótese
#Vamos avaliar se a idade possui relação com o valor total
wilcox.test(dataset$IDADE, dataset$VLR_TOTAL)
wilcox.test(as.integer(dataset$GENERO), dataset$VLR_TOTAL)


#Vendo se existe alguma relação entre idade e valores total através de regressão linear
linearModel <- lm(IDADE ~ VALOR_01 + VALOR_02 + VALOR_03 + VALOR_04 + VLR_TOTAL, dataset)
linearModel

linearM <- lm(IDADE ~ VLR_TOTAL, dataset)
linearM
summary(linearModel)
#através da sumarização do modelo linear vemos que os seus p-values são baixos mas somente o VALOR_02 parece realmente ter uma relação com as idades

cor1 <- cor(dataset$IDADE, dataset$VALOR_01)
cor1
cor2 <- cor(dataset$IDADE, dataset$VALOR_02)
cor2
cor3 <- cor(dataset$IDADE, dataset$VALOR_03)
cor3
cor4 <- cor(dataset$IDADE, dataset$VALOR_04)
cor4

#mas avaliando as correlações separadas vemos que existe correlação muito fraca entre os dados


#plotando em um gráfico de dispersão os dados de VALORES x IDADE
plot(dataset$IDADE, dataset$VLR_TOTAL)
abline(linearModel, col='blue', lwd=4)
#dessa forma é possível ver que temos outliers a partir de 1000000 (realmente sabemos como é difícil ser um milionário)

#vamos remover os outliers para melhor visualizar os dados
dataset1 <- subset(dataset, VLR_TOTAL <= quartil3)
summary(dataset1)
q1 <- 714.3
mediana1 <- 1473.7
q3 <- 3885.7
hist(dataset1$VLR_TOTAL)
abline(v=q1, col="red")
abline(v=q3, col="red")
abline(v=mediana1, col="blue" )


boxplot(dataset1$VLR_TOTAL, col="green")
abline(h=mediana1, col="purple", lwd=3)


# Gênero Feminino
datasetF <- subset(dataset, GENERO == "F")
summary(datasetF)

q1F <- 800
medianaF <- 1885.7
q3F <- 11828.6

plot(datasetF$IDADE, datasetF$VLR_TOTAL)
abline(h=q1F, col="blue")
abline(h=medianaF, col="red")
abline(h=q3F, col="blue")


hist(datasetF$VLR_TOTAL)
abline(v=q1F, col="blue")
abline(v=medianaF, col="red")
abline(v=q3F, col="blue")


# Gênero Masculino
datasetM <- subset(dataset, GENERO == "M")
summary(datasetM)

q1M <- 1000
medianaM <- 2857
q3M <- 21143

plot(datasetM$IDADE, datasetM$VLR_TOTAL)
abline(h=q1M, col="blue")
abline(h=medianaM, col="red")
abline(h=q3M, col="blue")

hist(datasetM$VLR_TOTAL)
abline(v=q1M, col="blue")
abline(v=medianaM, col="red")
abline(v=q3M, col="blue")


## Tempo de execução
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
