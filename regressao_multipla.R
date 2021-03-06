
# use install.packages( ) function for installation
install.packages("tidyverse")
install.packages("broom")
library(tidyverse)  # data loading, manipulation and plotting
library(ggplot2) # data plotting
library(GGally)# para scaterplot matrix

auto_O<-read.table("auto.txt",h=F) # lendo o conjunto de dados (lembrar de mostrar o diret�rio)

auto <- data.frame(MPG = auto_O[,1],cilindros= auto_O[,2], deslocamento = auto_O[,3],
                   potencia = auto_O[,4],peso =auto_O[,5],aceleracao = auto_O[,6],
                   Ano_M = auto_O[,7], origem=auto_O[,8], nome_carro =auto_O[,9])

attach(auto)# fixar as vari�veis
as.numeric(potencia)

#modelo de regress�o linear simples
mod01<- lm(data = auto, MPG ~ peso) # criando um modelo com apenas uma vari�vel
summary(mod01) # pedindo o resumo do modelo
(MPG_01<- 46.3173644  - 0.0076766*peso)# modelo de regress�o linear simples


# resumo dos dados 
summary(auto)# d� o resumo de cada vari�vel (e temos problema com a vrai�vel pot�ncia)
summary(auto$peso)
summary(auto$cilindros)
summary(auto$deslocamento)
summary(auto$aceleracao)

plot(mod01$residuals, ylab="Res�duos")# res�duos com vari�ncia constante e m�dia zero para o primeiro modelo criado
# Box-plot para vermos como os dados se comportam
boxplot(auto$MPG)
boxplot(auto$peso)
boxplot(auto$cilindros)
boxplot(auto$deslocamento)
boxplot(auto$aceleracao)

### retirando dados faltantes de pot�ncia
auto_O1<-read.table("auto_semNA.txt",h=F)# sem dados faltantes
summary(auto_O1)
auto <- data.frame(MPG = auto_O1[,1],cilindros= auto_O1[,2], deslocamento = auto_O1[,3],
                   potencia = auto_O1[,4],peso =auto_O1[,5],aceleracao = auto_O1[,6],
                   Ano_M = auto_O1[,7], origem=auto_O1[,8], nome_carro =auto_O1[,9])
attach(auto)# fixar as vari�veis
head(auto)# ler o topo do conjunto de dados
length(MPG) # 392 observa��es

# reescalonando vari�veis
GPL100<- 100/MPG # transformando milhas por gal�o em gal�o por litro
peso1000<- peso/1000
deslocamento100<- deslocamento/100
potencia100<- potencia/100

auto02<- data.frame(GPL100, MPG ,cilindros, deslocamento100,
                   potencia100, peso1000, segundo0a60 = aceleracao,
                   Ano_M, origem, nome_carro) # novo data frame
attach(auto02)# fixar as vari�veis
summary(auto02) #medidas descritivas
# verificando a natureza dos dados
class(GPL100)# vari�vel resposta
class(MPG)
class(cilindros)
class(deslocamento100)# explicativa
class(potencia100) ## explicativa
class(peso1000)# explicativa
class(segundo0a60)# explicativa
## Desvio padr�o das vari�veis
sd(GPL100);sd(deslocamento100);sd(potencia100);sd(peso1000);sd(segundo0a60)

dados<-data.frame(GPL100,deslocamento100,potencia100,peso1000,segundo0a60 )# dados que ser�o usados
attach(dados)
plot(dados)# verifica a correla��o graficamente entre todas as vari�veis

### verificando os valores da correla��o
# Check correlation between variables
cor(dados)

# Nice visualization of correlations
ggcorr(dados, method = c("everything", "pearson"))# visualiza��o

# Corrgram library
library(corrgram)

# First
corrgram(dados, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Car Milage Data in PC2/PC1 Order") 
corrgram(dados, order=TRUE, upper.panel=panel.cor)# mostra com claridade

#HISTOGRAMas
ggplot(data = dados, aes(x = GPL100)) +
  geom_histogram(fill = 'blue',bins = 30)
ggplot(data = dados, aes(x = peso1000)) +
  geom_histogram(fill = 'blue',bins = 30)
ggplot(data = dados, aes(x = potencia100)) +
  geom_histogram(fill = 'blue',bins = 30)
ggplot(data = dados, aes(x = deslocamento)) +
  geom_histogram(fill = 'blue', bins = 30)
ggplot(data = dados, aes(x = segundo0a60)) +
  geom_histogram(fill = 'blue', bins = 30)

### fazendo o modelo de regress�o linear multipla
modelo=lm(GPL100 ~ peso1000 + peso1000 + potencia100 + deslocamento100 + segundo0a60 ) 
modelo # mostra como � o seu modelo
summary(modelo)
plot(modelo$residuals)
plot(fitted(modelo),residuals(modelo),xlab="Valores Ajustados",ylab="Res�duos")
abline(h=0)

## Teste de normalidade dos res�duos
shapiro.test(residuals(modelo))

library(lmtest)# tem o teste de studentized Breusch-Pagan test 
bptest(modelo) # teste de homocedasticidade 
## rejeitou a hip�tese nula de homogeneidade


## Modelo sem a vari�vel deslocamento
modelo02=lm(GPL100 ~ peso1000 + peso1000 + potencia100  + segundo0a60 ) 
modelo02 # mostra como � o seu modelo
summary(modelo02)
plot(modelo02$residuals)
plot(fitted(modelo02),residuals(modelo02),xlab="Valores Ajustados",ylab="Res�duos")
abline(h=0)

## Teste de normalidade dos res�duos
shapiro.test(residuals(modelo02))
bptest(modelo02) # teste de homocedasticidade 
## rejeitou a hip�tese nula de homogeneidade

## QQ-Plot
qqnorm(residuals(modelo02), ylab="Res�duos",main="", xlab = "Quantis Te�ricos")
qqline(residuals(modelo02))

### Transformar a vari�vel resposta para tentar normalidade.
modT=lm( log(GPL100) ~ peso1000 + peso1000 + potencia100  + segundo0a60 )
summary(modT)
plot(fitted(modT),residuals(modT),xlab="Valores Ajustados",ylab="Res�duos")
abline(h=0)
shapiro.test(residuals(modT))# normalidade
bptest(modT)# homocedastico
