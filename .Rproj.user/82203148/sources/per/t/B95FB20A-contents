library(ggplot2)
library(tidyr)
#notas
notas <- c(8.02,8.02,7.91,8.08,7.42,8.62,8.60,8.76,
           8.38,7.91,8.38,8.10,7.92,8.28,7.23,8.66,
           8.40,7.38,7.60,7.98,7.14,8.84,7.98,8.86,
           7.3,7.93,7.44,8.03,7.93,7.66,7.88,7.36,
           7.78,8.33,8.84,8.60)

#teste de normalidade (se p < 0.05 não normal)
shapiro.test(notas)

hist(scale(notas))

mean(notas)
sd(notas)

norm_notas <- rnorm(100000, mean=mean(notas), sd=sd(notas))

hist(norm_notas, probability=TRUE)

#probabilidade de nota acima ou menor que 8.5 (lower.tail=T/F)
p1 <- pnorm(8.5, mean=8.04, sd=0.49, lower.tail=T)
p2 <- pnorm(8.5, mean=8.04, sd=0.49, lower.tail=F)

#validando
pdireita <- sum(norm_notas > 8.5) / length(norm_notas)
p1 + pdireita

#tenho a probabilidade, quero saber o valor
qnorm(0.85, mean=8.04, sd=0.49)

#################################
# DEMONSTRAÇÃO DA VARIÂNCIA
################################
d1 <- rnorm(1000, mean=3, sd=1.58)
d2 <- rnorm(1000, mean=3, sd=1.58)

#variancia
plot(d1, col="gray", bg="red", lwd=2)
abline(h=mean(d1), col=c("red"), lty=3, lwd=2)
abline(h=(mean(d1) - var(d1)), col=c("blue"), lty=3, lwd=2)
abline(h=(mean(d1) + var(d1)), col=c("blue"), lty=3, lwd=2)

#################################
# DEMONSTRAÇÃO COEFICIENTE DE VARIAÇÃO
################################
#CV >30%
d1 <- rnorm(1000, mean = 3, sd = 1.58)
(sd(d1)/mean(d1))*100
plot(d1)
hist(d1)
#CV 10 - 20%
d2 <- rnorm(1000, mean = 3, sd = 0.6)
(sd(d2)/mean(d2))*100
plot(d2, ylim=c(-2,8))
hist(d2, xlim=c(-2,8))
#CV 10%
d3 <- rnorm(1000, mean = 3, sd = 0.2)
(sd(d3)/mean(d3))*100
plot(d3, ylim=c(-2,8))
hist(d3, xlim=c(-2,8))

plot(d1)
sd(d1) / mean(d1) * 100

mean(d1)
sd(d1)

#################################
# PROBABILIDADE DISTRIBUIÇÃO NORMAL
################################
#utilizando o graphics (pacote nativo)
x = seq(0,20, by =0.01)
media = 10
var   = 4

# função de densidade
dx  = dnorm(x, mean = media, sd = sqrt(var))
#plot da função de densidade
plot(x, dx, type = "l", col = "blue", 
     ylab = bquote(f[X]~(x)~~"densidades"), 
     xlab = "x", 
     main =  bquote("N"~(mu==.(media)~","~sigma^2 == .(var))))


# poligono para representar a área sob a curva
a = 8 # Limite inferior
b = 12 # Limite superior
da = dnorm(a,mean = media, sd = sqrt(var))   # Densidade no Limite inferior crítico
db = dnorm(b,mean = media, sd = sqrt(var))   # Densidade no Limite superior crítico

polygon(x = c(a, a  , x[a<x & x<b], b), # X = conjunto dos valores de a até b
        y = c(0, da , dx[a<x & x<b], 0),          # Y = conjunto das Density de a até b
        col = "red",
        density = c(20),
        angle = c(-45))

# adicionado valores no eixo x
Map(axis, side=1, at = round(c(a,b),2),
    col.axis = c("red" , "red"),
    col.ticks = c("red", "red"),
    lwd=0, las=1,
    lwd.ticks = 2)

# cálculo da probabilidade de uma variável aleatória normal utilizando a função do R "pnorm"
# pnorm é a função cumulativa de probabilidade de uma distribuição normal
prob = pnorm(b, mean = media, sd = sqrt(var)) - pnorm(a, mean = media, sd = sqrt(var)) 

# legendas para o gráfico
legenda <- list( bquote( "Probabilidade =" ~ .(round(prob,4)) )  )
mtext(side = 3, do.call(expression, legenda), line=-2:-2, adj=1, col=c("red"))

####################################################

df <- read.csv("data/notas.csv")
prova <- df$nota

hist(prova)
boxplot(prova)

#teste de normalidade (se p < 0.05 não normal)
#H0: Os dados seguem uma distribuição normal (p-valor > 0.05)
#H1: Os dados não seguem uma distribuição normal (p-valor < 0.05)
shapiro.test(prova)

##################################
#teorema do limite central
#a medida que a amostra aumenta, tende a uma distribuição normal
amostra <- rnorm(1000, mean = mean(prova), sd = sd(prova))
hist(amostra)
shapiro.test(amostra)
##################################

#remove outliers
prova <- prova[prova > 0]
hist(prova)
boxplot(prova)

#novo teste de normalidade
shapiro.test(prova)

media <- mean(prova)
dp    <- sd(prova)

hist(prova)
abline(v=media, col=c("red"), lty=3, lwd=2)

#para facilitar a interpretação ajusta a média para 70
prova <- rnorm(500, mean = media, sd = dp)
hist(prova)
abline(v=media, col=c("red"), lty=3, lwd=2)

#probabilidade de nota maior que 81
pnorm(81, mean=media, sd=dp, lower.tail=F)

#probabilidade de nota entre 60 e 80
pnorm(60, mean=70, sd=10, lower.tail=F) - pnorm(60, mean=70, sd=10, lower.tail=T)

#probabilidade de nota acima ou menor que 8.5 (lower.tail=T/F)
pnorm(80, mean=70, sd=10, lower.tail=F)

################################
# Usando o valor de Z obtem-se 
# a probabilidade direta
################################
pnorm(-2)

###############################
#=================================================
# Simulation of central limit theorem
#=================================================
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
#---------------------------------------------------------------------------
# One uniform random variable simulated 10000 times
#---------------------------------------------------------------------------
size=1 # No. of random variables in sum.
repeats=10000 # No. of values to simulate for
# histogram.
v=runif(size*repeats) # Vector of uniform random
# variables.
w=matrix(v,size, repeats) # Enter v into a matrix
# sizeXrepeats).
y=colSums(w) # Sum the columns.
hist(y,freq=FALSE,ann=FALSE) # Histogram.
title("size 1")
#------------------------------------------------------------------------------
#Sum of 2 uniform random variables simulated 10000 times
#------------------------------------------------------------------------------
size=2 # No. of random variables in sum.
repeats=20000 # No. of values to simulate for
# histogram.
v=runif(repeats) # Vector of uniform random
# variables.
w=matrix(v,size, repeats) # Enter v into a matrix
# sizeXrepeats).
y=colSums(w) # Sum the columns.
hist(y,freq=FALSE,ann=FALSE) # Histogram.
title("size 2")
#------------------------------------------------------------------------------
#Sum of 4 uniform random variables simulated 10000 times
#------------------------------------------------------------------------------
size=4 # No. of random variables in sum.
repeats=40000 # No. of values to simulate for
# histogram.
v=runif(repeats) # Vector of uniform random
# variables.
w=matrix(v,size, repeats) # Enter v into a matrix 
4
# sizeXrepeats).
y=colSums(w) # Sum the columns.
hist(y,freq=FALSE,ann=FALSE) # Histogram.
title("size 4")
#-------------------------------------------------------------------------------
#Sum of 20 uniform random variables simulated 10000 times
#------------------------------------------------------------------------------
size=20 # No. of random variables in sum.
repeats=200000 # No. of values to simulate for
# histogram.
v=runif(repeats) # Vector of uniform random
# variables.
w=matrix(v,size, repeats) # Enter v into a matrix
# sizeXrepeats).
y=colSums(w) # Sum the columns.
hist(y,freq=FALSE,ann=FALSE) # Histogram.
title("size 20") 


##############################################
atual <- c(110, 120, 115, 125, 130, 118, 121, 124, 122, 117, 119, 123, 126, 128, 113, 127, 129, 116, 114, 111, 125, 123, 120, 118, 119, 122, 124, 126, 127, 130)
novo  <- c(100, 105, 98, 102, 104, 101, 99, 97, 96, 100, 103, 99, 98, 101, 102, 95, 97, 96, 100, 103, 99, 97, 101, 98, 100, 102, 99, 103, 100, 104)

shapiro.test(b)

layout(matrix(c(1,2),2,2,byrow=TRUE))
hist(a)
hist(b)

t.test(a,b)


#########TESTE Z##############################
install.packages("BSDA")
library(BSDA)

mu <- mean(atual)
sigma <- sd(atual)

# Usando a função z.test do pacote BSDA
result <- z.test(novo, mu = mu, sigma.x = sigma)

# Exibir os resultados
result



#####################################################
# Lei dos Grandes Números e Teoria do Limite Central
#####################################################

#+ Exemplo 1:
#+ Média de 1 a 1000 lançamentos de dados

set.seed(1)

dice.seq <- c(1:6, 20, 100, 1000)

rolls <- data.frame()

#Calcula a média de n lançamentos aleatórios de um dado por n repetições
for(i in dice.seq) {
  print(i)
  #calcula a média de 1,2,3,4,5,6...20....100 e 1000 lançamentos
  aux <- as.data.frame(replicate(100, mean(sample(1:6, i, replace=TRUE))))
  colnames(aux) <- paste("V",i, sep = "")
  #gera um dataframe com as médias dos lançamentos
  if (length(rolls) <= 0) rolls <- aux
  else rolls <- cbind(rolls, aux)
}

#ploting charts
par(mfrow = c(3, 3)) 

for(i in seq(1:9)) {
  hist(rolls[,i], breaks=50, xlim=c(1, 6), ylim=c(0, 3), freq=FALSE, main="", xlab="")
  legend("topleft", paste(colnames(rolls[i]), "dice"), bty="n")
}

pvalues <- data.frame()
#teste de normalidade
for(i in seq(1:9)) {
  #+Null Hypothesis (H₀): The data IS normally distributed
  #+Alternative Hypothesis (H₁): The data is NOT normally distributed.
  normal <- shapiro.test(rolls[,i])
  pvalues <- rbind(pvalues, c(colnames(rolls[i]), as.numeric(normal$p.value), (normal$p.value >= 0.05)))
}
colnames(pvalues) <- c("seq","pvalue","is.normal")
pvalues$pvalue <- round(as.numeric(pvalues$pvalue), 4)

pvalues %>% 
  ggplot(aes(x=reorder(seq, pvalue), y=pvalue, group=1)) + 
  geom_line() +
  geom_text(aes(label = pvalue)) +
  theme_minimal() +
  scale_y_continuous(label = scales::percent)
#  geom_hline(yintercept = 0.05, colour = "red")


#+ Exemplo 2:
#+ Simulação:
#+ 10 amostras de 10  lançamentos (possibilidades de 1 a 6)
#+ 10 amostras de 20  lançamentos
#+ 10 amostras de 30  lançamentos 
#+ 10 amostras de 50  lançamentos 
#+ 10 amostras de 100 lançamentos 
pvalues <- data.frame()

rolls <- as.data.frame(replicate(10, sample(1:6, 1000, replace=TRUE)))        
means <- rolls %>% summarise_all(mean, trim = 1)
means <- means %>% pivot_longer(cols=c(colnames(means)), values_to='mean') 
hist(means$mean)

normal  <- shapiro.test(means$mean)
pvalues <- rbind(pvalues, c("A50", as.numeric(normal$p.value), (normal$p.value >= 0.05)))

rolls <- data.frame()
for (i in 1:100) {
  if( i == 1 )
    rolls <- cbind(floor(runif(100, min=1, max=7)))
  else
    rolls <- cbind(rolls, floor(runif(100, min=1, max=7)))
}

rolls %>% mean()

rollsdf <- as.data.frame(rolls)

rollsdf %>% ggplot(aes(x=V4))+
  geom_bar(stat="count")

#+Null Hypothesis (H₀): The data IS normally distributed
#+Alternative Hypothesis (H₁): The data is NOT normally distributed.
shapiro.test(rollsdf$V2)

# Teoria do limite central
#a média das amostras se aproxima de uma distribuição normal
rollsdf %>% summarise_all(mean())
means <- as.data.frame(colMeans(rollsdf))
colnames(means) <- "mean"
hist(means$mean)

#teste de normalidade das médias dos lançamentos
shapiro.test(means$mean)


qnorm(0.99)
format(pnorm(-6.3)* 100,scientific = F) 

(24.85-25.08)/0.05
(25.15-25.08)/0.05
1-pnorm(-1.5)-pnorm(-0.84)
(1-pnorm(0.2))*100

qnorm(0.85)





