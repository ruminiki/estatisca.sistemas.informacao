library(dplyr)
library(ggplot2)
library(tidyr)
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


#############################
#+ Exemplo 2:
#+ Simulação:
#+ Média de 10, 20, 30, 50 e 100 lançamentos (possibilidades de 1 a 6)
#############################
pvalues <- data.frame()
rolls   <- data.frame()

pval.seq <- c(10, 20, 30, 50, 100, 1000, 2000, 5000)

for(i in 5:5000) {
  #calcula a média de 1,2,3,4,5,6...20....100 e 1000 lançamentos
  samples     <- sample(1:6, i, replace=TRUE)
  roll.number <- i
  s.dev       <- sd(samples)
  aux         <- data.frame(rolls = roll.number, mean=mean(samples), s.dev = s.dev)
  #gera um dataframe com as médias dos lançamentos
  rolls       <- rbind(rolls, aux)
}

#calcula o p-value para cada intervalo de tamanho de amostra
pos.ini <- 0
for(i in pval.seq){
  normal  <- shapiro.test(rolls[pos.ini:i,]$mean)
  pvalues <- rbind(pvalues, data.frame(interval = paste(pos.ini,"-",i,sep = ""), pvalue = normal$p.value))
  pos.ini <- i
}

par(mfrow = c(3, 3)) 
pos.ini <- 0
for(i in pval.seq){
  hist(rolls[pos.ini:i,]$mean, breaks=50, xlim=c(1, 6), ylim=c(0, 3), freq=FALSE, main="", xlab="")
  legend("left", paste(pos.ini,"-",i), bty="n")
  pos.ini <- i
}

#gráfico das médias e quantidade de lançamentos
rolls %>% 
  ggplot(aes(x=reorder(mean, rolls), y=mean, group=1)) + 
  geom_line() +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#  geom_hline(yintercept = 0.05, colour = "red")

rolls %>% 
  ggplot(aes(x=reorder(s.dev, rolls), y=s.dev, group=1)) + 
  geom_line() +
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
#  geom_hline(yintercept = 0.05, colour = "red")

pvalues %>% 
  ggplot(aes(x=interval, y=pvalue, group=1)) + 
  geom_line() +
  theme_minimal() +
  geom_hline(yintercept = 0.05, colour = "red")


hist(rolls[1000:2000,]$mean, breaks = 100)



#############################
#+ Exemplo 3:
#############################

rolls <- as.data.frame(replicate(50, sample(1:6, 100, replace=TRUE)))

means <- rolls %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
means <- means %>% pivot_longer(cols = starts_with("V"), values_to = "mean")

#teste normalidade uma amostra
shapiro.test(rolls$V1)
hist(rolls$V1)

#teste normalidade, média das amostras
hist(means$mean)
shapiro.test(means$mean)
#+Null Hypothesis (H₀): The data IS normally distributed
#+Alternative Hypothesis (H₁): The data is NOT normally distributed.
