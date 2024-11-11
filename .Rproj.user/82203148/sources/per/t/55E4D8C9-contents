library(ggplot2)
library(dplyr)
library(viridis)
#library(hrbrthemes)

#####################################
# Carregar arquivo de dados
#####################################
df <- read.csv("data/titanic.csv")
# Metadados do dataset
# survival	Survival	0 = No, 1 = Yes
# pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
# sex	Sex	
# Age	Age in years	
# sibsp	# of siblings / spouses aboard the Titanic	
# parch	# of parents / children aboard the Titanic	
# ticket	Ticket number	
# fare	Passenger fare	
# cabin	Cabin number	
# embarked	Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton

#Visualizar dataframe
#str | glimpse | head | nrow | colnames | dim | names | table
str(df) #estrutura do dataset
glimpse(df) #visão geral, tipos de dados e primeiros registros
head(df, n=10) #exibe os n primeiros registros
nrow(df) #número de linhas do dataset
ncol(df) #número de colunas do dataset
colnames(df) # =names(df)
dim(df) #dimensões do dataset

#######################################
# Resumo estatístico do dataset
#######################################
#selecionando os atributos de interesse
df <- df %>% select(Survived, Pclass, Sex, Age, Fare, Embarked)

summary(df)

#Converte atributos numéricos qualitativos para Factor
df$Survived <- as.factor(df$Survived)
df$Pclass   <- as.factor(df$Pclass)
df$Sex      <- as.factor(df$Sex)
df$Embarked <- as.factor(df$Embarked)

summary(df$Fare)

df %>%
  na.omit() %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd)))

#tabela de frequência e contingência de atributos qualitativos
table(df$Survived)
table(df$Sex, df$Survived)


#######################################
# Análise gráfica
# Analisar a idade, tarifa, sexo e classe 
#######################################

#+ GGPLOT2, é a principal biblioteca para a geração de gráficos do R.
#+ Ela funciona criando a estrutura de coordenadas, x e y, e adicionando camadas ao gráfico.
#+ Camadas podem ser barras, linhas, colunas, pontos.
#+ O primeiro argumento é o dataset, então ggplot(data = df), cria um gráfico vazio para o dataset 'df'.
#+ O segundo argumento é o aesthetic, e servem para mapear os dados para elementos visuais.
#+ 
#+ Links úteis:
#+ https://r-graph-gallery.com/
#+ https://r-charts.com/

#Converte Survived para Sim ou Não
df$Survived <- ifelse(df$Survived == 1, "Sim", "Não")

#+ Histograma é uma representação gráfica da distribuição de frequências de uma variável quantitativa contínua. 
#+ Ele é um gráfico de barras, onde a altura de cada barra representa a frequência de ocorrência de um valor 
#+ ou intervalo de valores da variável.

# Histograma Idade
hist(df$Age,
     main = "Histograma dos Dados",
     xlab = "Idade",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     breaks = 10)  # Número de intervalos (bins)
abline(v=mean(df$Age, na.rm = T), col = "red")
abline(v=median(df$Age, na.rm = T), col = "blue")

# Dispersão dos dados
cat("Coeficiente de Variação: ", (sd(df$Age, na.rm = T) / mean(df$Age, na.rm = T))*100)

# Histograma Tarifa
hist(df$Fare,
     main = "Histograma dos Dados",
     xlab = "Tarifa",
     ylab = "Frequência",
     col = "lightblue",
     border = "black",
     breaks = 50)  # Número de intervalos (bins)
abline(v=mean(df$Fare), col = "red")
abline(v=median(df$Fare), col = "blue")

# Dispersão dos dados
cat("Coeficiente de Variação: ", (sd(df$Fare, na.rm = T) / mean(df$Fare, na.rm = T))*100)
plot(df$Fare)
plot(df$Age)
plot(df$Age, ylim=c(0,500))

#######################################
#Box-plot Sobreviventes (Survived) e Idade (Age)
#######################################
#+ Conhecido como diagrama de caixa, é uma representação gráfica da distribuição de uma variável numérica. 
#+ Ele mostra a mediana, os quartis, os valores extremos e quaisquer outliers. 
#+ Um boxplot é dividido em três partes:
#+ A caixa central representa o interquartil, que é o intervalo entre o primeiro quartil (Q1) e o terceiro quartil (Q3). 
#+ A mediana (Q2) é representada por uma linha horizontal no meio da caixa.
#+ Os bigodes são as linhas que se estendem da caixa até os valores extremos. 
#+ Os valores extremos (outliers) são definidos como os valores que estão fora de 1,5 vezes o interquartil (Q3 - Q1).

df %>%
  ggplot(aes(x = Survived, y = Age)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width = 0.3, color = 4) 


df %>%
  na.omit() %>% 
  ggplot(aes(x = Age, fill = Survived, colour=Survived)) +
  geom_density(alpha = 0.4) +
  theme_minimal()

#######################################
#Box-plot Sobreviventes (Survived) e Tarifa (Fare)
#######################################

df %>%
  ggplot(aes(x = Survived, y = Fare)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width = 0.3, color = 4) + 
  scale_y_continuous(trans="log2")
  

df %>%
  ggplot(aes(x = Fare, y = after_stat(count), group=Survived, fill = Survived)) +
  ylab("Densidade") + 
  xlab("Tarifa") + 
  geom_histogram(aes(color=Survived),alpha=0.2)


df %>%
  ggplot(aes(x = Fare, y = after_stat(count), group=Survived, fill = Survived)) +
  ylab("Densidade") + 
  xlab("Tarifa") + 
  geom_density(aes(color=Survived),alpha=0.2)


#######################################
# Sobreviventes por sexo
#######################################

df %>%
  ggplot(aes(x = Sex, fill=Survived)) +
  geom_bar(stat="count") +
  scale_fill_viridis_d() +
  theme_minimal()

####################
# Sobreviventes por classe
####################

#Histograma sobreviventes por idade e classe
df %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, y = after_stat(count), group=Survived, fill = Survived)) +
  ylab("Densidade") + 
  xlab("Idade") + 
  geom_histogram(aes(color=Survived),alpha=0.2) + 
  facet_grid(Pclass ~ .)


#Densidade sobreviventes por idade e classe
df %>%
  na.omit() %>% 
  ggplot(aes(x = Age, y = after_stat(count), group=Survived, fill = Survived)) +
  ylab("Densidade") + 
  xlab("Idade") + 
  geom_density(aes(color=Survived),alpha=0.2) + 
  facet_grid(Pclass ~ .)


#Sobreviventes por Classe
df %>%
  ggplot(aes(x = Sex, fill=Survived)) +
  geom_bar(stat="count", alpha = 0.7) +
  scale_fill_viridis_d()+
  facet_grid(Pclass ~ .)

########################
#calcula o percentual de sobreviventes
########################
df %>% 
  group_by(Survived) %>% 
  summarise(perc = n()/nrow(df)*100)

df %>% 
  filter(Sex == "male") %>% 
  group_by(Survived, Sex) %>% 
  summarise(perc = n()/sum(df$Sex == "male")*100)

df %>% 
  filter(Sex == "female") %>% 
  group_by(Survived, Sex) %>% 
  summarise(perc = n()/sum(df$Sex == "female")*100)


################################################
# EXERCÍCIO
################################################
#+ Neste exercício usar o dataset Student Performance

df <- read.csv("data/StudentPerformance.csv")

#1 Usando o Box-plot, verifique se existem diferenças nas médias finais por Gender, Distance_From_Home e Parental_Education_Level.








