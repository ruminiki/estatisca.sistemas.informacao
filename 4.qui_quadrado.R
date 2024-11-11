################################################
#     TESTE DE HIPÓTESES DADOS QUALITATIVOS
#               QUI-QUADRADO
################################################

################################################
# DATASET TITANIC
################################################

df <- read.csv("data/titanic.csv")

glimpse(df)

################################################
# ASSOCIAÇÃO ENTRE Survived e Pclass
################################################

#converte variável categórica inteira, para fator
df$Survived <- ifelse(df$Survived == 1, "Survived", "Died")
df$Pclass   <- as.factor(df$Pclass)

# Realiza o teste qui-quadrado
teste <- chisq.test(df$Survived, df$Pclass)

# Exibe o resultado do teste
print(teste)
print(ifelse(teste$p.value <= 0.05, "Bingo! Existe associação entre as variáveis!", "Oops! Não existe associação entre as variáveis!"))

# Analisa as proporções das variáveis
proporcao <- prop.table(table(df$Survived, df$Pclass), margin = 2)
print(proporcao)

#chart
as.data.frame(proporcao) %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("") +
  scale_fill_brewer(direction = 1, palette = "Pastel1") +
  facet_grid(~Var2)


################################################
# ASSOCIAÇÃO ENTRE Survived e Sex
################################################

# Realiza o teste qui-quadrado
teste <- chisq.test(df$Survived, df$Sex)

# Exibe o resultado do teste
print(teste)
print(ifelse(teste$p.value <= 0.05, "Bingo! Existe associação entre as variáveis!", "Oops! Não existe associação entre as variáveis!"))

# Analisa as proporções das variáveis
proporcao <- prop.table(table(df$Survived, df$Sex), margin = 2)
print(proporcao)

#chart
as.data.frame(proporcao) %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("") +
  scale_fill_brewer(direction = 1, palette = "Pastel1") +
  facet_grid(~Var2)


################################################
# ASSOCIAÇÃO ENTRE Sex e Pclass
################################################
# Desenvolva o teste de hipóteses para as variáveis Sex e Pclass.
# Analise o resultado





################################################
# DATASET HEALTHCARE
################################################

df <- read.csv("data/healthcare_dataset.csv")

glimpse(df)

################################################
# ASSOCIAÇÃO ENTRE Gender e Admission.Type
################################################

# Realiza o teste qui-quadrado
teste <- chisq.test(df$Gender, df$Admission.Type)

# Exibe o resultado do teste
print(teste)
print(ifelse(teste$p.value <= 0.05, "Bingo! Existe associação entre as variáveis!", "Oops! Não existe associação entre as variáveis!"))

# Analisa as proporções das variáveis
proporcao <- prop.table(table(df$Gender, df$Admission.Type), margin = 2)
print(proporcao)

#chart
as.data.frame(proporcao) %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("") +
  scale_fill_brewer(direction = 1, palette = "Pastel2") +
  facet_grid(~Var2)


################################################
# ASSOCIAÇÃO ENTRE Age.Class e Admission.Type
################################################

# Cria uma variável categórica a partir de intervalos de idade
df$Age.Class <- cut(df$Age,
                     breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("Under 20", "20-40", "40-60", "60-80", "80-100"),
                     right = FALSE)

# Realiza o teste qui-quadrado
teste <- chisq.test(df$Age.Class, df$Admission.Type)

# Exibe o resultado do teste
print(teste)
print(ifelse(teste$p.value <= 0.05, "Bingo! Existe associação entre as variáveis!", "Oops! Não existe associação entre as variáveis!"))

# Analisa as proporções das variáveis
proporcao <- prop.table(table(df$Age.Class, df$Admission.Type), margin = 2)
print(proporcao)

#chart
as.data.frame(proporcao) %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("") +
  scale_fill_brewer(direction = 1, palette = "Pastel1") +
  facet_grid(Var2 ~ .)


