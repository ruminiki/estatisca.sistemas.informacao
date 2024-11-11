################################################
#   TESTE DE HIPÓTESES DADOS QUANTITATIVOS
#               T-STUDENT
################################################

################################################
# DATASET TITANIC
################################################

df <- read.csv("data/StudentPerformance.csv")

glimpse(df)

# Vars Exam_Score by Family_Income
# H0: Não há diferença entre as notas de estudantes
# H1: Há diferença entre as notas de estudantes

table(df$Family_Income)

High.Income <- df %>% filter(Family_Income == "High")
Low.Income  <- df %>% filter(Family_Income == "Low")

# Realiza o teste t para comparação entre notas dos dois grupos
resultado <- t.test(High.Income$Exam_Score, Low.Income$Exam_Score)

print(resultado)

############################
# Vars Attendance by School_Type
# H0: Não há diferença entre as frequências dos estudantes
# H1: Há diferença entre as frequências dos estudantes

School.Type.Private <- df %>% filter(School_Type == "Private")
School.Type.Public  <- df %>% filter(School_Type == "Public")

# Realiza o teste t para comparação entre notas dos dois grupos
resultado <- t.test(School.Type.Private$Attendance, School.Type.Public$Attendance)

print(resultado)

############################
# Vars Exam_Score by Extracurricular_Activities
# H0: Não há diferença entre as notas de estudantes
# H1: Há diferença entre as notas de estudantes

Extracurricular.Yes <- df %>% filter(Extracurricular_Activities == "Yes")
Extracurricular.No  <- df %>% filter(Extracurricular_Activities == "No")

# Realiza o teste t para comparação entre notas dos dois grupos
resultado <- t.test(Extracurricular.Yes$Exam_Score, Extracurricular.No$Exam_Score)

print(resultado)


############################
# Vars Hours_Studied by Class performance
# H0: Não há diferença entre as notas de estudantes
# H1: Há diferença entre as notas de estudantes

High.Performance <- df %>% filter(Exam_Score >= 70)
Low.Performance  <- df %>% filter(Exam_Score < 70)

# Realiza o teste t para comparação entre notas dos dois grupos
resultado <- t.test(High.Performance$Hours_Studied, Low.Performance$Hours_Studied)

print(resultado)



