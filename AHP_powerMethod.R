
#Gravando o output para passar para o .txt
   
sink("./output.text", append = T)


library(expm) 


# Pesos dos Criterios (------Valores a serem ajustados------------)

print("Teste Geral:") 
cat("\n")
cat("\n")

CR1_CR2 =3
CR1_CR3 =5
CR1_CR4 =1
CR1_CR5 =2
CR2_CR3 =2
CR2_CR4 =0.5
CR2_CR5 =0.5
CR3_CR4 =0.5
CR3_CR5 =0.5
CR4_CR5 =2

#Random COnsistency Index for n =5
RI = 1.12

#Montando a Matriz de Comparacao
Criteria_Comparison_Matrix = matrix(
  
  # Sequencia de elementos da Matriz
  c(1, CR1_CR2, CR1_CR3, CR1_CR4, CR1_CR5, 
    1/CR1_CR2, 1,CR2_CR3, CR2_CR4, CR2_CR5,
    1/CR1_CR3,1/CR2_CR3,1,CR3_CR4,CR3_CR5,
    1/CR1_CR4,1/CR2_CR4,1/CR3_CR4,1,CR4_CR5,
    1/CR1_CR5,1/CR2_CR5,1/CR3_CR5,1/CR4_CR5,1),
  
  # Numero de Linhas
  nrow = 5,
  
  # Numero de Colunas
  ncol = 5,		

  byrow = TRUE		
)


# Nomeando Linhas
rownames(Criteria_Comparison_Matrix) = c("Formatacao", "Padronizacao", "Atualizacao", "Rejeicao", "Aceitacao")
# Nomeando Colunas
colnames(Criteria_Comparison_Matrix) = c("Formatacao", "Padronizacao", "Atualizacao", "Rejeicao", "Aceitacao")

print("A matriz de comparacao e:")
print(Criteria_Comparison_Matrix)


#Elevando a matriz a decima
Matriz_powered = Criteria_Comparison_Matrix%^%10

cat("\n")
cat("\n")
print("Resultado da potencia��o da matriz:")
print(Matriz_powered)


cat("\n")
cat("\n")
print("Resultado da soma dos elementos da matriz:")
Soma = sum(Matriz_powered)
print(Soma)




#Obtendo os pesos dos criterios realizando a soma das linhas da Matriz de COmparacao normalizada e dividindo pela soma
#total da matriz de potencia��o
# (Vetor Prioridade)

Criteria_Weights = rowSums(Matriz_powered)/Soma
cat("\n")
cat("\n")
print("Resultado dos pesos dos criterios:")
print(Criteria_Weights)




#Pegando o maior valor do vetor de pesos

Criterio_Maior_Index = which.max(Criteria_Weights)
cat("\n")
cat("\n")
print("O criterio com maior peso e:")
print(Criteria_Weights[Criterio_Maior_Index])


#Verificando a consistencia dos pesos
Ws = Criteria_Comparison_Matrix%*%Criteria_Weights

# COnsistency Vector
Consis =((1/Criteria_Weights)%*%Ws)


#Lambda
Lambda = Consis/5


#Consistency Index

CI=(Lambda-5)/(5-1)
cat("\n")
cat("\n")
print("O indice de consistencia e:")
print(CI)

#Consistency Ratio
cat("\n")
cat("\n")
print("A razao de consistencia e")
CR = CI/RI
print(CR)


#Teste de Consistencia
cat("\n")
cat("\n")
print("Teste Geral:")

if (CR<0.1) {
  
  print("Os pesos atribuidos aos criterios sao consistentes") 
  
} else {

 print("Os pesos atribuidos aos criterios nao sao consistentes") 
  
}
  







#Subcriterios



cat("\n")
cat("\n")
print("Teste para os subcriterios de CR1:") 



#CR1 (Formatacao)

# Pesos dos Criterios (------Valores a serem ajustados------------)


CR11_CR12 =3
CR11_CR13 =7
CR11_CR14 =7
CR12_CR13 =5
CR12_CR14 =3
CR13_CR14 =2


#Random COnsistency Index for n =4
RI = 0.9

#Montando a Matriz de Comparacao
Criteria_Comparison_Matrix = matrix(
  
  # Sequencia de elementos da Matriz
  c(1, CR11_CR12, CR11_CR13, CR11_CR14, 
    1/CR11_CR12, 1,CR12_CR13, CR12_CR14,
    1/CR11_CR13,1/CR12_CR13,1,CR13_CR14,
    1/CR11_CR14,1/CR12_CR14,1/CR13_CR14,1),
  
  # Numero de Linhas
  nrow = 4,
  
  # Numero de Colunas
  ncol = 4,		
  
  byrow = TRUE		
)


# Nomeando Linhas
rownames(Criteria_Comparison_Matrix) = c("CR11", "CR12", "CR13", "CR14")
# Nomeando Colunas
colnames(Criteria_Comparison_Matrix) = c("CR11", "CR12", "CR13", "CR14")
cat("\n")
cat("\n")
print("A matriz de comparacao para os subcriterios de CR1 e:")
print(Criteria_Comparison_Matrix)


#Elevando a matriz � d�cima
Matriz_powered = Criteria_Comparison_Matrix%^%10

cat("\n")
cat("\n")
print("Resultado da potencia��o da matriz:")
print(Matriz_powered)


cat("\n")
cat("\n")
print("Resultado da soma dos elementos da matriz:")
Soma = sum(Matriz_powered)
print(Soma)




#Obtendo os pesos dos criterios realizando a soma das linhas da Matriz de COmparacao normalizada e dividindo pela soma
#total da matriz de potencia��o
# (Vetor Prioridade)

Criteria_Weights = rowSums(Matriz_powered)/Soma
cat("\n")
cat("\n")
print("Resultado dos pesos dos criterios:")
print(Criteria_Weights)




#Pegando o maior valor do vetor de pesos

Criterio_Maior_Index = which.max(Criteria_Weights)
cat("\n")
cat("\n")
print("O criterio com maior peso e:")
print(Criteria_Weights[Criterio_Maior_Index])



#Verificando a consistencia dos pesos
Ws = Criteria_Comparison_Matrix%*%Criteria_Weights

# COnsistency Vector
Consis =((1/Criteria_Weights)%*%Ws)

#Lambda
Lambda = Consis/4


#Consistency Index

CI=(Lambda-4)/(4-1)
cat("\n")
cat("\n")
print("O indice de consistencia e:")
print(CI)

#Consistency Ratio
cat("\n")
cat("\n")
print("A razao de consistencia e")
CR = CI/RI
print(CR)


#Teste de Consistencia
cat("\n")
cat("\n")
print("Teste de Consistencia Subcriterios CR1:")

if (CR<0.1) {
  
  print("Os pesos atribuidos aos criterios sao consistentes") 
  
} else {
  
  print("Os pesos atribuidos aos criterios nao sao consistentes") 
  
}






cat("\n")
cat("\n")
print("Teste para os subcriterios de CR2:") 

#CR2 (Padronizacao)

# Pesos dos Criterios (------Valores a serem ajustados------------)

CR21_CR22 =5
CR21_CR23 =3
CR21_CR24 =7
CR21_CR25 =9
CR22_CR23 =2
CR22_CR24 =3
CR22_CR25 =7
CR23_CR24 =3
CR23_CR25 =6
CR24_CR25 =5

#Random COnsistency Index for n =5
RI = 1.12

#Montando a Matriz de Comparacao
Criteria_Comparison_Matrix = matrix(
  
  # Sequencia de elementos da Matriz
  c(1, CR21_CR22, CR21_CR23, CR21_CR24, CR21_CR25, 
    1/CR21_CR22, 1,CR22_CR23, CR22_CR24, CR22_CR25,
    1/CR21_CR23,1/CR22_CR23,1,CR23_CR24,CR23_CR25,
    1/CR21_CR24,1/CR22_CR24,1/CR23_CR24,1,CR24_CR25,
    1/CR21_CR25,1/CR22_CR25,1/CR23_CR25,1/CR24_CR25,1),
  
  # Numero de Linhas
  nrow = 5,
  
  # Numero de Colunas
  ncol = 5,		
  
  byrow = TRUE		
)


# Nomeando Linhas
rownames(Criteria_Comparison_Matrix) = c("CR21", "CR22", "CR23", "CR24", "CR25")
# Nomeando Colunas
colnames(Criteria_Comparison_Matrix) = c("CR21", "CR22", "CR23", "CR24", "CR25")

cat("\n")
cat("\n")
print("A matriz de comparacao para os subcriterios de CR2 e:")
print(Criteria_Comparison_Matrix)


#Elevando a matriz � d�cima
Matriz_powered = Criteria_Comparison_Matrix%^%10

cat("\n")
cat("\n")
print("Resultado da potencia��o da matriz:")
print(Matriz_powered)


cat("\n")
cat("\n")
print("Resultado da soma dos elementos da matriz:")
Soma = sum(Matriz_powered)
print(Soma)




#Obtendo os pesos dos criterios realizando a soma das linhas da Matriz de COmparacao normalizada e dividindo pela soma
#total da matriz de potencia��o
# (Vetor Prioridade)

Criteria_Weights = rowSums(Matriz_powered)/Soma
cat("\n")
cat("\n")
print("Resultado dos pesos dos criterios:")
print(Criteria_Weights)




#Pegando o maior valor do vetor de pesos

Criterio_Maior_Index = which.max(Criteria_Weights)
cat("\n")
cat("\n")
print("O criterio com maior peso e:")
print(Criteria_Weights[Criterio_Maior_Index])


#Verificando a consistencia dos pesos
Ws = Criteria_Comparison_Matrix%*%Criteria_Weights

# COnsistency Vector
Consis =((1/Criteria_Weights)%*%Ws)


#Lambda
Lambda = Consis/5


#Consistency Index

CI=(Lambda-5)/(5-1)
cat("\n")
cat("\n")
print("O indice de consistencia e:")
print(CI)

#Consistency Ratio
cat("\n")
cat("\n")
print("A razao de consistencia e")
CR = CI/RI
print(CR)


#Teste de Consistencia
cat("\n")
cat("\n")
print("Teste de Consistencia Subcriterios CR2:")

if (CR<0.1) {
  
  print("Os pesos atribuidos aos criterios sao consistentes") 
  
} else {
  
  print("Os pesos atribuidos aos criterios nao sao consistentes") 
  
}







cat("\n")
cat("\n")
print("Teste para os subcriterios de CR4:") 

#CR4 (Rejeicao)



# Pesos dos Criterios (------Valores a serem ajustados------------)


CR31_CR32 =3
CR31_CR33 =7
CR32_CR33 =5            


#Random COnsistency Index for n =3
RI = 0.58

#Montando a Matriz de Comparacao
Criteria_Comparison_Matrix = matrix(
  
  # Sequencia de elementos da Matriz
  c(1,CR31_CR32, CR31_CR33, 
    1/CR31_CR32, 1, CR32_CR33,
    1/CR31_CR33,1/CR32_CR33,1),
  
  # Numero de Linhas
  nrow = 3,
  
  # Numero de Colunas
  ncol = 3,		
  
  byrow = TRUE		
)


# Nomeando Linhas
rownames(Criteria_Comparison_Matrix) = c("CR31", "CR32", "CR33")
# Nomeando Colunas
colnames(Criteria_Comparison_Matrix) = c("CR31", "CR32", "CR33")

cat("\n")
cat("\n")
print("A matriz de comparacao para os subcriterios de CR4 e:")
print(Criteria_Comparison_Matrix)


#Elevando a matriz � d�cima
Matriz_powered = Criteria_Comparison_Matrix%^%10

cat("\n")
cat("\n")
print("Resultado da potencia��o da matriz:")
print(Matriz_powered)


cat("\n")
cat("\n")
print("Resultado da soma dos elementos da matriz:")
Soma = sum(Matriz_powered)
print(Soma)




#Obtendo os pesos dos criterios realizando a soma das linhas da Matriz de COmparacao normalizada e dividindo pela soma
#total da matriz de potencia��o
# (Vetor Prioridade)

Criteria_Weights = rowSums(Matriz_powered)/Soma
cat("\n")
cat("\n")
print("Resultado dos pesos dos criterios:")
print(Criteria_Weights)




#Pegando o maior valor do vetor de pesos

Criterio_Maior_Index = which.max(Criteria_Weights)
cat("\n")
cat("\n")
print("O criterio com maior peso e:")
print(Criteria_Weights[Criterio_Maior_Index])

#Verificando a consistencia dos pesos
Ws = Criteria_Comparison_Matrix%*%Criteria_Weights

# COnsistency Vector
Consis =((1/Criteria_Weights)%*%Ws)
 

#Lambda
Lambda = Consis/3


#Consistency Index

CI=(Lambda-3)/(3-1)
cat("\n")
cat("\n")
print("O indice de consistencia e:")
print(CI)

#Consistency Ratio
cat("\n")
cat("\n")
print("A razao de consistencia e")
CR = CI/RI
print(CR)


#Teste de Consistencia
cat("\n")
cat("\n")
print("Teste de Consistencia Subcriterios CR4:")



if (CR<0.1) {
  
  print("Os pesos atribuidos aos criterios sao consistentes") 
  
} else {
  
  print("Os pesos atribuidos aos criterios nao sao consistentes") 
  
}


sink() #Parando a gravacao




