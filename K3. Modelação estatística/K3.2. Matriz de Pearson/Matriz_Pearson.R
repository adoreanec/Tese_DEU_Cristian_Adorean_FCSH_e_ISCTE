# Carregar pacotes necessários
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)

# Ler dados do ficheiro Excel
dados <- read_excel('C:/Users/35193/OneDrive - fcsh.unl.pt/Artigos/Matriz de Pearson/BD_Pearson.xlsx', sheet = 'Lx') ###Substituir conforme necessário

# Selecionar as colunas de interesse
variaveis <- select(dados, Altitude, Slope, Cycle_dens, Docks_dens, Population, Real_estate,
                    Tourists_offer_dens, Residential_areas_dens, own_ebike, shared_ebike, own_escooter, shared_escooter)

# Calcular a matriz de correlação de Pearson
matriz_correlacao <- cor(variaveis, use = "complete.obs", method = "pearson")

# Inicializar a matriz de p-values com NA
p_values <- matrix(NA, ncol = ncol(variaveis), nrow = ncol(variaveis), dimnames = list(colnames(variaveis), colnames(variaveis)))

# Calcular p-values para cada par único de variáveis
for (i in 1:(ncol(variaveis)-1)) {
  for (j in (i+1):ncol(variaveis)) {
    test_result <- cor.test(variaveis[[i]], variaveis[[j]], method = "pearson")
    # Atribuir o p-value aos elementos correspondentes na matriz de p-values
    p_values[i, j] <- test_result$p.value
    p_values[j, i] <- test_result$p.value
  }
}

# Visualizar a matriz de correlação de Pearson
print("Matriz de Correlação:")
print(matriz_correlacao)
corrplot(matriz_correlacao, method = "circle")

# Visualizar os p-values
print("Matriz de p-values:")
print(p_values)

# Exportar a matriz de correlação para um ficheiro CSV
write.csv(matriz_correlacao, file = "C:/Users/35193/OneDrive - fcsh.unl.pt/matriz_correlacao_lx.csv")

# Exportar a matriz de p-values para um ficheiro CSV
write.csv(p_values, file = "C:/Users/35193/OneDrive - fcsh.unl.pt/p_values_lx.csv")

