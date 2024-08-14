library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(fpc)
library(klaR)
library(clValid)
library(writexl)

# Carregar os dados
dataset <- read_excel("C:/Users/cadorean/OneDrive - fcsh.unl.pt/Modelação estatística/Análise de clusters/Abril/BD_CA_3cities.xlsx")
df <- data.frame(dataset)

# Definição de cidades
cidades <- unique(df$cidd)

# Variáveis categóricas incluindo 'btp'
categorical_vars <- c("btp", "ner", "gen", "idd", "spr", "caf", "dvr", "daf_c",
                      "mtvr_walking", "mtvr_ownmicromob", "mtvr_sharedmicromob",
                      "mtvr_pt", "mtvr_car", "mtvr_taxi")

# Métodos de clusterização para testar
linkage_methods <- c("ward.D2", "single", "complete", "average")

# Loop para calcular dendrogramas e exportá-los
for (cidade in cidades) {
  df_filtered <- df %>%
    filter(cidd == cidade) %>%
    mutate(across(categorical_vars, as.factor))
  
  if (nrow(df_filtered) > 0) {
    for (method in linkage_methods) {
      # Calcular a distância de Jaccard
      jaccard_distance <- daisy(df_filtered[categorical_vars], metric = "gower")
      
      # Hierarchical Clustering
      hclust_result <- hclust(jaccard_distance, method = method)
      
      # Exportar dendrogramas
      dend <- fviz_dend(hclust_result, cex = 0.5)  # Ajustar k conforme necessário
      ggsave(paste0("Dendrogram_", cidade, "_", method, ".png"), dend)
    }
  } else {
    cat("Nenhum dado para", cidade, "\n")
  }
}

# Números de clusters para testar
num_clusters <- 2:10

# Criar dataframe para armazenar todos os resultados
all_results <- data.frame()

for (cidade in cidades) {
  for (method in linkage_methods) {
    for (k in num_clusters) {
      df_filtered <- df %>%
        filter(cidd == cidade) %>%
        mutate(across(categorical_vars, as.factor))
      
      if (nrow(df_filtered) > 0) {
        jaccard_distance <- daisy(df_filtered[categorical_vars], metric = "gower")
        
        # Hierarchical Clustering
        hclust_result <- hclust(jaccard_distance, method = method)
        clusters <- cutree(hclust_result, k = k)
        
        # Avaliação dos clusters
        silhouette_score <- mean(silhouette(clusters, jaccard_distance)[, "sil_width"])
        dunn_score <- clValid::dunn(dist(as.matrix(jaccard_distance)), clusters)
        ch_score <- calinhara(jaccard_distance, clusters)
        
        # Gravar os resultados no dataframe acumulado
        all_results <- rbind(all_results, data.frame(
          City = cidade,
          Method = method,
          NumClusters = k,
          Silhouette = silhouette_score,
          Dunn = dunn_score,
          CalinskiHarabasz = ch_score
        ))
      } else {
        cat("Nenhum dado para", cidade, " com o método ", method, " e ", k, " clusters.\n")
      }
    }
  }
}

# Exportar os resultados acumulados para um único arquivo Excel
write_xlsx(all_results, path = "All_Cluster_Results.xlsx")
