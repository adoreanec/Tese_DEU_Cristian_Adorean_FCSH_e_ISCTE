library(FactoMineR)
library(factoextra)
library(readxl)
library(dplyr)
library(writexl)

# Carregar os dados
dataset <- read_excel("C:/Users/cadorean/OneDrive - fcsh.unl.pt/Modelação estatística/Análise de clusters/Abril/BD_CA_3cities.xlsx")
df <- data.frame(dataset)

# Selecionar variáveis específicas e converter para fatores
vars_to_select <- c("btp", "ner", "gen", "idd", "sf", "imc", "spr", "freg",
                    "dmaf", "dspbt", "ccaf", "caf", "nvd", "dvr",
                    "dpvr", "mvr", "nvd", "daf_c", "daf_par_dis",
                    "dr", "mtvr_walking", "mtvr_ownmicromob", "mtvr_sharedmicromob",
                    "mtvr_pt", "mtvr_taxi", "mtvr_car")
df_selected <- df %>%
  select(all_of(vars_to_select)) %>%
  mutate(across(everything(), as.factor))

# Realizar MCA (Análise de Correspondência Múltipla)
mca_results <- MCA(df_selected, graph = FALSE)

# Calcular a contribuição total de cada variável
contributions <- as.data.frame(mca_results$var$contrib)
contributions$Variable <- rep(names(df_selected), sapply(df_selected, nlevels))
total_contributions <- aggregate(. ~ Variable, data = contributions, FUN = sum)

# Exportar as contribuições totais para Excel
write_xlsx(list(Total_Contributions = total_contributions), "MCA_results.xlsx")

# Visualização opcional no R com fviz_mca_biplot
fviz_mca_biplot(mca_results, choice = "var", label = "var", repel = TRUE,
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Obter os dados das variáveis do MCA
mca_vars <- get_mca_var(mca_results)

# Certificar-se de que 'quality_representation' é um data frame
quality_representation <- as.data.frame(mca_vars$cos2)

# Adicionar os nomes das variáveis como uma nova coluna
quality_representation$Variable <- rownames(quality_representation)

# Extrair o nome da variável global (antes do sublinhado)
quality_representation$GlobalVariable <- sub("_.*", "", quality_representation$Variable)

# Calcular o cos2 total por variável
# Somente as colunas numéricas devem ser usadas no cálculo
quality_representation$Total_cos2 <- rowSums(quality_representation[, sapply(quality_representation, is.numeric)])

# Calcular a média de cos2 por variável global
global_cos2 <- quality_representation %>%
  group_by(GlobalVariable) %>%
  summarise(Average_cos2 = mean(Total_cos2))

# Organizar as variáveis pela qualidade de representação média
ordered_quality <- global_cos2 %>%
  arrange(desc(Average_cos2))

# Exportar os resultados do cos2 médio por variável global para Excel
write_xlsx(list(Quality_Representation = ordered_quality), "MCA_cos2_results.xlsx")

