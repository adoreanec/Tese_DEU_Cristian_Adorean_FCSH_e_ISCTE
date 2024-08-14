##### Análise descritiva de questões Likert #####

# Passo 1: Carregar os pacotes que serão usados
if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, likert, table1, readxl, flextable, RColorBrewer)

# Carregar o banco de dados
dados <- readxl::read_xlsx("Banco_Likert.xlsx")

# View(dados)                                 # Visualização dos dados em janela separada
glimpse(dados)                              # Visualizção de um resumo dos dados

# Passo 3: Ajustar as variáveis
dados$`Frequência de utilização` <- factor(dados$`Frequência de utilização`,
                                     levels = c("Diariamente",
                                                "Semanalmente",
                                                "Mensalmente",
                                                "Esporadicamente"),
                                     ordered = T)

dados[,1:17] <- lapply(dados[,1:17], factor, levels = 1:5,
                       labels = c("Totalmente insatisfeitos/as", "Insatisfeitos/as",
                                  "Indiferentes",
                                  "Satisfeitos/as", "Totalmente satisfeitos/as"),
                       ordered = T)

## Descritiva geral
table1::table1(~ ., data = dados, overall = "n (%)", decimal.mark = ",")

### Mudar o nome das questões
nomes <- readxl::read_xlsx("Banco_Likert.xlsx", sheet = 2)
colnames(dados)[1:17] <- nomes$Nome[1:17]

table1::table1(~ ., data = dados, overall = "n (%)", decimal.mark = ",")

## Descritiva por grupo
table1::table1(~ . | `Frequência de utilização`, data = dados,
               overall = "Total", decimal.mark = ",")

## Gráfico geral
dados_graf <- likert::likert(as.data.frame(dados[1:17]))

likert::likert.bar.plot(dados_graf) +
  labs(y = "Frequência (%)") +
  guides(fill = guide_legend(title = "Níveis de satisfação")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("likert_Satisfaction_lisboa_own_ebike.png", width = 13, height = 7)
