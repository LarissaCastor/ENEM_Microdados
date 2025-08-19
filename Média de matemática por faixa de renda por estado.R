library(dplyr)
library(ggplot2)

dataset_enem <- microdados_enem_nordeste_1_

#converção da Q006 (renda familiar) em três faixas: Baixa, Média e Alta.
dataset_enem %>%
  mutate(renda_cat = case_when(
    Q006 %in% c(
      "Nenhuma Renda.",
      "Até R$ 1.320,00.",
      "De R$ 1.320,01 ate R$ 1.980,00."
    ) ~ "Baixa",
    
    Q006 %in% c(
      "De R$ 1.980,01 ate R$ 2.640,00.",
      "De R$ 2.640,01 ate R$ 3.300,00.",
      "De R$ 3.300,01 ate R$ 3.960,00.",
      "De R$ 3.960,01 ate R$ 5.280,00."
    ) ~ "Média",
    
    Q006 %in% c(
      "De R$ 5.280,01 ate R$ 6.600,00.",
      "De R$ 6.600,01 ate R$ 7.920,00.",
      "De R$ 7.920,01 ate R$ 9240,00.",
      "De R$ 9.240,01 ate R$ 10.560,00.",
      "De R$ 10.560,01 ate R$ 11.880,00.",
      "De R$ 11.880,01 ate R$ 13.200,00.",
      "De R$ 13.200,01 ate R$ 15.840,00.",
      "De R$ 15.840,01 ate R$19.800,00.",
      "De R$ 19.800,01 ate R$ 26.400,00.",
      "Acima de R$ 26.400,00."
    ) ~ "Alta",
    
    TRUE ~ NA_character_           #Qualquer valor fora das listas vira NA
  )) %>%
  
  #Agrupamento por estado e renda
  group_by(SG_UF_PROVA, renda_cat) %>%
  summarise(media_mt = mean(NU_NOTA_MT, na.rm = TRUE), .groups = "drop") %>%
  
  #Plotagem do gráfico de barras das médias por faixa.
  ggplot(aes(x = renda_cat, y = media_mt, fill = renda_cat)) +
  geom_col() +
  facet_wrap(~ SG_UF_PROVA) +
  labs(
    title = "Nota média de Matemática por faixa de renda e estado",
    x = "Categoria de Renda",
    y = "Nota Média em Matemática"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
