library(ggplot2)


# Selecionar apenas as colunas desejadas
notas <- microdados_enem_nordeste_1_[, c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT")] %>%
  drop_na()  #acrescentado devido a ausência de algumas notas de candidatos faltantes

# Converter de formato largo para longo (necessário para ggplot)
notas_long <- notas %>%
  pivot_longer(cols = everything(),
               names_to = "Prova",
               values_to = "Nota")

# Criar boxplots comparativos
ggplot(notas_long, aes(x = Prova, y = Nota, fill = Prova)) +
  geom_boxplot() +
  labs(title = "Distribuição das Notas por Área do ENEM",
       x = "Área da Prova",
       y = "Nota") +
  theme_minimal() +
  theme(legend.position = "none")
