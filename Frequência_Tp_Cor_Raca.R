library(ggplot2)

enem <- microdados_enem_nordeste_1_

ggplot(enem, aes(x = as.factor(TP_COR_RACA))) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.5,
            size = 4) +
  labs(title = "Frequência da Variável TP_COR_RACA",
       x = "Código da Cor/Raça",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

