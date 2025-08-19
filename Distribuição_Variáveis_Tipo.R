library(dplyr)
library(writexl)
library(ggplot2)


#Avaliação dos tipos de dados / gerar função para descrever resultados
enem <- microdados_enem_nordeste_1_
str(microdados_enem_nordeste_1_)

classificar_variavel <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return("Quantitativa")
  } else if (is.character(x) || is.factor(x)) {
    return("Qualitativa")
  } else {
    return("Outro tipo")
  }
}

tabela_variaveis <- data.frame(
  Variavel = names(enem),
  Tipo = sapply(enem, classificar_variavel)
)

write_xlsx(tabela_variaveis, "tabela_variaveis.xlsx")
#Contando a quantidade de variáveis quantitativas e qualitativas
contagem_tipos <- table(tabela_variaveis$Tipo)
contagem_tipos_variaveis <- as.data.frame(contagem_tipos)
colnames(contagem_tipos_variaveis) <- c("Tipo", "Quantidade")
write_xlsx(contagem_tipos_variaveis, "contagem_tipos_variaveis.xlsx")

#criando um gráfico de pizza para visualização do percentual de variáveis quantitativas e qualitativas
contagem_tipos_variaveis$Percentual <- round(100 * contagem_tipos_variaveis$Quantidade / sum(contagem_tipos_variaveis$Quantidade), 1)
contagem_tipos_variaveis$Label <- paste0 (contagem_tipos_variaveis$Percentual, " %")
ggplot(contagem_tipos_variaveis, aes(x = "", y = Quantidade, fill = Tipo)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 6) +
  ggtitle("Distribuição de Variáveis por Tipo") +
  scale_fill_manual(values = c("cyan3", "coral"))
