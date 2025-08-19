install.packages("sf", repos = "https://cran.rstudio.com/")



library(dplyr)
library(tidyr)
library(ggplot2)
library(geobr)
library(sf)


ENEM_2023 <- microdados_enem_nordeste_1_

## Médias das disciplinas por estados do Nordeste
### Calcular as médias de cada disciplina para cada estado
medias_por_estado <- ENEM_2023 %>%
  group_by(CO_UF_PROVA) %>%
  summarise(media_cn = mean(NU_NOTA_CN, na.rm = TRUE),
            media_ch = mean(NU_NOTA_CH, na.rm = TRUE),
            media_lc = mean(NU_NOTA_LC, na.rm = TRUE),
            media_mt = mean(NU_NOTA_MT, na.rm = TRUE),
            media_re = mean(NU_NOTA_REDACAO, na.rm = TRUE))
### Visualizar médias por estados NE
print(medias_por_estado)
### Pivotar tabela - arrumar para exibir no gráfico
medias_tabela <- medias_por_estado %>%
  pivot_longer(cols = c(media_cn,media_ch,media_lc,media_mt,media_re),
               names_to = "disciplina",
               values_to = "medias")
### Obter os dados geográficos dos estados
estados <- read_state(code_state = "all", year = 2020)
### Filtrar estados do nordeste
estados_NE <- estados %>%
  filter(code_region == 2)
### Renomear coluna para facilitar união
estados_NE <- estados_NE %>%
  rename(CO_UF_PROVA = code_state)
### Unir os dados das médias com os dados geográficos dos estados
medias_geodata <- estados_NE %>%
  inner_join(medias_tabela, by = "CO_UF_PROVA")
### Renomear valores para exibição no gráfico
medias_geodata$disciplina[medias_geodata$disciplina == "media_cn"] <- "Ciências da Natureza"
medias_geodata$disciplina[medias_geodata$disciplina == "media_ch"] <- "Ciências Humanas"
medias_geodata$disciplina[medias_geodata$disciplina == "media_lc"] <- "Língua Portuguesa"
medias_geodata$disciplina[medias_geodata$disciplina == "media_mt"] <- "Matemática"
medias_geodata$disciplina[medias_geodata$disciplina == "media_re"] <- "Redação"

# Gráfico mapa médias disciplinas estados do nordeste
### Criar mapa dos estados do Nordeste com ggplot2
### Observacao - não exibindo Pernambuco por falta de dados
ggplot(data = medias_geodata) +
  geom_sf(aes(fill = medias), color = "white") +
  facet_wrap( ~ disciplina) +
  #alterar a letra no parâmetro option para mudar as cores
  scale_fill_viridis_c(option = "D", direction = -1) +
  labs(title = "Médias ENEM - 2023 - Nordeste",
       subtitle = "Média das notas de diferentes disciplinas por estado.\nCores mais escuras representam médias maiores.",
       caption = "Fonte: INEP - Microdados Enem 2023.\nEstado de Pernambuco não exibido por falta de dados.",
       fill = "Média") +
  theme_bw() +
  #formatação dos campos
  theme(
    legend.text = element_text(family = "Ubuntu", face = "bold", color = "black", size = 10),
    legend.key.size = unit(0.3, "cm"),
    legend.position = "right",
    plot.title = element_text(family = "Ubuntu", face = "bold", size = 20),
    plot.subtitle = element_text(size = 12),
    plot.margin = margin(t = 20, r = 8, b = 7, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    painel.grid = element_blank()
  )
#-------------------------------------------------------------------------------

## Médias das disciplinas por municípios de Sergipe
### Criar novo dataframe só com os dados de Sergipe
ENEM_2023_SE <- microdados_enem_nordeste_1_ %>%
  filter(SG_UF_PROVA == "SE")
### Visualizar resumo dados
glimpse(ENEM_2023_SE)
### Calcular as médias de cada disciplina para cada município de Sergipe
medias_por_municipio <- ENEM_2023_SE %>%
  group_by(CO_MUNICIPIO_PROVA) %>%
  summarise(media_cn = mean(NU_NOTA_CN, na.rm = TRUE),
            media_ch = mean(NU_NOTA_CH, na.rm = TRUE),
            media_lc = mean(NU_NOTA_LC, na.rm = TRUE),
            media_mt = mean(NU_NOTA_MT, na.rm = TRUE),
            media_re = mean(NU_NOTA_REDACAO, na.rm = TRUE))
### Pivotar tabela - arrumar para exibir no gráfico
medias_tabela_SE <- medias_por_municipio %>%
  pivot_longer(cols = c(media_cn,media_ch,media_lc,media_mt,media_re),
               names_to = "disciplina",
               values_to = "medias")
### Obter os dados geográficos dos 75 municípios de Sergipe
municipios <- read_municipality("SE", year = 2020)
### Visualizar resumo dados
glimpse(municipios)
### Exibir nome dos municipios
distinct(municipios,name_muni)
### Converter para inteiro para permitir união
municipios$code_muni = as.integer(municipios$code_muni)
### Renomear coluna para falicitar união
municipios <- municipios %>%
  rename(CO_MUNICIPIO_PROVA = code_muni)
### Unir os dados das médias com os dados geográficos dos municipios
medias_geodata_SE <- municipios %>%
  inner_join(medias_tabela_SE, by = "CO_MUNICIPIO_PROVA")
### Renomear valores para melhor exibição no gráfico
medias_geodata_SE$disciplina[medias_geodata_SE$disciplina == "media_cn"] <- "Ciências da Natureza"
medias_geodata_SE$disciplina[medias_geodata_SE$disciplina == "media_ch"] <- "Ciências Humanas"
medias_geodata_SE$disciplina[medias_geodata_SE$disciplina == "media_lc"] <- "Língua Portuguesa"
medias_geodata_SE$disciplina[medias_geodata_SE$disciplina == "media_mt"] <- "Matemática"
medias_geodata_SE$disciplina[medias_geodata_SE$disciplina == "media_re"] <- "Redação"


# Gráfico mapa médias disciplinas municipios de Sergipe
### Observacao - exibindo apenas municípios onde ocorreu prova
ggplot(data = medias_geodata_SE) +
  geom_sf(aes(fill = medias), color = "white") +
  facet_wrap( ~ disciplina) +
  #alterar a letra no parâmetro option para mudar as cores
  scale_fill_viridis_c(option = "D", direction = -1) +
  labs(title = "Médias ENEM - 2023\nMunicípios de Sergipe",
       subtitle = "Média das notas de diferentes disciplinas por municípios de Sergipe.\nCores mais escuras representam médias maiores.",
       caption = "Fonte: INEP - Microdados Enem 2023.",
       fill = "Média") +
  theme_bw() +
  #formatação dos campos
  theme(
    legend.text = element_text(family = "Ubuntu", face = "bold", color = "black", size = 8),
    legend.key.size = unit(0.3, "cm"),
    legend.position = "right",
    plot.title = element_text(family = "Ubuntu", face = "bold", size = 20),
    plot.subtitle = element_text(size = 12),
    plot.margin = margin(t = 20, r = 8, b = 7, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    painel.grid = element_blank()
  )


