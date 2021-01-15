## Análise exploratória dos dados por Unidades de desenvolvimento Humano (UDH) 
## na região metropolitana da grande São Paulo (RMSP)

library(tidyverse)
library(corrplot)
library(reshape2)
library(readxl)
# Definindo diretório de trabalho
setwd("C:\\GISA_Lucca\\Projetos_Lucca_GISA\\Demandas_CEINFO\\01142021_RMP_COVID19_UDH")
# Importando banco de dados completo

DadosPNUD <- read_excel("PNAD.xlsX", sheet = "TABELA") 

# Criando um banco secundário com variáveis previamente selecionadas a partir de análise dos metadados

#Filtrando apenas as informações referentes ao ano mais recente (2017) e selecionando as unidades federativas

DadosPNUD %>% 
  filter(Ano == 2017) %>% 
  arrange(desc(AGREGA))




#CONSTRUINDO GRÁFICO 1

#Selecionando as variáveis sem unidade que variam de 0 a 1 para efetuar uma análise inicial

IDH_1 <- read_excel("PNAD.xlsX", sheet = "TABELA") 

IDH_2 <- select(IDH_1, COR, IDHM_E , IDHM_L, IDHM_R)

IDH_3 <- rename(IDH_2, "Educação" = IDHM_E, "Longevidade" = IDHM_L, "Renda" = IDHM_R)

IDH_4 <- melt(IDH_3)

grafico_1 <- ggplot(IDH_4, aes(x = variable, y = value)) + geom_boxplot() + 
  geom_jitter(alpha = 3, aes(color = COR), show.legend = T) + 
  labs( x = "", y = "", title = "Distribuição dos valores que compõe o IDH classificados pela variável raça/cor")
grafico_1

#### Vamos análiser o banco de dados principal dando foco especial na população raça/cor autodeclarada negra
#### visto que os indicadores de educação, longevidade e renda estão em média menores, configurando situação de risco.

DadosPNUD <- read_excel("PNAD.xlsX", sheet = "TABELA") 

P_NEGRA_UF <- DadosPNUD %>% 
  filter(Ano == 2017 & COR == 'NEGRA', SIGLA_AGREGA != 'BRASIL') %>% 
  arrange(AGREGA)

P_NEGRA_UF <- P_NEGRA_UF[1:27,]

    

## Vamos analisar o comportamento de alguns indicadores importantes e desagregar espacialmente para mapear a situação

#Indicador importante de educação que influencia o nível de IDH
grafico_2 <- ggplot(P_NEGRA_UF, aes(x = reorder(SIGLA_AGREGA, -T_ANALF18M), y = T_ANALF18M)) + 
  geom_bar(aes(color = ), show.legend = F , stat = 'identity') + coord_flip() +
  labs(x = "", y = "")

grafico_3 <- ggplot(P_NEGRA_UF, aes(x = reorder(SIGLA_AGREGA, MORT1), y = MORT1)) + 
  geom_bar(aes(fill = MORT1), show.legend = F , stat = 'identity') + coord_flip() +
  labs(x = "", y = "") + scale_fill_gradient()
grafico_3 
