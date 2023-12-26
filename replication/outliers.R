library(tidyverse)
library(haven)
library(easystats)

# Dados não tratados
raw_data <- haven::read_dta("data/potter_tavits_data.dta") |> 
  as_tibble()
glimpse(raw_data)

# Sem outliers
# campaigns <- subset(raw_data, postenp < 9.2)
campaigns <- raw_data |> 
  filter(postenp < 9.2)

# Modelo 1 - raw.data -----------------------------------------------------

full_raw_data <- lm(
  postenp ~ fundparity4
    + demyears
    + fed
    + pres
    + log(avemag)
    + fract
    + log(avemag):fract,
  data = raw_data
)

summary(full_raw_data)

stargazer::stargazer(full_raw_data, type = "text")

### 1.1 Identificando outilers ----

raw_data_na <- raw_data |> drop_na(postenp, 
                                   fundparity4,
                                   demyears,
                                   fed,
                                   pres,
                                   avemag,
                                   fract)
n_distinct(raw_data$cnty)
n_distinct(raw_data_na$cnty)

# obs que foram exlucidas do modelo linear
# Obtém o conjunto de dados usado no modelo
dados_do_modelo <- model.frame(full_raw_data)

# Obtém os índices das observações no conjunto de dados original que não foram usados no modelo
observacoes_excluidas <- setdiff(1:nrow(raw_data), rownames(dados_do_modelo))

raw_data[observacoes_excluidas,]



# Criar um box plot para uma variável específica ('variavel1')
boxplot(postenp)

# Identificar outliers clicando neles no gráfico
identify(boxplot(raw_data$postenp))

identify(raw_data$postenp)





# Supondo que seus dados estejam em um dataframe chamado "dados" e a coluna de interesse seja "valores"
coluna <- raw_data$postenp

# Calculando o intervalo interquartil usando a função IQR()
iqr <- IQR(coluna, na.rm = TRUE)

# Calculando os limites para identificar outliers
limite_inferior <- quantile(coluna, 0.25, na.rm = TRUE) - 1.5 * iqr
limite_superior <- quantile(coluna, 0.75, na.rm = TRUE) + 1.5 * iqr

# Identificando outliers
outliers <- coluna[coluna < limite_inferior | coluna > limite_superior]

# Exibindo os valores considerados outliers
print(outliers)

boxplot()


# Supondo que seus dados estejam em um dataframe chamado "dados" e a coluna de interesse seja "valores"
boxplot(raw_data$postenp, main = "Boxplot dos Valores")

# Identificando outliers no boxplot
outliers <- boxplot(raw_data$postenp, plot = FALSE)$out

# Exibindo os valores considerados outliers
print(outliers)

library(ggplot2)

# Supondo que seus dados estejam em um dataframe chamado "dados" e a coluna de interesse seja "valores"
raw_data |> 
  drop_na(postenp) |> 
  ggplot() +
  aes(y = postenp) +
  geom_boxplot() +
  ggtitle("Boxplot dos Valores") +
  geom_text(raw_data = subset(raw_data, postenp > quantile(postenp, 0.75) + 1.5 * IQR(postenp) |
                            postenp < quantile(postenp, 0.25) - 1.5 * IQR(postenp)),
            aes(label = round(postenp, 2)), nudge_x = 0.1, nudge_y = 0.1, color = "red")


raw_data |> filter(postenp > 10) |> dplyr::select(cnty, postenp)




raw_data |> dplyr::filter(postenp > 10) |> dplyr::select(cnty, year, postenp)






#### Residuals vs leverage ----

par(mfrow = c(2,2))
plot(full_raw_data)

raw_data[c(1, 113, 180),] 

#### Standardized residuals ----

outliers <- which(rstandard(full_raw_data) > 2 | rstandard(full_raw_data) < - 2)

raw_data[outliers,]

outliers <- which(rstandard(full_raw_data) > 2 | rstandard(full_raw_data) < - 2)
paises_outliers <- raw_data[outliers,] |> pull(cnty)



### 1.2 Regressao robusta ----

# Utilize a função 'rlm' para uma regressão robusta
library(MASS)  # Certifique-se de ter carregado a biblioteca MASS

robust_model <- rlm(postenp ~ fundparity4 
                    + demyears 
                    + fed 
                    + pres 
                    + log(avemag) 
                    + fract 
                    + log(avemag):fract, 
                    data = raw_data)

summary(robust_model)

stargazer::stargazer(robust_model, type = "text")

# Modelo 1 - campaigns ----------------------------------------------------

full_campaigns <- lm(
  postenp ~ fundparity4
  + demyears
  + fed
  + pres
  + log(avemag)
  + fract
  + log(avemag):fract,
  data = campaigns
)

summary(full_campaigns)


