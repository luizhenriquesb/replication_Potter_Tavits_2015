# Modelo 1 (all democracies)
full <- lm(
  postenp ~ fundparity4
  + demyears
  + fed
  + pres
  + log(avemag)
  + fract
  + log(avemag):fract,
  data = campaigns
)

# Modelo 2 (1974 and later)
post1974 <- lm(
  postenp ~ fundparity4
  + demyears
  + fed
  + pres
  + log(avemag)
  + fract
  + log(avemag):fract,
  data = later1974
)

# Criando um data frame com os valores ajustados e os resíduos de ambos os modelos
residuals_full <- residuals(full)
fitted_values_full <- fitted(full)
data_full <- data.frame(modelo = rep("all democracies", length(residuals_full)),
                        fitted = fitted_values_full, 
                        residuals = residuals_full)

residuals_post1974 <- residuals(post1974)
fitted_values_post1974 <- fitted(post1974)
data_post1974 <- data.frame(modelo = rep("1974 and later", length(residuals_post1974)),
                           fitted = fitted_values_post1974, 
                           residuals = residuals_post1974)

# Combinando os data frames dos dois modelos
combined_data <- rbind(data_full, data_post1974)

# Criando o gráfico de dispersão dos valores ajustados versus resíduos para ambos os modelos
combined_data |> 
  ggplot() +
  aes(x = fitted, y = residuals, color = modelo) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Resíduos vs. Valores Ajustados",
    x = "Valores Ajustados",
    y = "Resíduos"
  ) +
  theme_classic()

# Criando o gráfico de dispersão dos valores ajustados versus resíduos para ambos os modelos
combined_data |> 
  ggplot() +
  aes(x = fitted, y = sqrt(residuals), color = modelo) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  # geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "Resíduos vs. Valores Ajustados",
    x = "Valores Ajustados",
    y = "sqrt(Resíduos)"
  ) +
  theme_classic()
