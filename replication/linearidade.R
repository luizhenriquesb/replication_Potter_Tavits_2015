
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

# Linearidade da relação postenp x fundparity4
campaigns |> 
  ggplot() +
  aes(x = postenp, y = fundparity4) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "fundparity4",
    x = "fundparity4",
    y = "Resíduos"
  ) +
  theme_classic()

# Linearidade da relação postenp x demyears
campaigns |> 
  ggplot() +
  aes(x = postenp, y = demyears) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Number of democratic years",
    x = "demyears",
    y = "Resíduos"
  ) +
  theme_classic()

# Linearidade da relação postenp x fed
campaigns |> 
  ggplot() +
  aes(x = postenp, y = fed) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Whether or not federal",
    x = "fed",
    y = "Resíduos"
  ) +
  theme_classic()

# Linearidade da relação postenp x pres
campaigns |> 
  ggplot() +
  aes(x = postenp, y = pres) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Whether or not presidential",
    x = "pres",
    y = "Resíduos"
  ) +
  theme_classic()

# Linearidade da relação postenp x log(avemag)
campaigns |> 
  ggplot() +
  aes(x = postenp, y = log(avemag)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Average district magnitude",
    x = "log(avemag)",
    y = "Resíduos"
  ) +
  theme_classic()

# Linearidade da relação postenp x fract
campaigns |> 
  ggplot() +
  aes(x = postenp, y = fract) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Ethnolinguistic fractionalization",
    x = "fract",
    y = "Resíduos"
  ) +
  theme_classic()

# Linearidade da relação postenp x log(avemag):fract
interactions <- data.frame(
  log_avemag_fract = log(campaigns$avemag) * campaigns$fract,
  postenp = campaigns$postenp
)

interactions |> 
  ggplot() +
  aes(x = log_avemag_fract, y = postenp) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Average district magnitude * Ethnolinguistic fractionalization",
    x = "log(avemag):fract",
    y = "Resíduos"
  ) +
  theme_classic()




# Obtendo os resíduos e valores ajustados
residuals_full <- residuals(full)
fitted_values_full <- fitted(full)

# Gráfico de valores ajustados versus resíduos
plot(fitted_values_full, residuals_full,
     xlab = "Valores Ajustados", ylab = "Resíduos",
     main = "Resíduos vs. Valores Ajustados")
abline(h = 0, col = "red")
# Criando um data frame com os valores ajustados e os resíduos
residuals_full <- residuals(full)
fitted_values_full <- fitted(full)
data <- data.frame(Fitted = fitted_values_full, Residuals = residuals_full)

# Gráfico de valores ajustados versus resíduos com linha suavizada usando ggplot
ggplot(data, aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, color = "red") +
  xlab("Valores Ajustados") +
  ylab("Resíduos") +
  ggtitle("Resíduos vs. Valores Ajustados")
