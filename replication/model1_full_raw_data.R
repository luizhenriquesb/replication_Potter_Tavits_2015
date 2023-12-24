full_raw_data <-lm(postenp ~ fundparity4
         + demyears
         + fed 
         + pres 
         + log(avemag) 
         + fract 
         + log(avemag):fract, 
         data=raw_data)

display(full_raw_data)
summary(full_raw_data)


# 1. Resíduos vs. Valores Ajustados
ggplot() +
  aes(x = full_raw_data$fitted.values, y = resid(full_raw_data)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  theme_classic() +
  labs(
    title = "Resíduos vs. Valores Ajustados",
    subtitle = "Amostra de 10 mil casos aleatórios",
    x = "Valores Ajustados",
    y = "Resíduos")

# 2. homocedasticidade
ggplot() +
  aes(x = full_raw_data$fitted.values, y = resid(full_raw_data)^2) +
  labs(
    title = "Magnitude dos resíduos",
    subtitle = "Seleção de 10 mil casos aleatórios",
    x = "Preditor",
    y = "Resíduos ao quadrado"
  ) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  theme_classic()

# 3. Normalidade dos erros
qqnorm(residuals(full_raw_data))
qqline(residuals(full_raw_data))

# 4. vif

car::vif(full_raw_data) |> knitr::kable()
