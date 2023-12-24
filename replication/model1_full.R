
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

# Model 1: Full (sem outilers)

# Testando tudo de uma vez

library(easystats)
check_model(full)

# Linearidade

plot(raw.data$postenp, raw.data$fundparity4)

# 1. Resíduos vs. Valores Ajustados
ggplot() +
  aes(x = full$fitted.values, y = resid(full)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_classic() +
  labs(
    title = "Resíduos vs. Valores Ajustados",
    subtitle = "Amostra de 10 mil casos aleatórios",
    x = "Valores Ajustados",
    y = "Resíduos"
  )


# 2. homocedasticidade
ggplot() +
  aes(x = full$fitted.values, y = resid(full)^2) +
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
qqnorm(residuals(full))
qqline(residuals(full))

# 4. vif

car::vif(full) |> knitr::kable()
