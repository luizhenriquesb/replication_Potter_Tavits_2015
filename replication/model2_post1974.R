library(tidyverse)

post1974<-lm(postenp ~ fundparity4
             + demyears
             + fed 
             + pres 
             + log(avemag) 
             + fract 
             + log(avemag):fract, 
             data=later1974)	

# Testando tudo de uma vez

library(easystats)
check_model(post1974)

# Linearidade

plot(raw.data$postenp, raw.data$fundparity4)
plot(campaigns$postenp, campaigns$fundparity4)

# 1. Resíduos vs. Valores Ajustados
ggplot() +
  aes(x = post1974$fitted.values, y = resid(post1974)) +
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
  aes(x = post1974$fitted.values, y = resid(post1974)^2) +
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
qqnorm(residuals(post1974))
qqline(residuals(post1974))

# 4. vif

car::vif(post1974) |> knitr::kable()
