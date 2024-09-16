# maturing decay with increasing consumption
pacman::p_load(
  tidyverse,
  deSolve)

g <- 0.133

consumption <- seq(0, 100, by = 1)
Pyoung <- rep(100, 101)

mat_con <- data.frame(con = consumption, Py = Pyoung)%>% 
  mutate(ratio = consumption/Pyoung) %>% 
  mutate(mature = g * g^(3*ratio))

plot(mat_con$ratio, mat_con$mature)

ggplot(mat_con, aes(ratio, mature)) +
  geom_line() +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "Consumption/Young browsed biomass", y = "Rate of maturation")+
  theme(plot.margin=unit(c(0.2,0.4,0.2,0.2), 'cm'))
  
