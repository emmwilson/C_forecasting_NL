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


# try 3d surface plot

library(plot3D)
library(plotly)


M <- mesh(seq(0, 100, by = 0.25),seq(0, 100, by = 0.25))
x <- M$x
y <- M$y
z <- 0.133 * 0.133^(3*x/(y+0.01))

surf3D(x, y, z, colvar = z, bty = "b", phi = 5, theta = 30, xlab = "Consumption (t)", ylab = "By (t)", 
       zlab = "rate of maturation", r = 3, cex = 1, cex.lab = 0.6, contour = list(ces.lab = 0.1)) 
  



# plotly

plot_ly(x = x, y = y, z = z) %>% 
  add_surface(colorscale = 'Viridis') %>%
  layout(
    showlegend = T,
    scene = list(
      xaxis = list(title = 'Consumption (t)', showticklabels = FALSE,
                   showgrid = FALSE), 
      yaxis = list(title = 'By (t)', showticklabels = FALSE,
                   showgrid = FALSE),
      zaxis = list(title = 'Maturation (t/year)')))


      