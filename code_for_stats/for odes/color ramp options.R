# return year where change in all stock less tha 1e-10

args_recover_delta <- as.data.frame(args_recover[["269 forest"]]) %>% 
  mutate(dMoose = Moose - lag(Moose), dPyoung = Pyoung - lag(Pyoung), dPmature = Pmature - lag(Pmature), dUnpal = Unpal - lag(Unpal)) 
#%>% filter(if_all(c(6:9), ~. <= 1e-8 | is.nan(.)))
         

ggplot(subset(args_recover_delta, time >=600), aes(x = time, y = Pmature)) +
      geom_line() 

ggplot() +
  geom_tile(data = avg_yr_50_f, aes(x = Moose, y = By, fill = mean_yr)) +
  geom_step(data = n_f2, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_gradient2(low = "#00400f", high = "#FF6600", mid = "#FFEA00", midpoint = 430) +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Years to recover") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 500)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 6))

ggplot() +
  geom_tile(data = avg_yr_50_f, aes(x = Moose, y = By, fill = mean_yr)) +
  geom_step(data = n_f2, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_gradient2(low = "#00400f", high = "#FF6600", mid = "white", midpoint = 430) +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Years to recover") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 500)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 6))

ggplot() +
  geom_tile(data = avg_yr_50_f, aes(x = Moose, y = By, fill = mean_yr)) +
  geom_step(data = n_f2, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_gradient(low = "#00400f", high  = "#ef7400") +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Years to recover") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 500)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 6))


ggplot() +
  geom_tile(data = avg_yr_50_f, aes(x = Moose, y = By, fill = mean_yr)) +
  geom_step(data = n_f2, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_gradient(low = "#00400f", high  = "#FFEA00") +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Years to recover") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 500)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 6))

ggplot() +
  geom_tile(data = n_f, aes(x = Moose, y = By, fill = percent_recover)) +
  geom_step(data = n_f2, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_gradient2(high = "#00400f", low = "#FF6600", mid = "#FFEA00", midpoint = 50) +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Percent\nrecovered") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

ggplot() +
  geom_tile(data = n_f, aes(x = Moose, y = By, fill = percent_recover)) +
  geom_step(data = n_f2, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_gradient(high = "#00400f", low = "#FFEA00") +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Percent\nrecovered") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

ggplot() + #currrently this one
  geom_tile(data = n_f, aes(x = Moose, y = By, fill = percent_recover)) +
  geom_step(data = n_f2, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_gradient2(high = "#4D8F26", low = "#f29414", mid = "#f3f59e", midpoint = 50) +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Percent\nrecovered") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(- 10.34483/2, 300)) +
  scale_x_continuous(expand = c(0,0))

ggplot() +
  geom_tile(data = n_f, aes(x = Moose, y = By, fill = percent_recover)) +
  geom_step(data = n_f2, aes(x = Moose, y = By), size = 0.5) +
  scale_fill_gradient2(high = "#2b7a0e", low = "#f29414", mid = "white", midpoint = 50) +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Percent\nrecovered") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(- 10.34483/2, 300)) +
  scale_x_continuous(expand = c(0,0))+ theme(legend.position="top")



ggplot() +
  geom_tile(data = avg_yr_50_f, aes(x = Moose, y = By, fill = mean_yr))  +
  scale_fill_gradient2(low = "#4EBFBF", high = "#F25749", mid = "#F2762E", midpoint = 500, na.value = "white") +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Years to\nrecover") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(- 10.34483/2, 300)) +
  scale_x_continuous(expand = c(0,0))

ggplot() + # and this one
  geom_tile(data = avg_yr_50_f, aes(x = Moose, y = By, fill = mean_yr)) +
  scale_fill_gradient(low = "#421650", high = "white", na.value = "#C4C4C4") +
  labs(x = "Moose biomass (Metric tons)", y = "Young palatable plant biomass (Metric tons)", fill = "Years to\nrecover") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(- 10.34483/2, 300)) +
  scale_x_continuous(expand = c(0,0))



