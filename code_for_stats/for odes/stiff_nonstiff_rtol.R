# changing tol

y_01 <- c(Moose = 0, Pyoung = 17, Pmature = 0, Unpal = 100)
out2_tol <- ode(y = y_01, times = time_vec1, func = lv, parms = parms,
                events=list(func = eventfun, time = time_vec1), atol = 1e-15, rtol = 1e-15)

long_out2_tol <- as.data.frame(out2_tol) %>% 
  pivot_longer(cols = c(2:5))

ggplot(subset(long_out2_tol, time >= 1450 & name == "Pyoung"), aes(x = time, y = value)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme_bw() + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0000000001))

# chnaging the error tolerance to be much smaller almost gets rid of the oscillations, maximum precision requested is 1e-15 and we still get absolutely miniscule oscilations around Pyoung

ggplot(subset(long_out2_tol), aes(x = time, y = value)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme_bw() + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0000000001))

# stiff
out2_lsode <- ode(y = y_01, times = time_vec1, func = lv, parms = parms, 
                  method = "lsode", events=list(func = eventfun, time = time_vec1), atol = 1e-21)

diagnostics(out2_lsode)

long_lsode <- as.data.frame(out2_lsode) %>% 
  pivot_longer(cols = c(2:5))

ggplot(subset(long_lsode, time <= 10 & name == "Moose"), aes(x = time, y = value)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme_bw() + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.0000000000000000000000001))

out2_vode <- ode(y = y_01, times = time_vec1, func = lv, parms = parms, method = "vode",
                  events=list(func = eventfun, time = time_vec1))

out2_lsodes <- ode(y = y_01, times = time_vec1, func = lv, parms = parms, method = "lsodes",
                   events=list(func = eventfun, time = time_vec1))


# nonstiff
out2_lsodar <- ode(y = y_01, times = time_vec1, func = lv, parms = parms, method = "lsodar",
                  events=list(func = eventfun, time = time_vec1))

long_lsodar <- as.data.frame(out2_lsodar) %>% 
  pivot_longer(cols = c(2:5))

ggplot(subset(long_lsodar, time >= 407 & name == "Unpal"), aes(x = time, y = value)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme_bw() + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.00001))



# stiff solver gets rid of oscillations, and values are the same
# seems to be a common occurance https://discourse.julialang.org/t/simple-approachable-example-highlighting-the-usage-of-a-stiff-ode-solver-in-diffeq/102741

# stiff solver can be rationalized because of the magnitudes of difference in scale of my stocks

# but stiff solver doesnt seem to use events properly so some moose values are negative
