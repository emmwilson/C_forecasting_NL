
# get values of Moose and BY combinations that go to forest 100% or 0%
n_f_rec <- n_f %>% 
  filter(percent_recover == 100 | percent_recover == 0) %>% 
  mutate(MPy = paste0(Moose, "_", By))

# make into list
MBy_list <- as.list(n_f_rec$MPy)

# get all combinations of initial conditions from list
y_0d_rec <- lapply(y_0d, function(x) paste0(x[[1]],"_", x[[2]]))

# remove ones that went to 100 or 0
y_0d_rec2 <- y_0d_rec[ !y_0d_rec %in% MBy_list ]

# remove y_0d_rec from y_0d based on name 
y_0d_par <- y_0d[names(y_0d) %in% names(y_0d_rec2)]
  