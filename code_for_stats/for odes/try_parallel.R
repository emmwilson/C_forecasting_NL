install.packages("parallel")
library(parallel)

args_p <- args
args_p$output <- mcmapply(ode_reps, parameters, initial_conditions, SIMPLIFY = F, mc.cores = getOption("mc.cores", 2L)