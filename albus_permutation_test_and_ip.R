################################################################################
# Session Settings
################################################################################
library(purrr)# for detect_index
library(dplyr) # for group_by
library(slam) # simple triplet matrix
library(gurobi) # solving LP
library(parallel) # for mclapply (only works for linux in a parallel version)
library(doParallel)
# for informations about parrallelise the code:
# https://waterprogramming.wordpress.com/2020/03/16/parallel-processing-with-r-on-windows/
# setwd(TODO)
source("constraints_r1_r2_aktuell.R") # contains the functions compute_constraints...

################################################################################
# Prepare Data Set
################################################################################
# Info about the record: https://www.gesis.org/allbus/allbus
# Here Allbus from 2014, see: ZA5240_fb.pdf on github
# We look at questions "V226" (health), "V102" (education), and.
# "V417" (income), "V81" (sex), "V7" (Erhebungsgebiet Ost -West)
# one could also look at V418
# "V2" is the identification number of the respondent.

dat_read <- foreign::read.spss("ZA5240_v2-1-0.sav", to.data.frame = TRUE,
                               sep = ",", dec = ".")
dat_convert_values <- dat_read[, c("V5", "V226", "V102", "V417", "V81", "V7")]
colnames(dat_convert_values) <- c("Split", "Gesundheit", "Ausbildung",
                                  "Einkommen", "Geschlecht", "O_W_Deutschland")

# deleting NAs
# here we are deleting the entry with minimal "Einkommen" value 37. We have to
# delete the nas bevor considering the rest, because else NA would be added as
# a further factor in as.order() later
non_na <- c()
for (i in seq(1, dim(dat_convert_values)[1])) {
  if (!any(is.na(dat_convert_values[i,]))) {
    non_na <- c(non_na, i)
  }
}
dat_convert_values <- dat_convert_values[non_na, ]

# Converting the values into numeric and order modes
# "Einkommen": compare with page 492 of codebook, variable V417
# "Gesundheit": compare with page 251 of codebook, variable V226
# "Ausbildung": compare with page 105 of codebook, variable V102

# dat_convert_values[["Einkommen"]][seq(1,10)]
# levels(dat_convert_values[["Einkommen"]])
# as.numeric(as.character(dat_convert_values[["Einkommen"]]))[seq(1,10)]
dat_convert_values[["Einkommen"]] <- as.numeric(as.character(
  dat_convert_values[["Einkommen"]]))

# dat_convert_values[["Ausbildung"]][seq(1,10)]
# levels(dat_convert_values[["Ausbildung"]])
# as.ordered(as.numeric(dat_convert_values[["Ausbildung"]]))[seq(1,10)]
dat_convert_values[["Ausbildung"]] <- as.ordered(as.numeric(
  dat_convert_values[["Ausbildung"]]))

# dat_convert_values[["Gesundheit"]][seq(1,10)]
# levels(dat_convert_values[["Gesundheit"]])
# as.ordered(as.numeric(dat_convert_values[["Gesundheit"]]))[seq(1,10)]
# # Note that here 1 is the best and 6 the worst health status --> needs to be
# # switched --> (7 -)
dat_convert_values[["Gesundheit"]] <- as.ordered( 7 - as.numeric(
  dat_convert_values[["Gesundheit"]]))
dat_convert_values[["Gesundheit"]][seq(1,10)]

# We only consider "Split B: F75B" and the population of east Germany
dat_convert_values <- dat_convert_values[which(
  dat_convert_values[["Split"]] == "SPLIT B: F75B"), ]


dat_convert_values <- dat_convert_values[which(
  dat_convert_values[["O_W_Deutschland"]] == "NEUE BUNDESLAENDER"), ]

dat <- dat_convert_values[ , c("Gesundheit",  "Ausbildung", "Einkommen",
                               "Geschlecht")]



# Duplicate handling
dat_dup_divi_sex <- dat %>% group_by_all() %>% count()
dat_dup_without_sex <- duplicated(dat_dup_divi_sex[, seq(1,3)])

data_female <- dat_dup_divi_sex[which(dat_dup_divi_sex$Geschlecht == "WEIBLICH"),
                                c(1, 2, 3, 5)] # deleting "Geschlect" column
data_female <- matrix(as.numeric(as.matrix(data_female)), ncol = 4)
colnames(data_female) <-  c("ordinal_1", "ordinal_2", "numeric", "dup_female")

data_male <- dat_dup_divi_sex[which(dat_dup_divi_sex$Geschlecht == "MAENNLICH"),
                              c(1, 2, 3, 5)]  # deleting "Geschlect" column
data_male <- matrix(as.numeric(as.matrix(data_male)), ncol = 4)
colnames(data_male) <-  c("ordinal_1", "ordinal_2", "numeric", "dup_male")

dat_final <- merge(x = data_female, y = data_male,
                   by = c("ordinal_1", "ordinal_2", "numeric"),
                   all.x = TRUE, all.y = TRUE)
dat_final[is.na(dat_final)] <- 0
dat_final$dup_all <- dat_final$dup_female + dat_final$dup_male
dat_final$ID <- seq(1:dim(dat_final)[1])


# View(dat_final)
# dim(dat_final)
# min(dat_final$numeric)
# max(dat_final$numeric)

index_max <- which(dat_final$numeric == max(dat_final$numeric))
# dat_final[index_max, ]
index_min <- which(dat_final$numeric == min(dat_final$numeric))
# dat_final[index_min, ]

# # Add minimal and maximal at the bottom of the matrix
dat_final[dim(dat_final)[1] + 1, ] <- c(min(dat_final$ordinal_1),
                                        min(dat_final$ordinal_2),
                                        dat_final[index_min, 3],
                                        0, 0, 0,
                                        max(dat_final$ID) + 1)
dat_final[dim(dat_final)[1] + 1, ] <- c(max(dat_final$ordinal_1),
                                        max(dat_final$ordinal_2),
                                        dat_final[index_max, 3],
                                        0, 0, 0,
                                        max(dat_final$ID) + 1)

rm(dat_read)
rm(dat)
rm(dat_convert_values)
rm(dat_dup_divi_sex)
rm(dat_dup_without_sex)
rm(data_female)
rm(data_male)
rm(i)
rm(index_max)
rm(index_min)
rm(non_na)
################################################################################
# Data Set: 100 womand and 100 man randomly sampled
# (and a small test data set --> see example 2)
################################################################################
# ## Test data set of example is given by
# set.seed(48)
# size_f_m <- 7
# dat_set <- dat_final
# cumsum_female <- cumsum(dat_set$dup_female)
# cumsum_male <- cumsum(dat_set$dup_male)
# sum_female <- sum(dat_set$dup_female)
# sum_male <- sum(dat_set$dup_male)
# dat_set$dup_female <- 0
# dat_set$dup_male <- 0
#
# # Compute a random sample
# woman_sample <- sample(seq(0, sum_female), size = size_f_m, replace = FALSE)
# for (i in woman_sample) {
#   index_sample_value <- which.max(cumsum_female >= i)
#   dat_set$dup_female[index_sample_value] <-
#     dat_set$dup_female[index_sample_value] + 1
# }
#
# man_sample <- sample(seq(0, sum_male), size = size_f_m, replace = FALSE)
# for (i in man_sample) {
#   index_sample_value <- which.max(cumsum_male >= i)
#   dat_set$dup_male[index_sample_value] <-
#     dat_set$dup_male[index_sample_value] + 1
# }
#
#
# dat_set$dup_all <- dat_set$dup_female + dat_set$dup_male
# dat_set <- dat_set[-which(dat_set$dup_all == 0), ]
# # add minimal and maximal element row (the last two rows of dat_final)
# # Note that the minimal and maximal value has neither a value for male nor female
# # -> does not increase the sample size
# dat_set[c(dim(dat_set)[1] + 1, dim(dat_set)[1] + 2), ] <-
#   dat_final[c(dim(dat_final)[1] - 1, dim(dat_final)[1]), ]
# dat_set$ID <- seq(1, dim(dat_set)[1])
# dat_set <-  dat_set[c(8,9,11,12, 15, 16), ]
# dat_set$ID <- seq(1, dim(dat_set)[1])



## Data Set used for computations
set.seed(48)
size_f_m <- 100
dat_set <- dat_final
cumsum_female <- cumsum(dat_set$dup_female)
cumsum_male <- cumsum(dat_set$dup_male)
sum_female <- sum(dat_set$dup_female)
sum_male <- sum(dat_set$dup_male)
dat_set$dup_female <- 0
dat_set$dup_male <- 0

# Compute a random sample
woman_sample <- sample(seq(0, sum_female), size = size_f_m, replace = FALSE)
for (i in woman_sample) {
  index_sample_value <- which.max(cumsum_female >= i)
  dat_set$dup_female[index_sample_value] <-
    dat_set$dup_female[index_sample_value] + 1
}

man_sample <- sample(seq(0, sum_male), size = size_f_m, replace = FALSE)
for (i in man_sample) {
  index_sample_value <- which.max(cumsum_male >= i)
  dat_set$dup_male[index_sample_value] <-
    dat_set$dup_male[index_sample_value] + 1
}


dat_set$dup_all <- dat_set$dup_female + dat_set$dup_male
dat_set <- dat_set[-which(dat_set$dup_all == 0), ]
# add minimal and maximal element row (the last two rows of dat_final)
# Note that the minimal and maximal value has neither a value for male nor female
# -> does not increase the sample size
dat_set[c(dim(dat_set)[1] + 1, dim(dat_set)[1] + 2), ] <-
  dat_final[c(dim(dat_final)[1] - 1, dim(dat_final)[1]), ]
dat_set$ID <- seq(1, dim(dat_set)[1])


# View(dat_set)
# dim(dat_set)
# sum(dat_set$dup_all)
# dat_set$ID <- seq(1, dim(dat_set)[1])



rm(cumsum_female)
rm(cumsum_male)
rm(sum_female)
rm(sum_male)
rm(woman_sample)
rm(index_sample_value)
rm(man_sample)
rm(i)
rm(size_f_m)


################################################################################
# Information about Computation Times etc w.r.t. the above data set (seed 48)
################################################################################

# constraint_r1_values
# computation time: Time difference of 4.239958 secs
# disk space: 831.8 kB
# dimensions: 8642    173
# number cores: 1

# constraint_r2_values
# computation time: Time difference of 34.27036 mins
# disk space: 680.9 MB
# dimensions: 7103114     173
# number cores: 1

# xi
# computation time: Time difference of 4.385978 mins
#                   (includes also simple_triplet_matrix construction)
# xi value: [1] 0.000121981
# number cores: 1

# computation test statistic for observed data (d_observed)
# computation time: Time difference of 4.909585 mins
# values: $d_nip_nreg: -0.104995,  $d_nip: -0.105,  $d_nreg: -0.0939451,
#         $d: -0.09395
# number cores: 1

# 1 runs of the permutation tests (permutation_test_result)
# computation time: Time difference of 5.522952 mins
# number cores: 1 (without externel permutation -> quess that gurobi does something)

# 2 runs of the permutation tests (permutation_test_result)
# computation time: Time difference of 9.865825 mins
# number cores: 1 (without externel permutation -> quess that gurobi does something)


# 1000 runs of the permutation tests (permutation_test_result)
# computation time:
# number cores:

################################################################################
# Compute constraint matrices
# see Paper page 7, Equation (11)
################################################################################

# Bei der Eingabe ist die Bezeichnung von "sup_female" "dup_male" nicht wichtig.
# Hier wird nur die ID, "ordinal_1", "ordinal_2" und "numeric" gebraucht
# Note: we do not (!) return a simple triplet matrix, only the components of it
start_time_r1 <- Sys.time()
constraint_r1_values <- compute_constraints_r1(dat_set)
total_time_r1 <- Sys.time() - start_time_r1


start_time_r2 <- Sys.time()
constraint_r2_values <- compute_constraints_r2(dat_set,
                                               constraint_r1_values$df_r1_values)
total_time_r2 <- Sys.time() - start_time_r2



# constraints_r1 <- slam::simple_triplet_matrix(constraint_r1_values$r_1_i,
#                                               constraint_r1_values$r_1_j,
#                                               constraint_r1_values$r_1_v)
# constraints_r2 <- slam::simple_triplet_matrix(constraint_r2_values$r_2_i,
# constraint_r2_values$r_2_j,
# constraint_r2_values$r_2_v)
#
# as.matrix(constraints_r2)
# as.matrix(constraints_r1)
# constraint_r2_values$xi
# constraint_r1_values$df_r1_values




# (!) Note that the file name must be changed according to the used data set
# .._100 if the data set with 100 sample of woman / man is used
# .._all if the entire sample is used

# (!) Die Daten werden in einem extra Ordner jeweils abgespeichert. In die Ordner
# mit dem Namen computation_results_..
# .._100 if the data set with 100 sample of woman / man is used
# .._all if the entire sample is used

# setwd()
# # Save objects to a file
# saveRDS(constraint_r1_values, file = "constraint_r1_values_100_seed_48.rds")
# saveRDS(constraint_r2_values, file = "constraint_r2_values_100_seed_48.rds")
# # Restore the objects
# constraint_r1_values <- readRDS("constraint_r1_values_100_seed_48.rds")
# constraint_r2_values <- readRDS("constraint_r2_values_100_seed_48.rds")

dim_r2 <- c(max(constraint_r2_values$r_2_i), dim(dat_set)[1])
dim_r1 <- c(max(constraint_r1_values$r_1_i), dim(dat_set)[1])


rm(start_time_r1)
rm(total_time_r1)
rm(start_time_r2)
rm(total_time_r2)
################################################################################
# Compute xi
# In paper: See Chapter 5.3
# Informations about gurobi (page 643ff):
# https://www.gurobi.com/wp-content/plugins/hd_documentations/documentation/9.0/refman.pdf
################################################################################
xi_r2_constraint <- rep(-1, dim_r2[1])
gurobi_r2_sense <-  rep(">",  dim_r2[1])
if (!any(is.na(constraint_r2_values$xi))) {
  xi_r2_constraint[constraint_r2_values$xi] <- 0
  gurobi_r2_sense[constraint_r2_values$xi] <- "="
}
gurobi_sense <- c(rep(">", dim_r1[1]), gurobi_r2_sense)

# simple_triplet matrix i=row, j=column, v=value

xi_A_r_i <- c(constraint_r1_values$r_1_i, seq(1, dim_r1[1]), # r_1 part
              constraint_r2_values$r_2_i + dim_r1[1], seq(dim_r1[1] + 1, dim_r1[1] + dim_r2[1])) # r_2 part



xi_A_r_j <- c(constraint_r1_values$r_1_j, rep(dim_r1[2] + 1, dim_r1[1]),
             constraint_r2_values$r_2_j, rep(dim_r2[2] + 1, dim_r2[1]))

xi_A_r_v <- c(constraint_r1_values$r_1_v, rep(-1, dim_r1[1]),
              constraint_r2_values$r_2_v, xi_r2_constraint)

# A <- slam::simple_triplet_matrix(xi_A_r_i, xi_A_r_j, xi_A_r_v)


# Note that construction the simple_triplet_matrix needs some minutes
# set min and max to default zero and one -> by lower and upper bound constraints
start_time_xi <- Sys.time()
gurobi_model_xi <- list()
gurobi_model_xi$A <- slam::simple_triplet_matrix(xi_A_r_i, xi_A_r_j, xi_A_r_v)
gurobi_model_xi$rhs <- rep(0, max(xi_A_r_i))
gurobi_model_xi$lb <- c(rep(0, dim_r1[2] - 1), 1, 0)
gurobi_model_xi$ub <- c(rep(1, dim_r1[2] - 2), 0, 1, 1)
gurobi_model_xi$vtypes <- c(rep("C", dim_r1[2] + 1))

gurobi_model_xi$obj <- c(rep(0, dim_r1[2]), 1)
gurobi_model_xi$modelsense <- "max"
gurobi_model_xi$sense <- gurobi_sense

# maximales Element auf 1 und minimales Element auf 0 -> am besten mit lower und upper bound
# minimales und maximales element werden fixiert die WErte



xi_gurobi <- gurobi::gurobi(gurobi_model_xi)
xi <- xi_gurobi$objval
xi
total_time_xi <- Sys.time() - start_time_xi


# # Save object to a file
# saveRDS(xi_gurobi, file = "xi_gurobi_100.rds")
# # Restore the object
# xi_gurobi <- readRDS("xi_gurobi_100.rds")
# xi <- xi_gurobi$objval

rm(gurobi_model_xi)
rm(xi_A_r_i)
rm(xi_A_r_j)
rm(xi_A_r_v)
rm(xi_r2_constraint)
rm(start_time_xi)
rm(total_time_xi)
rm(xi_gurobi)

################################################################################
# Compute Permutation test
# In paper: See Chapter 5.2
################################################################################
all_obs <- sum(dat_set$dup_all)
eps <- 0.5 # 0.0001 -> set eps so small such that eps*xi < 10^{-5}
gamma <- 0.01  #1 / 200 # 10 / 200, 20 / 200


# Constraint Matrix in the LP to compute p
# simple_triplet matrix i=row, j=column, v=value
permu_A_r_i <- c(constraint_r1_values$r_1_i, # r_1 part
                 constraint_r2_values$r_2_i + dim_r1[1]) # r_2 part

permu_A_r_j <- c(constraint_r1_values$r_1_j,
                 constraint_r2_values$r_2_j)

permu_A_r_v <- c(constraint_r1_values$r_1_v,
                 constraint_r2_values$r_2_v)

# Note that construction the simple_triplet_matrix needs some minutes
permu_A <- slam::simple_triplet_matrix(permu_A_r_i, permu_A_r_j, permu_A_r_v)

# The right-hand side vector for the linear constraints
eps_xi_r2_constraint <- rep(xi * eps, dim_r2[1])
if (!any(is.na(constraint_r2_values$xi))) {
  eps_xi_r2_constraint[constraint_r2_values$xi] <- 0
}
permu_rhs <- c(rep(xi * eps, dim_r1[1]), eps_xi_r2_constraint)

# Gurobi Model (no regularization)
# set minimales and maximales Element to 0 and 1 resp (using lower and upper bounds)
gurobi_model_permu <- list()
gurobi_model_permu$A <- permu_A
gurobi_model_permu$rhs <- rep(0, dim_r1[1] + dim_r2[1])
gurobi_model_permu$lb <- c(rep(0, dim_r1[2] - 1), 1)
gurobi_model_permu$ub <- c(rep(1, dim_r1[2] - 2), 0, 1)
gurobi_model_permu$vtypes <- c(rep("C", dim_r1[2]))

gurobi_model_permu$modelsense <- "min"
gurobi_model_permu$sense <- gurobi_sense


# Gurobi Model (regularization)
# set minimales and maximales Element to 0 and 1 resp (using lower and upper bounds)
gurobi_model_permu_regul <- gurobi_model_permu
gurobi_model_permu_regul$rhs <-  permu_rhs

# Reduce dat_set only to the part needed in the permutation test
dat_set_permu <- dat_set[, c("dup_female", "dup_male", "dup_all")]


# Function to compute d based on one permutation
compute_d_permuted <- function(index, gamma,
                               dat_set_permu,
                               gurobi_model_permu,
                               gurobi_model_permu_regul,
                               all_obs,
                               permutated_f_m = TRUE) {
  
  print(paste0("Bin jetzt bei Permutationsanzahl ", index))
  
  # Compute a random sample
  if (permutated_f_m) {
    woman_sample_index <- sample(seq(1, all_obs),
                                 size = sum(dat_set_permu$dup_female),
                                 replace = FALSE)
    woman_sample <- rep(0, all_obs)
    woman_sample[woman_sample_index] <- 1
    dat_set_permu <- dat_set_permu
    dat_set_permu$dup_female <- 0
    
    index_sample <- 1
    for (i in 1:(dim(dat_set_permu)[1] - 2)) {
      # Beachte, dass für jedes i dat_set_permu$dup_all[i] >= 1 sein muss, da es
      # mindestens eine Beobachtung gibt
      woman_sample_i <- woman_sample[seq(index_sample,
                                         index_sample + dat_set_permu$dup_all[i] - 1)]
      dat_set_permu$dup_female[i] <- sum(woman_sample_i)
      index_sample <- index_sample + dat_set_permu$dup_all[i]
    }
    # min und max never observed
    dat_set_permu$dup_male <- dat_set_permu$dup_all - dat_set_permu$dup_female
    dat_set_permu[c(dim(dat_set_permu)[1] - 1, dim(dat_set_permu)[1]), ] <- 0
  }
  
  
  # Zielfunktion definieren
  obj_nip <- (dat_set_permu$dup_male / all_obs) - (dat_set_permu$dup_female / all_obs)
  
  obj_pi_f <- (1 - gamma) * (dat_set_permu$dup_female / all_obs)
  obj_pi_f[dim(dat_set_permu)[1]] <- gamma
  
  obj_pi_m <- (1 - gamma) * (dat_set_permu$dup_male / all_obs)
  obj_pi_m[dim(dat_set_permu)[1] - 1] <- gamma
  
  obj_pi <- as.vector(obj_pi_m - obj_pi_f)
  
  # storing return list
  d_return <- list()
  # Computing d(x,y) (no ip, no regularization)
  g_model_nip_nreg <- gurobi_model_permu
  g_model_nip_nreg$obj <- obj_nip
  result_nip_nreg <- gurobi::gurobi(g_model_nip_nreg)
  if (result_nip_nreg$status == "OPTIMAL") {
    d_return$d_nip_nreg <- result_nip_nreg$objva
  } else {
    d_return$d_nip_nreg <- result_nip_nreg
  }
  
  # Computing d(x,y) (no ip, regularization)
  g_model_nip <- gurobi_model_permu_regul
  g_model_nip$obj <- obj_nip
  result_nip <- gurobi::gurobi(g_model_nip)
  if (result_nip$status == "OPTIMAL") {
    d_return$d_nip <- result_nip$objva
  } else {
    d_return$d_nip <- result_nip
  }
  
  # Computing d(x,y) (ip, no regularization)
  g_model_nreg <- gurobi_model_permu
  g_model_nreg$obj <- obj_pi
  result_nreg <- gurobi::gurobi(g_model_nreg)
  if (result_nreg$status == "OPTIMAL") {
    d_return$d_nreg <- result_nreg$objva
  } else {
    d_return$d_nreg <- result_nreg
  }
  
  # Computing d(x,y) (ip, regularization)
  g_model <- gurobi_model_permu_regul
  g_model$obj <- obj_pi
  result <- gurobi::gurobi(g_model)
  if (result$status == "OPTIMAL") {
    d_return$d <- result$objva
  } else {
    d_return$d <- result
  }
  
  # Storing the results
  
  return(d_return)
}


### Computation of the test statistic values based on the input
start_time_d_obs  <- Sys.time()
d_observed <- compute_d_permuted(1, gamma,
                                 dat_set_permu,
                                 gurobi_model_permu,
                                 gurobi_model_permu_regul,
                                 all_obs,
                                 permutated_f_m = FALSE)
total_time_d_obs <- Sys.time() - start_time_d_obs
saveRDS(d_observed, file = "d_observed_100_seed_48.rds")

### Test statistic computation based on iteration_number permuted observations
# Note that for a different number of cores it might be that the output is not
# the same, even when seed is set correctly

iteration_number <- 2000
iteration_seq <- seq(1, iteration_number)
no_cores <- 6  # parallel::detectCores(logical = TRUE)
cl <- parallel::makeCluster(no_cores)
doParallel::registerDoParallel(cl)
clusterExport(cl,
              varlist = c("compute_d_permuted","gamma", "dat_set_permu",
                          "gurobi_model_permu", "gurobi_model_permu_regul",
                          "all_obs"),
              envir = environment())
clusterEvalQ(cl,  library(gurobi))

RNGkind("L'Ecuyer-CMRG")
set.seed(858)
s <- .Random.seed
clusterSetRNGStream(cl = cl, iseed = s)

start_time <- Sys.time()
permutation_test_result <- parLapply(cl, iteration_seq, fun = function(x) {
  compute_d_permuted(x, gamma,
                     dat_set_permu,
                     gurobi_model_permu,
                     gurobi_model_permu_regul,
                     all_obs)})
total_time_permu <- Sys.time() - start_time
parallel::stopCluster(cl)

# # not parallel
# start_time <- Sys.time()
# permutation_test <- sapply(iteration_seq, FUN = compute_d_permuted,
#                            gamma = gamma,
#                            dat_set_permu = dat_set_permu,
#                            gurobi_model_permu = gurobi_model_permu,
#                            gurobi_model_permu_regul = gurobi_model_permu_regul,
#                            all_obs = all_obs)
# total_time_permu <- Sys.time() - start_time

# Save objects to a file
saveRDS(permutation_test_result, file = "permutation_test_result_100_seed_48.rds")
saveRDS(total_time_permu, file = "total_time_permu_100_seed_48.rds")

# # Restore the objects
# permutation_test_result <- readRDS("permutation_test_result.rds")


# Saving the result sorted by the computation (iteration_number see line 536)
result_d <- rep(NA, iteration_number) # test statistic with ip and regularisation
result_d_nip <- rep(NA, iteration_number)  # test statistic without ip and with regularisation
result_d_nreg <- rep(NA, iteration_number)  # test statistic with ip and without regularisation
result_d_nip_nreg <- rep(NA, iteration_number)  # test statistic without ip and without regularisation

nfea_result_d <- list() # no result - test statistic with ip and regularisation
nfea_result_d_nip <- list()  # no result - test statistic without ip and with regularisation
nfea_result_d_nreg <- list()  # no result - test statistic with ip and without regularisation
nfea_result_d_nip_nreg <- list()  # no result - test statistic without ip and without regularisation

saving_nfea <- 1
for (i in 1:iteration_number) {
  if (is.numeric(permutation_test_result[[i]]$d)) {
    result_d[i] <- permutation_test_result[[i]]$d
  } else {
    nfea_result_d[[saving_nfea]] <- list(permutation_test_result[[i]]$d)
    saving_nfea <- saving_nfea + 1
  }
  
  if (is.numeric(permutation_test_result[[i]]$d_nip)) {
    result_d[i] <- permutation_test_result[[i]]$d_nip
  } else {
    nfea_result_d[[saving_nfea]] <- list(permutation_test_result[[i]]$d_nip)
    saving_nfea <- saving_nfea + 1
  }
  
  if (is.numeric(permutation_test_result[[i]]$d_nreg)) {
    result_d[i] <- permutation_test_result[[i]]$d_nreg
  } else {
    nfea_result_d[[saving_nfea]] <- list(permutation_test_result[[i]]$d_nreg)
    saving_nfea <- saving_nfea + 1
  }
  
  if (is.numeric(permutation_test_result[[i]]$d_nip_nreg)) {
    result_d[i] <- permutation_test_result[[i]]$d_nip_nreg
  } else {
    nfea_result_d[[saving_nfea]] <- list(permutation_test_result[[i]]$d_nip_nreg)
    saving_nfea <- saving_nfea + 1
  }
}


################################################################################
# TODO
################################################################################
# change name from dup_... to count_... in dat_set /dat_final columns
