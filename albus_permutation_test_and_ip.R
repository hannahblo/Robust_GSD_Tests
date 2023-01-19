################################################################################
# Session Settings
################################################################################
library(purrr)# for detect_index
library(dplyr) # for group_by
library(slam) # simple triplet matrix
library(gurobi) # solving LP
library(parallel) # for mclapply (only works for linux in a parallel version)
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

dat_read <- foreign::read.spss("ZA5240_v2-1-0.sav", to.data.frame = TRUE)
dat <- dat_read[which(dat_read$V5 == "SPLIT B: F75B"),
                c("V226", "V102", "V417", "V81", "V7")]

colnames(dat) <- c("Gesundheit", "Ausbildung", "Einkommen", "Geschlecht",
                   "O_W_Deutschland")
dat[["Gesundheit"]] <- 7 - as.numeric(dat[["Gesundheit"]])
sapply(dat, function(y) sum(length(which(is.na(y))))) # number of nas per column
dat <- dat[rowSums(is.na(dat)) == 0,] # deleted NAs
dat <- dat[which(dat$O_W_Deutschland == "NEUE BUNDESLAENDER"), ]

dat[["Gesundheit"]] <- as.ordered(as.numeric(dat[["Gesundheit"]]))
dat[["Ausbildung"]] <- as.ordered(as.numeric(dat[["Ausbildung"]]))
dat[["Einkommen"]] <- as.numeric(dat[["Einkommen"]])

# Duplicates
dat_dup_divi_sex <- dat %>% group_by_all() %>% count()
dat_dup_without_sex <- duplicated(dat_dup_divi_sex[, seq(1,3)])
# dat_dup_divi_sex[dat_dup_without_sex, ]
# sum(!dat_dup_without_sex)

data_female <- dat_dup_divi_sex[which(dat_dup_divi_sex$Geschlecht == "WEIBLICH"),
                                c(1,2,3, 6)]
data_female <- matrix(as.numeric(as.matrix(data_female)), ncol = 4)
colnames(data_female) <-  c("ordinal_1", "ordinal_2", "numeric", "dup_female")

data_male <- dat_dup_divi_sex[which(dat_dup_divi_sex$Geschlecht == "MAENNLICH"),
                              c(1,2,3, 6)]
data_male <- matrix(as.numeric(as.matrix(data_male)), ncol = 4)
colnames(data_male) <-  c("ordinal_1", "ordinal_2", "numeric", "dup_male")

dat_final <- merge(x = data_female, y = data_male,
                   by = c("ordinal_1", "ordinal_2", "numeric"),
                   all.x = TRUE, all.y = TRUE)
dat_final[is.na(dat_final)] <- 0
dat_final$dup_all <- dat_final$dup_female + dat_final$dup_male
dat_final$ID <- seq(1:dim(dat_final)[1])
View(dat_final)
dim(dat_final)

# Add minimal and maximal at the bottom of the matrix
dat_final[dim(dat_final)[1] + 1, ] <- c(min(dat_final$ordinal_1),
                                        min(dat_final$ordinal_2),
                                        min(dat_final$numeric) - 1,
                                        0, 0, 1,
                                        max(dat_final$ID) + 1)
dat_final[dim(dat_final)[1] + 1, ] <- c(max(dat_final$ordinal_1),
                                        max(dat_final$ordinal_2),
                                        max(dat_final$numeric) + 1,
                                        0, 0, 1,
                                        max(dat_final$ID) + 1)





################################################################################
# Different Data Set Situations to go on
# Important: Use only one dat_set to go on!
################################################################################
## Entire Data Set
dat_set <- dat_final


## 100 Woman and 100 Man (randomly sampled)
set.seed(148)
dat_set <- dat_final
cumsum_female <- cumsum(dat_set$dup_female)
cumsum_male <- cumsum(dat_set$dup_male)
sum_female <- sum(dat_set$dup_female)
sum_male <- sum(dat_set$dup_male)
dat_set$dup_female <- 0
dat_set$dup_male <- 0

# Compute a random sample
woman_sample <- sample(seq(0, sum_female), size = 100, replace = FALSE)
index_sample <- 1
for (i in woman_sample) {
  index_sample_value <- which.max(cumsum_female >= i)
  dat_set$dup_female[index_sample_value] <-
    dat_set$dup_female[index_sample_value] + 1
}

man_sample <- sample(seq(0, sum_male), size = 100, replace = FALSE)
index_sample <- 1
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
View(dat_set)
dim(dat_set)
sum(dat_set$dup_all)


## Short Test Version
dat_set <- dat_final[c(5, 10, 150, 24, 50, 88, 182), ]
dat_set$ID <- seq(1, dim(dat_set)[2])

################################################################################
# Information about Computation Times etc w.r.t. the above data sets
################################################################################
## Entire Data Set

# constraint_r1_values
# computation time:
# disk space:
# dimensions:

# constraint_r2_values
# computation time:
# disk space:
# dimensions

# xi
# computation time:
# disk space:

# 50 runs of the permutation tests
# computation time:
# disk space:





## 100 Woman and 100 Man (randomly sampled)

# General Notes: Problem by running this test in parallel: Not enough disk space?
# --> Maybe presolving could help a bit
# Further, I have the feeling that gurobi is already parallising. Thus, If I add
#  mclapply it does not really help...

# constraint_r1_values
# computation time: Time difference of 12.02571 secs
# disk space: 732.2 kB
# dimensions: 7604  168
# Saved: in computation_result_100

# constraint_r2_values
# computation time: Time difference of 47.26213 mins
# disk space: 5005.5 MB
# dimensions: 5223397     168
# Saved: in computation_result_100

# xi
# computation time: Time difference of 2.632521 mins
# value: 0.002463054
# Saved: in computation_result_100

# 20 runs of the permutation test (return entire gurobi object)
# Problem: only two kernels produced results. I guess that this is due to disk
#   space problem as this does not occur when a smaller sample is used. Further
#   I could increase the number of kernel used by reducing the disk space of
#   the input
# computation time: Time difference of 12.39729 mins
# values: Hier wurde das gesamte Gurobi Object zurück gegeben, d.h. die Speicher
#    zeit ist deutlich länger als später, wenn nur der Wert zurück gegeben wird
#    Nur zwei Kerne haben funktioniert -> daher oft nur NULL als Rückgabe
# used cores:6 (only 2 worked properly the rest returned NULL)
# Warning message:
# In mclapply(X = seq(1, number_iterations), FUN = compute_d_permuted,  :
#               scheduled cores 1, 4, 5, 6 did not deliver results, all values of the jobs will be affected
# Saved: in computation_result_100 -> permutation_test_100_20_6.rds

# 20 runs of the permutation tests (return only value)
# computation time: Time difference of 12.56364 mins
# values: only two kernels worked -> 2/3 of the values are NULL
# used cores: 6 (only 2 worked properly the rest returned NULL)
# Warning message:
#   In mclapply(X = seq(1, number_iterations), FUN = compute_d_permuted,  :
#                 scheduled cores 3, 4, 5, 6 did not deliver results, all values of the jobs will be affected
# Saved: in computation_result_100 -> permutation_test_100_20_6_only_result.rds

# 20 runs of the permutation tests (return only value)
# computation time: Time difference of 30.66759 mins
# used cores: 1
# Saved: in computation_result_100 -> permutation_test_100_20_1_only_result.rds
# Note: Irgendwie verwendet er immer zweimal den selben seed. Das sollte aber gut
#   zu ändern sein :)

# 20 runs of the permutation tests (return entire gurobi object)
# computation time: Time difference of 30.83245 mins
# used cores: 1
# Saved: in computation_result_100 -> permutation_test_100_20_1.rds
# Note: Irgendwie verwendet er immer zweimal den selben seed. Das sollte aber gut
#   zu ändern sein :)


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





# (!) Note that the file name must be changed accoring to the used data set
# .._100 if the data set with 100 sample of woman / man is used
# .._all if the entire sample is used

# (!) Die Daten werden in einem extra Ordner jeweils abgespeichert. In die Ordner
# mit dem Namen computation_results_..
# .._100 if the data set with 100 sample of woman / man is used
# .._all if the entire sample is used

setwd()
# Save objects to a file
saveRDS(constraint_r1_values, file = "constraint_r1_values_100.rds")
saveRDS(constraint_r2_values, file = "constraint_r2_values_100.rds")
# Restore the objects
constraint_r1_values <- readRDS("constraint_r1_values_100.rds")
constraint_r2_values <- readRDS("constraint_r2_values_100.rds")

dim_r2 <- c(max(constraint_r2_values$r_2_i), dim(dat_set)[1])
dim_r1 <- c(max(constraint_r1_values$r_1_i), dim(dat_set)[1])



################################################################################
# Compute xi
# In paper: See Chapter 5.3
# Informations about gurobi (page 643ff): https://www.gurobi.com/wp-content/plugins/hd_documentations/documentation/9.0/refman.pdf
################################################################################

xi_r2_constraint <- rep(0, dim_r2[1])
if (!is.na(constraint_r2_values$xi)) {
  xi_r2_constraint[constraint_r2_values$xi] <- -1
}

# simple_triplet matrix i=row, j=column, v=value
xi_A_r_i <- c(constraint_r1_values$r_1_i, seq(1, dim_r1[1]), # r_1 part
              constraint_r2_values$r_2_i + dim_r1[1], seq(dim_r1[1] + 1, dim_r1[1] + dim_r2[1])) # r_2 part

xi_A_r_j <- c(constraint_r1_values$r_1_j, rep(dim_r1[2] + 1, dim_r1[1]),
             constraint_r2_values$r_2_j, rep(dim_r2[2] + 1, dim_r2[1]))

xi_A_r_v <- c(constraint_r1_values$r_1_v, rep(-1, dim_r1[1]),
              constraint_r2_values$r_2_v, xi_r2_constraint)


# Note that construction the simple_triplet_matrix needs some minutes
start_time_xi <- Sys.time()
gurobi_model_xi <- list()
gurobi_model_xi$A <- slam::simple_triplet_matrix(xi_A_r_i, xi_A_r_j, xi_A_r_v)
gurobi_model_xi$rhs <- rep(0, max(xi_A_r_i))
gurobi_model_xi$lb <- rep(0, dim_r1[2] + 1)
gurobi_model_xi$ub <- rep(1, dim_r1[2] + 1)
gurobi_model_xi$vtypes <- c(rep("C", dim_r1[2] + 1))

gurobi_model_xi$obj <- c(rep(0, dim_r1[2]), 1)
gurobi_model_xi$modelsense <- "max"
gurobi_model_xi$sense <- rep(">",  max(xi_A_r_i))



xi_gurobi <- gurobi::gurobi(gurobi_model_xi)
xi <- xi_gurobi$objval
total_time_xi <- Sys.time() - start_time_xi


# Save object to a file
saveRDS(xi_gurobi, file = "xi_gurobi_100.rds")
# Restore the object
xi_gurobi <- readRDS("xi_gurobi_100.rds")
xi <- xi_gurobi$objval


################################################################################
# Compute Permutation test
# In paper: See Chapter 5.2
################################################################################
all_obs <- sum(dat_set$dup_all)
eps <- 0.6


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
delta_r2_constraint <- rep(0, dim_r2[1])
if (!is.na(constraint_r2_values$xi)) {
  delta_r2_constraint[delta_r2_constraint$xi] <- xi * eps
}
permu_rhs <- c(rep(xi * eps, dim_r1[1]), delta_r2_constraint)

# Gurobi Model
gurobi_model_permu <- list()
gurobi_model_permu$A <- permu_A
gurobi_model_permu$rhs <- permu_rhs
gurobi_model_permu$lb <- rep(0, dim_r1[2])
gurobi_model_permu$ub <- rep(1, dim_r1[2])
gurobi_model_permu$vtypes <- c(rep("C", dim_r1[2]))

gurobi_model_permu$modelsense <- "max"
gurobi_model_permu$sense <- rep(">",  dim(permu_A)[1])

# Function to compute d based on one permutation
compute_d_permuted <- function(index,
                               dat_set_distr_fm, gurobi_model_permu, all_obs) {

  print(paste0("Bin jetzt bei Permutationsanzahl ", index))

  # Compute a random sample
  woman_sample <- sample(c(0,1), size = all_obs, replace = TRUE)
  dat_set_permu <- dat_set_distr_fm
  dat_set_permu$dup_female <- 0

  index_sample <- 1
  for (i in 1:dim(dat_set_permu)[1]) {
    # Beachte, dass für jedes i dat_set_permu$dup_all[i] >= 1 sein muss, da es
    # mindestens eine Beobachtung gibt
    woman_sample_i <- woman_sample[seq(index_sample,
                                       index_sample + dat_set_permu$dup_all[i] - 1)]
    dat_set_permu$dup_female <- sum(woman_sample_i)
  }
  dat_set_permu$dup_male <- dat_set_permu$dup_all - dat_set_permu$dup_female



  # Computing d(x,y) based on dat_set_permu (first set up gurobi model)
  gurobi_model_permu_inner <- gurobi_model_permu
  gurobi_model_permu_inner$obj <- (dat_set_permu$dup_female / all_obs) -
    (dat_set_permu$dup_male / all_obs)

  permu_gurobi <- gurobi::gurobi(gurobi_model_permu_inner)
  return(permu_gurobi$objval)
}

# Note that for a different number of cores it might be that the output is not
# the same, even when seed is set correctly
number_iterations <- 20
dat_set_distr_fm <- dat_set[, c("dup_female", "dup_male", "dup_all")]
RNGkind("L'Ecuyer-CMRG")
set.seed <- 1986
start_time_permu <- Sys.time()
permutation_test <- mclapply(X = seq(1, number_iterations),
                           FUN = compute_d_permuted,
                           dat_set_distr_fm = dat_set_distr_fm,
                           gurobi_model_permu = gurobi_model_permu,
                           all_obs = all_obs,
                           mc.set.seed = TRUE,
                           mc.cores = 1) # detectCores() - 2
total_time_permu <- Sys.time() - start_time_permu

# Save objects to a file
saveRDS(permutation_test, file = "permutation_test_100_20_1.rds")
# Restore the objects
permutation_test <- readRDS("permutation_test_100.rds")


################################################################################
# Fragen / TODOs
################################################################################
# Bei der Berechnung von xi: was steht da in den Constraints der
# Theortisch hinzugefügten minimalen und maximalen Elementen

# Analoge Frage bei Berechnung von xi, was ist mit den maximalen
# Elementen

# @Georg: Du meintest, dass du eine Presolve-gurobi R-Version gefunden hast.
# Könntest du mir den Namen der Funktion am Montag geben?

# Fehlt (außer IP) noch etwas bei der Berechnung?

# Was für ein Einfluss hat es, dass die numerische Variable auch zusammen
# gefasst wurde?

# Wäre es ok, wenn ich den Code statt einmal 1000 Permutation, 5 mal 200
# Permutationen berechnen lasse? (natürlich mit unterschiedlichen Seeds)


# TODO Hannah nächste Woche
# IP Programmierung
# Komplett durchlaufen lassen
# seed setzen bei mclapply nachsehen -> irgendwie immer zweimal hintereinander
#     der selbe


