################################################################################
# Session Settings
################################################################################
library(purrr)# for detect_index
library(dplyr) # for group_by
library(slam) # simple triplet matrix
library(gurobi) # solving LP
# library(parallel) # for mclapply (only works for linux in a parallel version)
# library(doParallel)
# for informations about parrallelise the code:
# https://waterprogramming.wordpress.com/2020/03/16/parallel-processing-with-r-on-windows/
# setwd(TODO)
source("constraints_r1_r2_aktuell.R") # contains the functions compute_constraints...
source("compute_permutation_test.R")

################################################################################
# Prepare Data Set
################################################################################
# Info about the record: https://www.gesis.org/allbus/allbus
# Here Allbus from 2014, see: ZA5240_fb.pdf on github
# We look at questions "V226" (health), "V102" (education), and.
# "V417" (income), "V81" (sex), "V7" (Erhebungsgebiet Ost -West)
# one could also look at V418
# "V2" is the identification number of the respondent.

setwd("data/")
dat_read <- foreign::read.spss("albus.sav", to.data.frame = TRUE,
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
colnames(data_female) <-  c("ordinal_1", "ordinal_2", "numeric", "count_group_a")

data_male <- dat_dup_divi_sex[which(dat_dup_divi_sex$Geschlecht == "MAENNLICH"),
                              c(1, 2, 3, 5)]  # deleting "Geschlect" column
data_male <- matrix(as.numeric(as.matrix(data_male)), ncol = 4)
colnames(data_male) <-  c("ordinal_1", "ordinal_2", "numeric", "count_group_b")

dat_final <- merge(x = data_female, y = data_male,
                   by = c("ordinal_1", "ordinal_2", "numeric"),
                   all.x = TRUE, all.y = TRUE)
dat_final[is.na(dat_final)] <- 0
dat_final$count_all <- dat_final$count_group_a + dat_final$count_group_b
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
# Randomly sampe a subset of 100 observations in each group
################################################################################
set.seed(48)
dat_set <- sample_random_subset(dat_final)


################################################################################
# Conduct the permutation test
################################################################################
### Step 1: Compute the constraints given by R1 and R2
# see Paper page 7, Equation (11)
start_time_r1 <- Sys.time()
constraint_r1_values <- compute_constraints_r1(dat_set)
total_time_r1 <- Sys.time() - start_time_r1


start_time_r2 <- Sys.time()
constraint_r2_values <- compute_constraints_r2(dat_set,
                                               constraint_r1_values$df_r1_values)
total_time_r2 <- Sys.time() - start_time_r2
# saveRDS(constraint_r1_values, file = "constraint_r1_values_albus.rds")
# saveRDS(constraint_r2_values, file = "constraint_r2_values_albus.rds")




### Step 2: Compute xi
# In paper: See Chapter 5.3
# Informations about gurobi (page 643ff):
# https://www.gurobi.com/wp-content/plugins/hd_documentations/documentation/9.0/refman.pdf
xi <- compute_xi(constraint_r1_values, constraint_r2_values, dim(dat_set)[1])
# saveRDS(xi, "xi_albus.rds")


### Step 3: Compute the permutation test based on four different eps values
# In paper: See Chapter 5.2
eps_0 <- 0
eps_1 <- 0.25
eps_2 <- 0.5
eps_3 <- 0.75
eps_4 <- 1

# the general gurobi model
gurobi_permu <- compute_gurobi_permu(eps_0, eps_1, eps_2, eps_3, eps_4,
                                     xi,
                                     constraint_r1_values,
                                     constraint_r2_values,
                                     dim(dat_set)[1])
# Reduce dat_set only to the part needed in the permutation test
dat_set_permu <- dat_set[, c("count_group_a", "count_group_b", "count_all")]

# Computation of the test statistic values based on the input
start_time_d_obs  <- Sys.time()
d_observed <- compute_d(1,
                        dat_set_permu,
                        gurobi_permu,
                        permutate_obs = FALSE)
total_time_d_obs <- Sys.time() - start_time_d_obs
# saveRDS(d_observed, file = "d_observed_albus.rds")

### Test statistic computation based on iteration_number permuted observations
# Note that gurobi already parallelises, thus parallelisation does not necessarily
# help to reduce the computation time

iteration_number <- 1000
iteration_seq <- seq(1, iteration_number)
set.seed(2893)
start_time <- Sys.time()
permutation_test <- sapply(iteration_seq, FUN = compute_d,
                           dat_set_permu = dat_set_permu,
                           gurobi_permu = gurobi_permu)
total_time_permu <- Sys.time() - start_time

# Save objects to a file
saveRDS(permutation_test, file = "permutation_test_albus.rds")
saveRDS(total_time_permu, file = "total_time_permu_albus.rds")
