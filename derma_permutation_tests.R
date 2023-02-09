# This file corresponds to the dermatological symptoms analysis.






################################################################################
# Session Settings
################################################################################
library(purrr)# for detect_index
library(dplyr) # for group_by
library(slam) # simple triplet matrix
library(gurobi) # solving LP
library(readr)  # data preparation
library(forcats) # data preparation

source("R/constraints_r1_r2.R") # contains the functions compute_constraints...
source("R/sample_permutation_test.R")
################################################################################
# Prepare Data Set: Derma Data
################################################################################


# dermatology data, doc: https://archive.ics.uci.edu/ml/datasets/dermatology
# (accessed: 07.02.2023)
derma_data_raw <- read_csv("data/dermatology.data")

derma_data <- derma_data_raw[,c(34,1,2,3,4,11)] %>% as.data.frame()

colnames(derma_data) <- c("age", "erythema", "scaling", "d-broders", "itching", "family_history")

derma_data_pos = subset(derma_data, subset = family_history == 1)
derma_data_neg = subset(derma_data, subset = family_history == 0)


dat_convert_values <- derma_data

# deleting NAs
# here we are deleting the entry with minimal "Einkommen" value 37. We have to
# delete the nas beford considering the rest, because else NA would be added as
# a further factor in as.order() later
non_na <- c()
for (i in seq(1, dim(dat_convert_values)[1])) {
  if (!any(is.na(dat_convert_values[i,]))) {
    non_na <- c(non_na, i)
  }
}
dat_convert_values <- dat_convert_values[non_na, ]

# Converting the variables of interest into numeric and order modes
dat_convert_values[["age"]] <- as.numeric(as.character(
  dat_convert_values[["age"]]))

dat_convert_values[["erythema"]] <- as.ordered(as.numeric(
  dat_convert_values[["erythema"]]))


dat_convert_values[["itching"]] <- as.ordered(as.numeric(
  dat_convert_values[["itching"]]))

# Restrict to variables of interest
dat <- dat_convert_values[ , c("age",  "erythema", "itching", "family_history")]



# Duplication handling
dat_dup_divi_fam <- dat %>% group_by_all() %>% count()
dat_dup_without_fam <- duplicated(dat_dup_divi_fam[, c("age",  "erythema", "itching")])


data_pos <- dat_dup_divi_fam[which(dat_dup_divi_fam$family_history == 1),
                                c(1, 2, 3, 5)] # deleting "fam history" column
data_pos <- matrix(as.numeric(as.matrix(data_pos)), ncol = 4)

colnames(data_pos) <-  c("numeric", "ordinal_1", "ordinal_2", "count_group_a")


data_neg <- dat_dup_divi_fam[which(dat_dup_divi_fam$family_history == 0),
                             c(1, 2, 3, 5)] # deleting "fam history" column
data_neg <- matrix(as.numeric(as.matrix(data_neg)), ncol = 4)

colnames(data_neg) <-  c("numeric", "ordinal_1", "ordinal_2", "count_group_b")


dat_final <- merge(x = data_pos, y = data_neg,
                   by = c("ordinal_1", "ordinal_2", "numeric"),
                   all.x = TRUE, all.y = TRUE)
dat_final[is.na(dat_final)] <- 0
dat_final$count_all <- dat_final$count_group_a + dat_final$count_group_b
dat_final$ID <- seq(1:dim(dat_final)[1])


# View(dat_final)
# dim(dat_final)
# min(dat_final$numeric)
# max(dat_final$numeric)

index_max <- which(dat_final$numeric == max(dat_final$numeric))[1]
# dat_final[index_max, ]
index_min <- which(dat_final$numeric == min(dat_final$numeric))[1]
# dat_final[index_min, ]

# Add minimal and maximal at the bottom of the matrix

# ATTENTION: It is very important for the following analysis that the
# the input at the second largest row is the minimal value and the largest row
# represents the maximal value
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



################################################################################
# Randomly sampe a subset of 100 observations in each group
################################################################################
set.seed(48)
dat_set <- sample_random_subset(dat_final,
                                size_each_group = NULL,
                                size_group_a = 46,
                                size_group_b = 100)


################################################################################
# Conduct the permutation test
################################################################################
### Step 1: Compute the constraints given by R1 and R2
# see Article Equation (12) in Section 7
start_time_r1 <- Sys.time()
constraint_r1_values <- compute_constraints_r1(dat_set)
total_time_r1 <- Sys.time() - start_time_r1


start_time_r2 <- Sys.time()
constraint_r2_values <- compute_constraints_r2(dat_set,
                                               constraint_r1_values$df_r1_values)
total_time_r2 <- Sys.time() - start_time_r2
# saveRDS(constraint_r1_values, file = "constraint_r1_values_derma.rds")
# saveRDS(constraint_r2_values, file = "constraint_r2_values_derma.rds")




### Step 2: Compute xi
# In paper: See Section 5.3, Proposition 1 and 2
# Informations about gurobi (page 643ff):
# https://www.gurobi.com/wp-content/plugins/hd_documentations/documentation/9.0/refman.pdf
# (accessed: 07.02.2023)
xi <- compute_xi(constraint_r1_values, constraint_r2_values, dim(dat_set)[1])
# saveRDS(xi, "xi_derma.rds")


### Step 3: Compute the permutation test based on four different eps values
# In paper: See Section 5.2
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
                        permutate_obs = FALSE,
                        reverse_objective = FALSE)
d_observed <- compute_d(1,
                        dat_set_permu,
                        gurobi_permu,
                        permutate_obs = FALSE,
                        reverse_objective = TRUE)
total_time_d_obs <- Sys.time() - start_time_d_obs
saveRDS(d_observed, file = "d_observed_derma.rds")

### Test statistic computation based on iteration_number permuted observations
# Note that gurobi already parallels, thus parallelism does not necessarily
# help to reduce the computation time

iteration_number <- 1000
iteration_seq <- seq(1, iteration_number)
set.seed(2893)
start_time <- Sys.time()
permutation_test <- sapply(iteration_seq, FUN = compute_d,
                           dat_set_permu = dat_set_permu,
                           gurobi_permu = gurobi_permu)
total_time_permu <- Sys.time() - start_time

# saveRDS(permutation_test, file = "permutation_test_derma.rds")
# saveRDS(total_time_permu, file = "total_time_permu_derma.rds")

