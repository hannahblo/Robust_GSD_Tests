################################################################################
# Session Settings
################################################################################
library(purrr)# for detect_index
library(dplyr) # for group_by
library(slam) # simple triplet matrix
library(tidyr)
library(gurobi) # solving LP
library(readr)
library(forcats)
# for informations about parrallelise the code:
# https://waterprogramming.wordpress.com/2020/03/16/parallel-processing-with-r-on-windows/
# setwd(TODO)

source("constraints_r1_r2_aktuell.R") # contains the functions compute_constraints...
source("compute_permutation_test.R")

################################################################################
# Prepare Data Set: Credit Data
################################################################################


# credit application data, doc: http://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29
credit_appl_data <- read_csv("data/german.data")

#convert to proper data frame
credit_data = as.data.frame(credit_appl_data)
colnames(credit_data) = "one"
credit_data = credit_data %>% separate(one, into = seq(1,21) %>% as.character() )

# select variables credit amount, employment status, savings account and approval (binary)
credit_data = select(credit_data, c("5", "7", "6", "2", "3", "21"))
colnames(credit_data) <- c("credit", "employment", "savings", "duration", "history", "approval")

credit_data$approval = credit_data$approval %>% as.factor()
credit_data$credit = credit_data$credit %>% as.numeric()
#credit_data$age = credit_data$age %>% as.numeric()
credit_data$history = credit_data$history %>% as.ordered %>% fct_rev

credit_data$savings = credit_data$savings %>% factor(ordered = FALSE)
levels(credit_data$savings) = c("< 100","< 500","< 1000",">= 1000","no")
credit_data$employment = credit_data$employment %>% as.ordered
levels(credit_data$employment) = c("no","< 1","< 4","< 7",">= 7")

credit_data_pos = subset(credit_data, subset = approval == 1)
credit_data_neg = subset(credit_data, subset = approval == 2)
# some descriptives
summary(credit_data_neg)
summary(credit_data_pos)

credit_data = select(credit_data, c("credit", "employment", "history", "approval"))

dat_convert_values = credit_data

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
dat_convert_values[["credit"]] <- as.numeric(as.character(
  dat_convert_values[["credit"]]))

# dat_convert_values[["Ausbildung"]][seq(1,10)]
# levels(dat_convert_values[["Ausbildung"]])
# as.ordered(as.numeric(dat_convert_values[["Ausbildung"]]))[seq(1,10)]
dat_convert_values[["employment"]] <- as.ordered(as.numeric(
  dat_convert_values[["employment"]]))

# dat_convert_values[["Gesundheit"]][seq(1,10)]
# levels(dat_convert_values[["Gesundheit"]])
# as.ordered(as.numeric(dat_convert_values[["Gesundheit"]]))[seq(1,10)]
# # Note that here 1 is the best and 6 the worst health status --> needs to be
# # switched --> (7 -)
dat_convert_values[["history"]] <- as.ordered(as.numeric(
  dat_convert_values[["history"]]))

#restrict to variables of interest
dat <- dat_convert_values



# Duplicate handling
dat_dup_divi_appr <- dat %>% group_by_all() %>% count()
dat_dup_without_appr <- duplicated(dat_dup_divi_appr[, c("credit", "employment", "history")])


data_pos <- dat_dup_divi_appr[which(dat_dup_divi_appr$approval == 1),
                             c(1, 2, 3, 5)] # deleting "approval" column
data_pos <- matrix(as.numeric(as.matrix(data_pos)), ncol = 4)

colnames(data_pos) <-  c("numeric", "ordinal_1", "ordinal_2", "count_group_a")


data_neg <- dat_dup_divi_appr[which(dat_dup_divi_appr$approval == 2),
                             c(1, 2, 3, 5)] # deleting "appproval" column
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
rm(dat_dup_divi_fam)
rm(dat_dup_without_fam)
rm(data_pos)
rm(data_neg)
rm(i)
rm(index_max)
rm(index_min)
rm(non_na)


################################################################################
# Randomly sampe a subset of 100 observations in each group
################################################################################
set.seed(48)
dat_set <- sample_random_subset(dat_final, 100)


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
# saveRDS(constraint_r1_values, file = "constraint_r1_values_credit.rds")
# saveRDS(constraint_r2_values, file = "constraint_r2_values_credit.rds")




### Step 2: Compute xi
# In paper: See Chapter 5.3
# Informations about gurobi (page 643ff):
# https://www.gurobi.com/wp-content/plugins/hd_documentations/documentation/9.0/refman.pdf
xi <- compute_xi(constraint_r1_values, constraint_r2_values, dim(dat_set)[1])
saveRDS(xi, "xi_credit.rds")


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
saveRDS(d_observed, file = "d_observed_credit.rds")

### Test statistic computation based on iteration_number permuted observations
# Note that gurobi already parallelises, thus parallelisation does not necessarily
# help to reduce the computation time

iteration_number <- 2000
iteration_seq <- seq(1, iteration_number)
set.seed(2893)
start_time <- Sys.time()
permutation_test <- sapply(iteration_seq, FUN = compute_d,
                           dat_set_permu = dat_set_permu,
                           gurobi_permu = gurobi_permu)
total_time_permu <- Sys.time() - start_time

# Save objects to a file
saveRDS(permutation_test, file = "permutation_test_credit.rds")
saveRDS(total_time_permu, file = "total_time_permu_credit.rds")

