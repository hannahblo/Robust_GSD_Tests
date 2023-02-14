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
source("R/sample_permutation_test.R") # permutation test, sample etc
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






################################################################################
# Visualization of test statistics
################################################################################

# prepare data frames for ggplot
all_test_results = data.frame("reg0" = permutation_test[1,] %>% unlist(), "reg0.25" = permutation_test[2,] %>% unlist(),
                              "reg0.5" = permutation_test[3,] %>% unlist(),"reg0.75" = permutation_test[4,] %>% unlist(),
                              "reg1" = permutation_test[5,] %>% unlist())
all_test_results = all_test_results %>% rev()
d_observed = d_observed %>% unlist()
names(d_observed) = c("eps_0","eps_0.25","eps_0.5","eps_0.75","eps_1")


## theme for horizontal charts
theme_flip <-
  theme(
    axis.text.x = element_text(face = "plain", family = "Roboto Mono", size = 22),
    axis.text.y = element_text(face = "bold", family = "Roboto", size = 26),
    panel.grid.major.x = element_line(color = "grey90", size = .9),
    #panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.text = element_text(family = "Roboto Mono", size = 18),
    legend.title = element_text(face = "bold", size = 18, margin = margin(b = 25))
  )


# nice colors
my_pal <- carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]
# colorblind friendly
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# for geomsegment
d_observed = unlist(d_observed)
d_observed_geom = data.frame(test = seq(1,5), d = d_observed )

legend_title = latex2exp::TeX("Distribution of test statistics ${d}_{I}$, ${d}^{\\epsilon}_{I}$, $\\underline{d}_{I}$ and $\\underline{d}^{\\epsilon}_{I}$")
legend_title = latex2exp::TeX("               Distribution of Test Statistics")# ${d}^{\\epsilon}_{I}$")

y_title = c(latex2exp::TeX("$\\epsilon = 0$"), latex2exp::TeX("$\\epsilon = 0.25$"),latex2exp::TeX("$\\epsilon = 0.5$"),
            latex2exp::TeX("$\\epsilon = 0.75$"),latex2exp::TeX("$\\epsilon = 1$"))


df = stack(all_test_results )

# visualize test statistics including observed ones (see Figure 2 in paper)
figure_2 <- ggplot(df, aes(x = values, y = fct_rev(ind), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1.3, rel_min_height = 0,
                               quantile_lines = TRUE, quantiles = 2,
                               color = "black", alpha = 2.8, size = 0.8,
                               jittered_points = TRUE,
                               position = position_points_jitter(width = 0, height = 0),
                               point_shape = '|', point_size = 2, point_alpha = 1, alpha = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete( name = "Regularization Strength                 ", labels = c("0", "0.25","0.5", "0.75","1")) +
  ggtitle(y_title) +
  xlim(c(-0.125,0.025)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(plot.title = element_text(hjust = 0.8)) +
  scale_fill_viridis_c(name = "Value of Test Statistic", option = "F") +
  coord_cartesian(clip = "off") +
  labs(title = legend_title) +
  theme_ridges(font_size = 18, grid = TRUE) +
  #theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = "bottom", legend.key.size = unit(1.2, 'cm')) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=28), plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.title.y = element_text(size = 20))+
  geom_segment(data = d_observed_geom, aes(x = d, xend = d, y = test,
                                           yend = test +0.99), color = "red", size = 1)

figure_2


# function to compute shares of rejected resampled test statistics
# (see supp. C)
reject_share <- function(gamma, d_res, d_obs){
  d_obs <- rep(d_obs, length(d_res))
  sum(d_obs - d_res > 2*gamma/(1 - gamma))
}

# check for gamma = 0
reject_share(0, d_res = all_test_results$reg1, d_obs = d_observed["eps_1"] )
reject_share(0, d_res = all_test_results$reg0.75, d_obs = d_observed["eps_0.75"] )
reject_share(0, d_res = all_test_results$reg0.5, d_obs = d_observed["eps_0.5"] )
reject_share(0, d_res = all_test_results$reg0.25, d_obs = d_observed["eps_0.25"] )
reject_share(0, d_res = all_test_results$reg0, d_obs = d_observed["eps_0"] )

# define grid of gamma values...
gamma_grid = seq(0,0.04, by = 0.00001) %>% as.list()
#... and lapply share computation over theses grids for all regularizations
rejection_shares = lapply(gamma_grid,
                          FUN = reject_share,
                          d_res = all_test_results$reg1, d_obs = d_observed["eps_1"])
rej_shares_reg1 =  rejection_shares %>% unlist()
df_rej_1 = data.frame("gamma" = gamma_grid %>% unlist, "Rejection share" = rej_shares_reg1, "method" = "eps = 1")
#
rejection_shares = lapply(gamma_grid,
                          FUN = reject_share,
                          d_res = all_test_results$reg0.75, d_obs = d_observed["eps_0.75"])
rej_shares_reg075 =  rejection_shares %>% unlist()
df_rej_075 = data.frame("gamma" = gamma_grid %>% unlist, "Rejection share" = rej_shares_reg075, "method" = "eps = 0.75")
#
rejection_shares = lapply(gamma_grid,
                          FUN = reject_share,
                          d_res = all_test_results$reg0.5, d_obs = d_observed["eps_0.5"] )
rej_shares_reg05 =  rejection_shares %>% unlist()
df_rej_05 = data.frame("gamma" = gamma_grid %>% unlist, "Rejection share" = rej_shares_reg05, "method" = "eps = 0.5")
#
rejection_shares = lapply(gamma_grid,
                          FUN = reject_share,
                          d_res = all_test_results$reg0.25, d_obs = d_observed["eps_0.25"])
rej_shares_reg025 =  rejection_shares %>% unlist()
df_rej_025 = data.frame("gamma" = gamma_grid %>% unlist, "Rejection share" = rej_shares_reg025, "method" = "eps = 0.25")
#
rejection_shares = lapply(gamma_grid,
                          FUN = reject_share,
                          d_res = all_test_results$reg0, d_obs = d_observed["eps_0"] )
rej_shares_reg0 =  rejection_shares %>% unlist()
df_rej_0 = data.frame("gamma" = gamma_grid %>% unlist, "Rejection share" = rej_shares_reg0, "method" = "eps = 0")




################################################################################
# Visualization of rejection shares
################################################################################

# only visualize regularized test statistics
df_rej_all = rbind(df_rej_025, df_rej_05, df_rej_075, df_rej_1)
method = latex2exp::TeX("$\\epsilon = 0$")

par(mar=c(3,4,2,2))

# color grid for regulaizations
pal = brewer.pal(5, "YlGnBu")[c(2,3,4,5)]

# compute p-values
df_rej_all$Rejection_share = 1 - df_rej_all$Rejection.share/nrow(all_test_results)

# Eventually make plot of p-avlues (1 - rejection shares) as function of contamination parameter gamma
# (corresponds to figure 3 in paper)
figure_3 <- ggplot(data = df_rej_all, aes(x = gamma, group = method)) +
  #geom_point(data = df_rej_all, aes(x = gamma, y = Rejection.share, colour = method))  +
  geom_line(data = df_rej_all, aes(x = gamma, y = Rejection_share, colour = method), size = 1.7) +
  labs(color = "Regularization") +
  ylab("p-values") +
  xlab(unname(TeX(c("$\\gamma$")))) +
  theme_minimal() +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=28)) +
  theme(legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.text.align = 0,
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 20)) +
  ylim(c(0, 0.2)) +
  xlim(c(0,0.012)) +
  geom_hline(yintercept=0.05, linetype="dashed", color = "red", size = 2) +
  scale_color_manual(values=pal,
                     labels = unname(TeX(c(#"$$\\epsilon = 0$$",
                       "$$\\epsilon = 0.25$$",
                       "$$\\epsilon = 0.5$$",
                       "$$\\epsilon = 0.75$$",
                       "$$\\epsilon = 1$$"))))

figure_3


