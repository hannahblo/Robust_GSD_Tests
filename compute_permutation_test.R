sample_random_subset <- function(dat_final, size_each_group = 100) {

  dat_set <- dat_final
  cumsum_group_a <- cumsum(dat_set$count_group_a)
  cumsum_group_b <- cumsum(dat_set$count_group_b)
  sum_group_a <- sum(dat_set$count_group_a)
  sum_group_b <- sum(dat_set$count_group_b)
  dat_set$count_group_a <- 0
  dat_set$count_group_b <- 0

  # Compute a random sample
  woman_sample <- sample(seq(0, sum_group_a), size = size_each_group, replace = FALSE)
  for (i in woman_sample) {
    index_sample_value <- which.max(cumsum_group_a >= i)
    dat_set$count_group_a[index_sample_value] <-
      dat_set$count_group_a[index_sample_value] + 1
  }

  man_sample <- sample(seq(0, sum_group_b), size = size_each_group, replace = FALSE)
  for (i in man_sample) {
    index_sample_value <- which.max(cumsum_group_b >= i)
    dat_set$count_group_b[index_sample_value] <-
      dat_set$count_group_b[index_sample_value] + 1
  }


  dat_set$count_all <- dat_set$count_group_a + dat_set$count_group_b
  dat_set <- dat_set[-which(dat_set$count_all == 0), ]
  # add minimal and maximal element row (the last two rows of dat_final)
  # Note that the minimal and maximal value has neither a value for male nor female
  # -> does not increase the sample size
  dat_set[c(dim(dat_set)[1] + 1, dim(dat_set)[1] + 2), ] <-
    dat_final[c(dim(dat_final)[1] - 1, dim(dat_final)[1]), ]
  dat_set$ID <- seq(1, dim(dat_set)[1])

  return(dat_set)
}


compute_xi <- function(constraint_r1_values,
                       constraint_r2_values,
                       number_observations) {

  print("Note that the the last entry of the constraint matrix must correspond to the maximal value of the data set and
  the second last entry of the constraint matrix must correspond to the minimal value of the data set.
        This corresponds to x lower and upper star, see Definition 1 and Definition 3.")

  dim_r2 <- c(max(constraint_r2_values$r_2_i), number_observations)
  dim_r1 <- c(max(constraint_r1_values$r_1_i), number_observations)

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
  # start_time_xi <- Sys.time()
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

  if (xi_gurobi$status == "OPTIMAL") {
    return(xi_gurobi$objval)
  } else {
    return(xi_gurobi)
  }
  # xi <- xi_gurobi$objval
  # xi
  # total_time_xi <- Sys.time() - start_time_xi

}

compute_gurobi_permu <- function(eps_0, eps_1, eps_2, eps_3, eps_4,
                                       xi,
                                       constraint_r1_values,
                                       constraint_r2_values,
                                 number_observations) {

  dim_r2 <- c(max(constraint_r2_values$r_2_i), number_observations)
  dim_r1 <- c(max(constraint_r1_values$r_1_i), number_observations)

  permu_A_r_i <- c(constraint_r1_values$r_1_i, # r_1 part
                   constraint_r2_values$r_2_i + dim_r1[1]) # r_2 part

  permu_A_r_j <- c(constraint_r1_values$r_1_j,
                   constraint_r2_values$r_2_j)

  permu_A_r_v <- c(constraint_r1_values$r_1_v,
                   constraint_r2_values$r_2_v)

  # Note that construction the simple_triplet_matrix needs some minutes
  # permu_A <- slam::simple_triplet_matrix(permu_A_r_i, permu_A_r_j, permu_A_r_v)

  # The right-hand side vector for the linear constraints
  eps_0_xi_r2_constraint <- rep(xi * eps_0, dim_r2[1])
  eps_1_xi_r2_constraint <- rep(xi * eps_1, dim_r2[1])
  eps_2_xi_r2_constraint <- rep(xi * eps_2, dim_r2[1])
  eps_3_xi_r2_constraint <- rep(xi * eps_3, dim_r2[1])
  eps_4_xi_r2_constraint <- rep(xi * eps_4, dim_r2[1])
  if (!any(is.na(constraint_r2_values$xi))) {
    eps_0_xi_r2_constraint[constraint_r2_values$xi] <- 0
    eps_1_xi_r2_constraint[constraint_r2_values$xi] <- 0
    eps_2_xi_r2_constraint[constraint_r2_values$xi] <- 0
    eps_3_xi_r2_constraint[constraint_r2_values$xi] <- 0
    eps_4_xi_r2_constraint[constraint_r2_values$xi] <- 0
  }
  permu_rhs_0 <- c(rep(xi * eps_0, dim_r1[1]), eps_0_xi_r2_constraint)
  permu_rhs_1 <- c(rep(xi * eps_1, dim_r1[1]), eps_1_xi_r2_constraint)
  permu_rhs_2 <- c(rep(xi * eps_2, dim_r1[1]), eps_2_xi_r2_constraint)
  permu_rhs_3 <- c(rep(xi * eps_3, dim_r1[1]), eps_3_xi_r2_constraint)
  permu_rhs_4 <- c(rep(xi * eps_4, dim_r1[1]), eps_4_xi_r2_constraint)

  # Gurobi Model (no regularization)
  # set minimales and maximales Element to 0 and 1 resp (using lower and upper bounds)
  gurobi_model_permu <- list()
  gurobi_model_permu$A <- slam::simple_triplet_matrix(permu_A_r_i, permu_A_r_j, permu_A_r_v)
  # gurobi_model_permu$rhs <- rep(0, dim_r1[1] + dim_r2[1])
  gurobi_model_permu$lb <- c(rep(0, dim_r1[2] - 1), 1)
  gurobi_model_permu$ub <- c(rep(1, dim_r1[2] - 2), 0, 1)
  gurobi_model_permu$vtypes <- c(rep("C", dim_r1[2]))


  gurobi_r2_sense <-  rep(">",  dim_r2[1])
  if (!any(is.na(constraint_r2_values$xi))) {
    gurobi_r2_sense[constraint_r2_values$xi] <- "="
  }
  gurobi_sense <- c(rep(">", dim_r1[1]), gurobi_r2_sense)
  gurobi_model_permu$modelsense <- "min"
  gurobi_model_permu$sense <- gurobi_sense


  return(list(permu_rhs_0 = permu_rhs_0,
              permu_rhs_1 = permu_rhs_1,
              permu_rhs_2 = permu_rhs_2,
              permu_rhs_3 = permu_rhs_3,
              permu_rhs_4 = permu_rhs_4,
         gurobi_model_permu = gurobi_model_permu))
}






compute_d <- function(index,
                      dat_set_permu,
                      gurobi_permu,
                      permutate_obs = TRUE) {



  all_obs <- sum(dat_set_permu$count_all)

  # Compute a random sample
  if (permutate_obs) {
    print(paste0("Bin jetzt bei Permutationsanzahl ", index))
    group_a_index <- sample(seq(1, all_obs),
                                 size = sum(dat_set_permu$count_group_a),
                                 replace = FALSE)
    group_a_sample <- rep(0, all_obs)
    group_a_sample[group_a_index] <- 1
    dat_set_permu <- dat_set_permu
    dat_set_permu$count_group_a <- 0

    index_sample <- 1
    for (i in 1:(dim(dat_set_permu)[1] - 2)) {
      # Beachte, dass für jedes i dat_set_permu$dup_all[i] >= 1 sein muss, da es
      # mindestens eine Beobachtung gibt
      group_a_sample_i <- group_a_sample[seq(index_sample,
                                         index_sample + dat_set_permu$count_all[i] - 1)]
      dat_set_permu$count_group_a[i] <- sum(group_a_sample_i)
      index_sample <- index_sample + dat_set_permu$count_all[i]
    }
    # min und max never observed
    dat_set_permu$count_group_b <- dat_set_permu$count_all - dat_set_permu$count_group_a
    dat_set_permu[c(dim(dat_set_permu)[1] - 1, dim(dat_set_permu)[1]), ] <- 0
  }


  # compute objective function
  gurobi_permu$gurobi_model_permu$obj <-
    (dat_set_permu$count_group_b / all_obs) - (dat_set_permu$count_group_a / all_obs)


  # storing return list
  d_return <- list()

  # Computing d(x,y) for eps_0
  gurobi_permu$gurobi_model_permu$rhs <- gurobi_permu$permu_rhs_0
  result <- gurobi::gurobi(gurobi_permu$gurobi_model_permu)
  if (result$status == "OPTIMAL") {
    d_return$result_eps_0 <- result$objva
  } else {
    d_return$result_eps_0 <- result
  }

  # Computing d(x,y) for eps_1
  gurobi_permu$gurobi_model_permu$rhs <- gurobi_permu$permu_rhs_1
  result <- gurobi::gurobi(gurobi_permu$gurobi_model_permu)
  if (result$status == "OPTIMAL") {
    d_return$result_eps_1 <- result$objva
  } else {
    d_return$result_eps_1 <- result
  }

  # Computing d(x,y) for eps_2
  gurobi_permu$gurobi_model_permu$rhs <- gurobi_permu$permu_rhs_2
  result <- gurobi::gurobi(gurobi_permu$gurobi_model_permu)
  if (result$status == "OPTIMAL") {
    d_return$result_eps_2 <- result$objva
  } else {
    d_return$result_eps_2 <- result
  }

  # Computing d(x,y) for eps_3
  gurobi_permu$gurobi_model_permu$rhs <- gurobi_permu$permu_rhs_3
  result <- gurobi::gurobi(gurobi_permu$gurobi_model_permu)
  if (result$status == "OPTIMAL") {
    d_return$result_eps_3 <- result$objva
  } else {
    d_return$result_eps_3 <- result
  }
  # Computing d(x,y) for eps_4
  gurobi_permu$gurobi_model_permu$rhs <- gurobi_permu$permu_rhs_4
  result <- gurobi::gurobi(gurobi_permu$gurobi_model_permu)
  if (result$status == "OPTIMAL") {
    d_return$result_eps_4 <- result$objva
  } else {
    d_return$result_eps_4 <- result
  }

  return(d_return)
}

