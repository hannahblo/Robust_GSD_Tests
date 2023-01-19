

## library(dplyr)
library(readr)

# credit application data, doc: http://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29
credit_appl_data <- read_csv("data/german.data")

# dermatology data, doc: https://archive.ics.uci.edu/ml/datasets/dermatology
derma_data_raw <- read_csv("data/dermatology.data")

derma_data <- derma_data_raw[,c(34,1,2,3,4,11)] %>% as.data.frame()

colnames(derma_data) <- c("age", "erythema", "scaling", "d-broders", "itching", "family_history")  

derma_data_pos = subset(derma_data, subset = family_history == 1)
derma_data_neg = subset(derma_data, subset = family_history == 0)
