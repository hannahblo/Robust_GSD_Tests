### Session Settings
################################################################################
# setwd("..")
library(purrr)# for detect_indec
library(dplyr)

################################################################################
### Preparing Data Set
################################################################################

# Info about the record: https://www.gesis.org/allbus/allbus
# Here Allbus from 2014, see:
# file:///C:/Users/ru39zel/Downloads/ZA5240_fb.pdf.
# We look at questions "V226" (health), "V102" (education), and.
# "V417" (income)
# one could also look at V418
# "V2" is the identification number of the respondent.


dat_read <- foreign::read.spss("ZA5240_v2-1-0.sav", to.data.frame = TRUE)
dat <- dat_read[which(dat_read$V5 == "SPLIT B: F75B"),
                c("V226", "V102", "V417")]

colnames(dat) <- c("Gesundheit", "Ausbildung", "Einkommen")
sapply(dat, function(y) sum(length(which(is.na(y))))) # number of nas per column
dat <- dat[rowSums(is.na(dat)) == 0,] # deleted NAs
dat$ID <- seq(1, dim(dat)[1], 1)

# View(dat)
dim(dat) # [1] 1340    4
barplot(table(dat[["Gesundheit"]])) # 6 level
barplot(table(dat[["Ausbildung"]])) # 8 level

dat[["Gesundheit"]] <- as.ordered(as.numeric(dat[["Gesundheit"]]))
dat[["Ausbildung"]] <- as.ordered(as.numeric(dat[["Ausbildung"]]))
dat[["Einkommen"]] <- as.numeric(dat[["Einkommen"]])


# Toy Dataset
dat <- dat[seq(1,100), ]
head(dat)




################################################################################
# Berechnung von R1
################################################################################

# Unique combination of ordinal observations
dim(unique(dat[,c("Gesundheit", "Ausbildung")])) # [1] 25  2
numb_unique_ordinal <- dim(unique(dat[,c("Gesundheit", "Ausbildung")])) [1]

# Sorted first by health, then within a health category is sorted by education
# and analogously by income.
sort_dat <- dat[
  order(dat[["Gesundheit"]], dat[["Ausbildung"]], dat[["Einkommen"]]),
  ]
head(sort_dat)


# Computation when in the sorted data a switch in the ordinal data is
value_bevor <- sort_dat[1, c("Gesundheit", "Ausbildung")]
index_switch_ordinal <- 1
i_start_ordinal_groups <- data.frame(ordinal_1 = rep(NA, numb_unique_ordinal),
                                     ordinal_2 = rep(NA, numb_unique_ordinal),
                                     from = rep(NA, numb_unique_ordinal),
                                     to = rep(NA, numb_unique_ordinal))
i_start_ordinal_groups[1, ] <- c(sort_dat[1,c("Gesundheit", "Ausbildung")],
                                 1, NA)

# We go through sort_dat and every time there is a change in the ordinal part,
# we store the value as well as from where to where that combination goes
# Note that this is based on sort_dat and not on dat
for (i in 2:dim(sort_dat)[1]) {
  value_current <- sort_dat[i, c("Gesundheit", "Ausbildung")]
  if (any(value_current != value_bevor)) {
    i_start_ordinal_groups[index_switch_ordinal + 1, ] <- c(value_current,
                                                            i, NA)
    i_start_ordinal_groups[index_switch_ordinal, 4] <- i - 1
    index_switch_ordinal <- index_switch_ordinal + 1
    value_bevor <- value_current
  }
}
# We have to add for the last group the "to" part
i_start_ordinal_groups[numb_unique_ordinal, 4] <- dim(sort_dat)[1]
head(i_start_ordinal_groups)


# Now, we compute r_1
n <- dim(sort_dat)[1]
r_1 <- matrix(rep(0, n * n), nrow = n)
df_index <- 1

# parallel to the calculation of r_1 we calculate df_r1_values, which stores the
# relation r_1, but which can then be sorted again and used for the calculation
# of r_2
df_r1_values <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(df_r1_values) <- c("ID_lower", "ID_upper",
                            "Gesundheit_lower", "Ausbildung_lower",
                            "Gesundheit_upper", "Ausbildung_upper",
                            "difference_numeric")


start_time <- Sys.time()
# We go through every observation
for (i in 1:n) {
  # Value comparing which value is above basis_value
  basis_value <- sort_dat[i, c("Gesundheit", "Ausbildung", "Einkommen")]
  # First, we consider only the ordinal part
  larger_ordinal <- intersect(
    which(i_start_ordinal_groups[, 1] >= basis_value[1, 1]),
    which(i_start_ordinal_groups[, 2] >= basis_value[1, 2]))
  # base_entry corresponds to the
  base_entry <- 0
  index_below <- sort_dat[i, "ID"]
  # We go through every group of ordinal combinations which fulfill being larger
  # then basis value.
  for (j in larger_ordinal) {
    # First entry where also the numeric part is larger
    first_income_above <-
      purrr::detect_index(sort_dat[seq(from = i_start_ordinal_groups[j, 3],
                                to = i_start_ordinal_groups[j,  4]),
                            "Einkommen"],
                   function(x) {x >= as.numeric(basis_value[3])})

    # if there does not exist a first entry, because every numeric value is
    # below, then first_income_above = 0
    if (!(first_income_above == 0)) {
      # Based on sort_dat every observation which lies in the same ordinal group
      # but has a higher index than first_income_above is larger then base_value
      # (in all components)
      index_above <- seq(i_start_ordinal_groups[j,  3] + first_income_above - 1,
                         i_start_ordinal_groups[j,  4], 1)
      # R_1 is sorted by the order of the original saving of the data, thus we
      # have to use the "ID" of sort_dat and not the index of sort_dat
      r_1[i, c(sort_dat[index_above, "ID"])] <- 1
      base_entry <- base_entry + length(index_above)
      # The following part is only for saving df_r1_values, here we save all
      # the combinations with value, ID and difference in the numeric part
      for (index_above_df in index_above) {
        df_r1_values[df_index, ] <-
          c(index_below, sort_dat[index_above_df, "ID"],
            basis_value[["Gesundheit"]], basis_value[["Ausbildung"]],
            sort_dat[index_above_df, "Gesundheit"],
            sort_dat[index_above_df, "Ausbildung"],
            as.numeric(sort_dat[index_above_df, "Einkommen"]) -
              as.numeric(basis_value[3]))
        df_index <- df_index + 1
      }
    }
  }
  # here we have a +1, because <= also counts itself and thus, at this position
  # here is a -1 in the entry
  # TODO
  # STOP: DA STIMMT WAS NICHT; WEIL EIGENTLICH IST DAS +! BEREITS IN DER BASE_
  # ENTRY VARIABLE DRIN
  r_1[i, index_below] <- -base_entry + 1
}
end_time <- Sys.time()
duration_time <- end_time - start_time
duration_time

r_1
rowSums(r_1)
head(df_r1_values)



### Berechnung von R2
################################################################################
# Berechnung von R2
################################################################################

# Sorted first by health_lower, then within a health_lower category is sorted by
# education_lower, then within this group by health_upper (descending), then
# by education_lower (descending) and finally again in decreasing by
# difference_numeric
# ANMERKUNG: DAS ZWISCHEN DURCH DESCENDING KOMMT VON EINER FRÜHEREN IDEE; DIE
# ICH VERWORFEN WERDEN KANN. DAS HAT ALSO FÜR DAS KOMMENDE KEINEN EINFLUSS
sort_df_r1 <- df_r1_values[
  order(df_r1_values[["Gesundheit_lower"]], df_r1_values[["Ausbildung_lower"]],
        -as.numeric(as.factor(df_r1_values[["Gesundheit_upper"]])),
        -as.numeric(as.factor(df_r1_values[["Ausbildung_upper"]])),
        df_r1_values[["difference_numeric"]]
  ), ]
head(sort_df_r1)

# Unique combination of ordinal observations
dim(unique(sort_df_r1[, c("Gesundheit_lower", "Ausbildung_lower",
                           "Gesundheit_upper", "Ausbildung_upper")]))
numb_unique_ordinal <- dim(unique(sort_df_r1[, c("Gesundheit_lower",
                                                 "Ausbildung_lower",
                                                 "Gesundheit_upper",
                                                 "Ausbildung_upper")])) [1]

value_bevor <- sort_df_r1[1, c("Gesundheit_lower", "Ausbildung_lower",
                               "Gesundheit_upper", "Ausbildung_upper")]


# Computation when in the sorted data a switch in the ordinal data is
index_switch_ordinal <- 1
i_df_ordinal_groups <- data.frame("Gesundheit_lower" = rep(NA, numb_unique_ordinal),
                                  "Ausbildung_lower" = rep(NA, numb_unique_ordinal),
                                  "Gesundheit_upper" = rep(NA, numb_unique_ordinal),
                                  "Ausbildung_upper" = rep(NA, numb_unique_ordinal),
                                  from = rep(NA, numb_unique_ordinal),
                                  to = rep(NA, numb_unique_ordinal))
i_df_ordinal_groups[1, ] <- c(sort_df_r1[1, c("Gesundheit_lower", "Ausbildung_lower",
                                            "Gesundheit_upper", "Ausbildung_upper")], 1, NA)

# We go through sort_df_r1 and every time there is a change in the ordinal part,
# we store the value as well as from where to where that combination goes
for (i in 2:dim(sort_df_r1)[1]) {
  # This code is analogously to the one in the r_1 part
  value_current <- sort_df_r1[i, c("Gesundheit_lower", "Ausbildung_lower",
                                 "Gesundheit_upper", "Ausbildung_upper")]
  if (any(value_current != value_bevor)) {
    i_df_ordinal_groups[index_switch_ordinal + 1, ] <- c(value_current, i, NA)
    i_df_ordinal_groups[index_switch_ordinal, 6] <- i - 1
    index_switch_ordinal <- index_switch_ordinal + 1
    value_bevor <- value_current
  }
}
# We have to add for the last group the "to" part
i_df_ordinal_groups[numb_unique_ordinal, 6] <- dim(sort_df_r1)[1]
head(i_df_ordinal_groups)


# Now, we compute r_1
m <- dim(sort_df_r1)[1]
r_2 <- matrix(rep(0, n * m), nrow = m)

start_time <- Sys.time()
# We go through every existing relation in r_1 which is given by sort_df_r1
for (i in 1:dim(sort_df_r1)[1]) { # DAS HIER KANN MAN GUT PARALLELISIERN
  basis_value <- sort_df_r1[i, c("Gesundheit_lower", "Ausbildung_lower",
                                 "Gesundheit_upper", "Ausbildung_upper",
                                 "difference_numeric")]
  basis_value_id <- sort_df_r1[i, c("ID_lower", "ID_upper")]
  larger_ordinal <- Reduce(intersect,
    list(which(i_df_ordinal_groups[, 1] >= basis_value[1, 1]),
         which(i_df_ordinal_groups[, 2] >= basis_value[1, 2]),
         which(i_df_ordinal_groups[, 3] <= basis_value[1, 3]),
         which(i_df_ordinal_groups[, 4] <= basis_value[1, 4])))
  below_entry <- 0
  for (j in larger_ordinal) {
    # erster Index in der
    difference_drueber <-
      purrr::detect_index(sort_df_r1[seq(from = i_df_ordinal_groups[j, "from"],
                                       to = i_df_ordinal_groups[j,  "to"]),
                                   "difference_numeric"],
                          function(x) {x >= as.numeric(basis_value[5])})

    if (!(difference_drueber == 0)) {

      index_above <- seq(i_df_ordinal_groups[j,  "from"] + difference_drueber - 1,
                         i_df_ordinal_groups[j,  "to"],
                         1)
      r_2[i, sort_df_r1[index_above, 1]] <- 1
      r_2[i, sort_df_r1[index_above, 2]] <- 1
      below_entry <- below_entry + length(index_above)
    }
  }
  # TODO
  # STOP: DA STIMMT WAS NICHT
  r_2[i, basis_value_id["ID_lower"]] <- -(below_entry/2) + 1
  r_2[i, basis_value_id["ID_upper"]] <- -(below_entry/2) + 1
}
end_time <- Sys.time()
duration_time <- end_time - start_time
duration_time
head(r_2)
r_2
rowSums(r_2)


################################################################################
### Fragen
################################################################################
# Frage GS: Warnung beim Einlesen?
# Gibt genau diesen Datensatz auch auf Englisch, wäre das sinnvoll?
# Ergibt es Sinn, dass (1,1) im Ordinalen nicht existiert --> Ich glaube, dass
#     ich Ausbildung falschherum kodiert habe
# Frage CJ: Wie lang braucht dein Algorithmus bei 100? Bei mir sind das nur
#     wenige Sekunden derzeit.... (Ich glaube, aber das ich etwas übersehen
#     habe)


