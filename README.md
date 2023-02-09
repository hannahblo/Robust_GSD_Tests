# Analysis of Dermatological Symptoms, Credit Approval Data and Multi-Dimensional Poverty Measurement

## Introduction
This anonymous repository contains R-code and data sets corresponding to the "Robust Statistical Comparison of Random Variables with Locally Varying Scale of Measurement" article. We apply the introduced tests on three examples: dermatological symptoms, credit approval data, and multi-dimensional poverty measurement.

The structure of the repository is as follows:
- File poverty_permutation_tests.R analyzes is an multi-dimensional poverty analysis.
- File credit_permutation_tests.R analyzes the credit approval data.
- File derma_permutation_tests.R analyzes the dermatological symptoms data.
- Folder R/ contains the needed functions (permutation test, sampling, definition of the constraint matrix etc) for the analysis.
- Folder data/ contains the data sets corresponding to the dermatological symptoms and credit approval data analysis. The data set used for the multi-dimensional poverty measurement is freely accessible, but only after registration at the following [online portal](https://search.gesis.org/research_data/ZA5240) (accessed: 08.02.2023). Please ensure that the downloaded data set is in data/ file.
- File _setup_session.R installs all (except for gurobi) needed R-packages.

The code was tested with
- R version 4.2.1
- R version 4.2.2

on

- Linux Ubuntu 20.04.5
- Windows 10 

## Setup
First, please install all necessary R-packages:
- For the computation of the linear programs, we used the R interface of gurobi optimizer, see [here](https://www.gurobi.com/). This is a commercial
solver that offers a free academic licenses which can be found [here](https://www.gurobi.com/features/academic-named-user-license/). To install this package, please follow the instructions there. A documentation can be found [here](https://www.gurobi.com/wp-content/plugins/hd_documentations/documentation/9.0/refman.pdf) (page 643ff).
- Afterwards, please install all dependencies by sourcing the file _setup_session.R.

Then download the following files and save them in a folder named 'R':
- constraints_r1_r2.R
- sample_permutation_test.R

Afterwards, please download the data sets and save them in a folder named 'data':
- dermatology.data
- credit.data
- the data set corresponding to the poverty analysis is freely accessible, but only after registration at the following [online portal](https://search.gesis.org/research_data/ZA5240) (accessed: 08.02.2023). Please download the file ZA5240 v2-2-0.sav (5.31MB) there.


In order to reproduce the papers' key results (and visualizations thereof) further download these scripts and save in respective folder:
- poverty_permutation_tests.R (estimated runtime: 4 days)
- credit_permutation_tests.R (estimated runtime: 4 days)
- derma_permutation_tests.R (estimated runtime: 4 days)
Running these three files reproduces our result.
