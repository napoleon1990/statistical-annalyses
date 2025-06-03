# statistical-annalyses
Statistical Analysis Assignment: 
## Overview
This R script performs a comprehensive statistical analysis, including data merging, distribution fitting, descriptive statistics, and nonparametric hypothesis testing. The project focuses on analyzing lipid-related variables (lipids1–lipids4) grouped by an outcome variable, with additional steps for error handling and missing data imputation.

# Datasets
The analysis uses three input files:

distribution.csv: Contains lipid-related continuous variables.

factor_data.csv: Includes categorical variables and the outcome.

imputed_data.csv: Supplementary variables with imputed values.

The merged dataset, data_for_analysis.csv, combines all necessary variables for analysis.

# Dependencies
R Version: 4.0.0 or higher recommended.

Required Packages:

dplyr (data manipulation)

MASS (distribution fitting)

gtsummary (descriptive tables)

car / lawstat (statistical tests, including Brunner-Munzel)

DataExplorer (exploratory analysis)

Install packages using:

r
Copy
Download
install.packages(c("dplyr", "MASS", "gtsummary", "car", "lawstat", "DataExplorer"))
## Workflow
# 1. Data Preparation
Reads and merges datasets by record_id.

Saves the combined dataset as data_for_analysis.csv.

# 2. Distribution Analysis
For each lipid variable (lipids1–lipids4), grouped by outcome:

Fits normal, lognormal, and exponential distributions.

Selects the best-fitting distribution using BIC (Bayesian Information Criterion).

Records the optimal distribution for each variable-group pair.

# 3. Descriptive Statistics
Generates summary tables stratified by outcome.

Includes distribution parameters (e.g., mean ± SD for normal distributions).

Exports tables in HTML format:

descriptive_stats_by_outcome.html (basic statistics).

descriptive_stats_with_bm_test.html (includes Brunner-Munzel results).

# 4. Brunner-Munzel Test
Performs nonparametric Brunner-Munzel tests for each lipid variable.

Appends test statistics and p-values to summary tables.

# 5. Error Handling (Bonus)
Checks for missing values in lipids5 (if present).

Imputes missing values using the mean.

Re-runs analyses with imputed data.

## Key Functions & Code
Distribution Fitting & BIC Comparison
r
Copy
Download
fit_normal <- try(fitdistr(subset_data, "normal"), silent = TRUE)  
bics <- c(normal = if(!inherits(fit_normal, "try-error")) BIC(fit_normal) else NA)  
best_dist <- names(which.min(bics))  
Brunner-Munzel Test
r
Copy
Download
result <- brunner.munzel.test(group1, group2)  
Missing Value Imputation
r
Copy
Download
if (sum(is.na(data$lipids5)) > 0) {  
  data$lipids5[is.na(data$lipids5)] <- mean(data$lipids5, na.rm = TRUE)  
}  
# Output Files
File	Description
data_for_analysis.csv	Merged dataset for analysis.
descriptive_stats_by_outcome.html	Basic descriptive statistics.
descriptive_stats_with_bm_test.html	Summary table with Brunner-Munzel test results.
descriptive_stats_updated_with_bm_test.html	Updated table after missing value imputation (if applicable).
## How to Run
Set Up:

Ensure all datasets are in the working directory.

Install required packages (see Dependencies).

## Execution:

Open the R script in RStudio or an R environment.

Run the entire script (Ctrl + A → Run in RStudio).

## Review Results:

Check generated HTML files for statistical summaries.

Examine data_for_analysis.csv for the merged dataset.

# Notes
If lipids5 has missing values, the script will impute them and re-run analyses.

For large datasets, consider optimizing runtime by parallelizing distribution fitting.

For questions or issues, please refer to the script comments or contact the author.
