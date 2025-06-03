# Load necessary packages ----
library(dplyr)         # Data manipulation toolkit
library(MASS)          # For distribution fitting
library(gtsummary)     # Generate nice statistical tables
library(car)           # Contains various statistical tests
library(lawstat)       # Brunner - Munzel | Contains Brunner - Munzel test
library(DataExplorer)  # For exploratory data analysis

# Set working directory and file paths ----
setwd("/Users/ajisafeadediwura/Desktop/Practical class 3-20250513")

# Read data ----
example_df <- read.csv("distribution.csv", header = TRUE, dec = ',', sep = ";")
factor_df <- read.csv("factor_data.csv")
imputed_df <- read.csv("imputed_data.csv")

# Check data structure ----
str(example_df)
str(factor_df)
str(imputed_df)

# Merge datasets ----
data_for_analysis <- merge(
  factor_df, 
  imputed_df, 
  by = "record_id",        # Merge key
  all = FALSE              # INNER JOIN | Only keep matching entries
)

# Save merged data ----
write.csv(data_for_analysis, "data_for_analysis.csv", row.names = FALSE)  

# 1：outcome ----
# Task 1: Estimate distributions of continuous variables by group (outcome) ----
# Define continuous variables
continuous_vars <- c("lipids1", "lipids2", "lipids3", "lipids4")

# Fit distributions by outcome group
outcome_groups <- unique(data_for_analysis$outcome)
distribution_results <- list()

for (var in continuous_vars) {
  for (group in outcome_groups) {
    subset_data <- data_for_analysis %>% 
      filter(outcome == group) %>% 
      pull({{var}})
    # Check if the subset data is empty
    if (length(subset_data) == 0) {
      warning(paste("variable", var, "in group", group, "No data under"))
      next
    }
    # Check if there are missing values ​​in the data
    if (any(is.na(subset_data))) {
      warning(paste("variable", var, "in group", group, "No data under"))
    }
    # Check if the data is a single value
    if (length(unique(subset_data)) == 1) {
      warning(paste("variable", var, "in group", group, "No data under"))
    }
    
    # Fit three distributions
    fit_normal <- try(fitdistr(subset_data, "normal"), silent = TRUE)
    fit_lognormal <- try(fitdistr(subset_data, "lognormal"), silent = TRUE)
    fit_exponential <- try(fitdistr(subset_data, "exponential"), silent = TRUE)
    
    # Calculate BIC values
    bics <- c(
      normal = if(!inherits(fit_normal, "try-error")) BIC(fit_normal) else NA,
      lognormal = if(!inherits(fit_lognormal, "try-error")) BIC(fit_lognormal) else NA,
      exponential = if(!inherits(fit_exponential, "try-error")) BIC(fit_exponential) else NA
    )
    valid_bics <- bics[!is.na(bics)]
    if (length(valid_bics) > 0) {
      best_dist_index <- which.min(valid_bics)
      best_dist_names <- names(valid_bics)
      dist <- best_dist_names[best_dist_index]
    } else {
      dist <- "unknown"
      warning(paste("variable", var, "in group", group, "The best distribution cannot be determined under"))
    }
    
    distribution_results[[paste0(var, "_outcome_", group)]] <- dist
    
    cat("变量:", var, "| 组别:", group, "| 最佳分布:", dist, "\n")
    cat("BIC值:", paste(names(bics), round(bics, 2), collapse = ", "), "\n\n")
  }
}

# Task 2: Create descriptive statistics table with distribution parameters ----
tbl_by_outcome <- data_for_analysis %>% 
  select(all_of(c("outcome", continuous_vars))) %>% 
  tbl_summary(
    by = outcome,
    statistic = all_continuous() ~ "{mean} ({sd})"  # Mean(SD)
  ) %>% 
  add_p()  # Add p-values

# Add distribution parameters to the table
for (var in continuous_vars) {
  for (group in outcome_groups) {
    dist <- distribution_results[[paste0(var, "_outcome_", group)]]
    if (!is.null(dist) && dist != "") {
      subset_data <- data_for_analysis %>% 
        filter(outcome == group) %>% 
        pull({{var}})
      if (dist == "normal") {
        fit <- fitdistr(subset_data, "normal")
        params <- paste0("μ = ", round(fit$estimate["mean"], 2), 
                         ", σ = ", round(fit$estimate["sd"], 2))
        params_en <- paste0("μ = ", round(fit$estimate["mean"], 2), 
                            ", σ = ", round(fit$estimate["sd"], 2))
      } else if (dist == "lognormal") {
        fit <- fitdistr(subset_data, "lognormal")
        params <- paste0("μ = ", round(fit$estimate["meanlog"], 2), 
                         ", σ = ", round(fit$estimate["sdlog"], 2))
        params_en <- paste0("μ = ", round(fit$estimate["meanlog"], 2), 
                            ", σ = ", round(fit$estimate["sdlog"], 2))
      } else if (dist == "exponential") {
        fit <- fitdistr(subset_data, "exponential")
        params <- paste0("λ = ", round(fit$estimate["rate"], 2))
        params_en <- paste0("λ = ", round(fit$estimate["rate"], 2))
      }
      # Add parameters to the table
      tbl_by_outcome <- tbl_by_outcome %>% 
        modify_footnote(
          all_stat_cols() ~ paste0("distributed: ", dist, "; parameter: ", params, 
                                   "\nDistribution: ", dist, "; Parameters: ", params_en)
        )
    } else {
      warning(paste("variable", var, "in group", group, "The dist value is empty and no parameters are added to the table."))
    }
  }
}

# Save the table
tbl_by_outcome %>% 
  as_gt() %>% 
  gt::gtsave("descriptive_stats_by_outcome.html")

# Task 3: Perform Brunner - Munzel test ----
brunner_munzel_results <- list()

for (var in continuous_vars) {
  group1 <- data_for_analysis %>% 
    filter(outcome == 0) %>% 
    pull({{var}})
  
  group2 <- data_for_analysis %>% 
    filter(outcome == 1) %>% 
    pull({{var}})
  
  # Perform Brunner - Munzel test
  result <- brunner.munzel.test(group1, group2)
  
  # Save results
  brunner_munzel_results[[var]] <- list(
    statistic = result$statistic,
    p_value = result$p.value
  )
  
  cat("variable:", var, "| Brunner - Munzel Test results:\n")
  cat("statistics:", round(result$statistic, 3), "\n")
  cat("p value:", format.pval(result$p.value, digits = 3), "\n\n")
}

# Add p-values to the descriptive table
tbl_by_outcome <- tbl_by_outcome %>% 
  modify_table_body(~(.) %>% dplyr::select(-p.value))
tbl_by_outcome <- tbl_by_outcome %>% 
  modify_header(p.value ~ "**Brunner - Munzel p-value**") %>% 
  add_p(test = all_continuous() ~ "brunner.munzel.test")

# Save the final table
tbl_by_outcome %>% 
  as_gt() %>% 
  gt::gtsave("descriptive_stats_with_bm_test.html")

# Bonus: Find and correct data errors (assuming lipids5 exists with missing values) ----
if ("lipids5" %in% names(data_for_analysis)) {
  # Check missing values
  missing_count <- sum(is.na(data_for_analysis$lipids5))
  cat("lipids5 Number of missing values:", missing_count, "\n")
  
  # Simple mean imputation
  if (missing_count > 0) {
    mean_value <- mean(data_for_analysis$lipids5, na.rm = TRUE)
    data_for_analysis$lipids5[is.na(data_for_analysis$lipids5)] <- mean_value
    
    cat("Mean used", round(mean_value, 2), "Interpolating missing values ​​of lipids5\n")
    
    # Save corrected data
    write.csv(data_for_analysis, "data_for_analysis_imputed.csv", row.names = FALSE)
  }
  
  # Re - run analysis with corrected data
  continuous_vars_updated <- c(continuous_vars, "lipids5")
  
  # Re - fit distributions
  distribution_results_updated <- list()
  
  for (var in continuous_vars_updated) {
    for (group in outcome_groups) {
      subset_data <- data_for_analysis %>% 
        filter(outcome == group) %>% 
        pull({{var}})
      # Check if the subset data is empty
      if (length(subset_data) == 0) {
        warning(paste("variable", var, "in group", group, "No data under"))
        next
      }
      # 检查数据中是否存在缺失值
      if (any(is.na(subset_data))) {
        warning(paste("variable", var, "in group", group, "There are missing values"))
      }
      # 检查数据是否为单一值
      if (length(unique(subset_data)) == 1) {
        warning(paste("variable", var, "in group", group, "The following data is a single value"))
      }
      
      fit_normal <- try(fitdistr(subset_data, "normal"), silent = TRUE)
      fit_lognormal <- try(fitdistr(subset_data, "lognormal"), silent = TRUE)
      fit_exponential <- try(fitdistr(subset_data, "exponential"), silent = TRUE)
      
      bics <- c(
        normal = if(!inherits(fit_normal, "try-error")) BIC(fit_normal) else NA,
        lognormal = if(!inherits(fit_lognormal, "try-error")) BIC(fit_lognormal) else NA,
        exponential = if(!inherits(fit_exponential, "try-error")) BIC(fit_exponential) else NA
      )
      valid_bics <- bics[!is.na(bics)]
      if (length(valid_bics) > 0) {
        best_dist_index <- which.min(valid_bics)
        best_dist_names <- names(valid_bics)
        dist <- best_dist_names[best_dist_index]
      } else {
        dist <- "unknown"
        warning(paste("variable", var, "in group", group, "The best distribution cannot be determined under"))
      }
      
      distribution_results_updated[[paste0(var, "_outcome_", group)]] <- dist
      
      cat("After Correction - Variable:", var, "| Group:", group, "| optimal distribution:", dist, "\n")
    }
  }
  
  # Re - create descriptive table
  tbl_by_outcome_updated <- data_for_analysis %>% 
    select(all_of(c("outcome", continuous_vars_updated))) %>% 
    tbl_summary(
      by = outcome,
      statistic = all_continuous() ~ "{mean} ({sd})"
    ) %>% 
    add_p()
  
  # Add parameter descriptions
  for (var in continuous_vars_updated) {
    for (group in outcome_groups) {
      dist <- distribution_results_updated[[paste0(var, "_outcome_", group)]]
      if (!is.null(dist) && dist != "") {
        subset_data <- data_for_analysis %>% 
          filter(outcome == group) %>% 
          pull({{var}})
        if (dist == "normal") {
          fit <- fitdistr(subset_data, "normal")
          params <- paste0("μ = ", round(fit$estimate["mean"], 2), 
                           ", σ = ", round(fit$estimate["sd"], 2))
          params_en <- paste0("μ = ", round(fit$estimate["mean"], 2), 
                              ", σ = ", round(fit$estimate["sd"], 2))
        } else if (dist == "lognormal") {
          fit <- fitdistr(subset_data, "lognormal")
          params <- paste0("μ = ", round(fit$estimate["meanlog"], 2), 
                           ", σ = ", round(fit$estimate["sdlog"], 2))
          params_en <- paste0("μ = ", round(fit$estimate["meanlog"], 2), 
                              ", σ = ", round(fit$estimate["sdlog"], 2))
        } else if (dist == "exponential") {
          fit <- fitdistr(subset_data, "exponential")
          params <- paste0("λ = ", round(fit$estimate["rate"], 2))
          params_en <- paste0("λ = ", round(fit$estimate["rate"], 2))
        }
        tbl_by_outcome_updated <- tbl_by_outcome_updated %>% 
          modify_footnote(
            all_stat_cols() ~ paste0("distributed: ", dist, "; parameter: ", params, 
                                     "\nDistribution: ", dist, "; Parameters: ", params_en)
          )
      } else {
        warning(paste("variable", var, "in group", group, "The dist value is empty and no parameters are added to the table."))
      }
    }
  }
  
  # Re - perform Brunner - Munzel test
  brunner_munzel_results_updated <- list()
  
  for (var in continuous_vars_updated) {
    group1 <- data_for_analysis %>% 
      filter(outcome == 0) %>% 
      pull({{var}})
    
    group2 <- data_for_analysis %>% 
      filter(outcome == 1) %>% 
      pull({{var}})
    
    result <- brunner.munzel.test(group1, group2)
    
    brunner_munzel_results_updated[[var]] <- list(
      statistic = result$statistic,
      p_value = result$p.value
    )
    
    cat("After Correction - Variable:", var, "| Brunner - Munzel Test results:\n")
    cat("statistics:", round(result$statistic, 3), "\n")
    cat("p value:", format.pval(result$p.value, digits = 3), "\n\n")
  }
  
  # Update and save the table
  tbl_by_outcome_updated <- tbl_by_outcome_updated %>% 
    modify_header(p.value ~ "**Brunner - Munzel p value**\n**Brunner - Munzel p-value**") %>% 
    add_p(test = all_continuous() ~ "brunner.munzel.test")
  
  tbl_by_outcome_updated %>% 
    as_gt() %>% 
    gt::gtsave("descriptive_stats_updated_with_bm_test.html")
}

# Generate final report ----
cat("\nData analysis completed！\n")
cat("The descriptive statistics table has been saved as: descriptive_stats_by_outcome.html\n")
cat("The table with the Brunner-Munzel test has been saved as: descriptive_stats_with_bm_test.html\n")

if (exists("tbl_by_outcome_updated")) {
  cat("The corrected descriptive statistics table has been saved as: descriptive_stats_updated_with_bm_test.html\n")
}
}
