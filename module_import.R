#########################################################
# Enhanced utils.R - Advanced data quality assessment and utility functions
#########################################################

#' Load data from a CSV or Excel file
#' 
#' @param file_path Path to the file to load
#' @param sheet_name Sheet name (for Excel files)
#' @return A data frame containing the loaded data
#' @export
load_data <- function(file_path, sheet_name = NULL) {
  file_ext <- tools::file_ext(file_path)
  
  if (file_ext == "csv") {
    tryCatch({
      data <- readr::read_csv(file_path, col_types = readr::cols(.default = readr::col_guess()))
      return(as.data.frame(data))
    }, error = function(e) {
      stop(paste("Error loading CSV file:", e$message))
    })
  } else if (file_ext %in% c("xlsx", "xls")) {
    tryCatch({
      if (!is.null(sheet_name)) {
        data <- openxlsx::read.xlsx(file_path, sheet = sheet_name)
      } else {
        data <- openxlsx::read.xlsx(file_path)
      }
      return(data)
    }, error = function(e) {
      stop(paste("Error loading Excel file:", e$message))
    })
  } else {
    stop("Unsupported file format. Please upload a CSV or Excel file.")
  }
}

#' Validate data structure for network analysis
#' 
#' @param data Data frame to validate
#' @param required_columns Vector of column names that must be present
#' @param id_column Name of the ID column
#' @param group_columns Vector of column names containing grouping variables
#' @return List with validation results and messages
#' @export
validate_data <- function(data, required_columns = NULL, id_column = NULL, 
                          group_columns = NULL) {
  results <- list(
    valid = TRUE,
    messages = character(0)
  )
  
  # Check if data exists and is a data frame
  if (is.null(data) || !is.data.frame(data)) {
    results$valid <- FALSE
    results$messages <- c(results$messages, "Data is not a valid data frame.")
    return(results)
  }
  
  # Check minimum number of rows
  if (nrow(data) < 5) {
    results$valid <- FALSE
    results$messages <- c(results$messages, 
                          "Data has fewer than 5 rows. More data is needed for robust analysis.")
  }
  
  # Check for required columns
  if (!is.null(required_columns)) {
    missing_cols <- required_columns[!required_columns %in% names(data)]
    if (length(missing_cols) > 0) {
      results$valid <- FALSE
      results$messages <- c(results$messages, 
                            paste("Missing required columns:", 
                                  paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Check ID column
  if (!is.null(id_column) && id_column %in% names(data)) {
    # Check for duplicate IDs
    if (any(duplicated(data[[id_column]]))) {
      results$valid <- FALSE
      results$messages <- c(results$messages, 
                            paste("Duplicate values found in ID column:", id_column))
    }
  }
  
  # Check group columns
  if (!is.null(group_columns)) {
    present_group_cols <- group_columns[group_columns %in% names(data)]
    for (col in present_group_cols) {
      # Check if there's at least two groups in each grouping variable
      if (length(unique(na.omit(data[[col]]))) < 2) {
        results$messages <- c(results$messages, 
                              paste("Warning: Group column", col, 
                                    "has fewer than 2 groups. This may not be useful for comparison."))
      }
    }
  }
  
  # Check if there are numeric columns for analysis
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) < 3) {
    results$valid <- FALSE
    results$messages <- c(results$messages, 
                          "Insufficient numeric columns for network analysis. Need at least 3.")
  }
  
  return(results)
}

#' Perform basic imputation using column means
#' 
#' @param data Data frame with missing values
#' @param columns Columns to impute (defaults to all numeric columns)
#' @return Data frame with imputed values
#' @export
impute_with_mean <- function(data, columns = NULL) {
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  # Create a copy of the data
  imputed_data <- data
  
  # Impute each column with its mean
  for (col in columns) {
    if (any(is.na(imputed_data[[col]]))) {
      col_mean <- mean(imputed_data[[col]], na.rm = TRUE)
      imputed_data[is.na(imputed_data[[col]]), col] <- col_mean
    }
  }
  
  return(imputed_data)
}

#' Enhanced data quality analysis with comprehensive statistical assessment
#' 
#' @param data Data frame to analyze
#' @param region_columns Vector of region column names
#' @param group_columns Vector of group column names (optional)
#' @return List with comprehensive quality metrics and recommendations
#' @export
analyze_data_quality <- function(data, region_columns, group_columns = NULL) {
  region_data <- data[, region_columns, drop = FALSE]
  if (!is.null(group_columns) && length(group_columns) > 0 && all(group_columns %in% names(data))) {
    # Use the first group column for sample size calculation (most relevant for analysis)
    primary_group_col <- group_columns[1]
    
    # Calculate sample sizes per group
    group_counts <- table(data[[primary_group_col]])
    min_group_size <- min(group_counts)
    max_group_size <- max(group_counts)
    avg_group_size <- mean(group_counts)
    
    # Use the minimum group size as the effective sample size for statistical considerations
    effective_sample_size <- min_group_size
    
    # Initialize quality metrics
    quality_metrics <- list(
      sample_size = nrow(data),
      missing_values = sum(is.na(region_data)),
      missing_percent = sum(is.na(region_data)) / (nrow(region_data) * ncol(region_data)) * 100,
      outliers_count = 0,
      outliers_by_region = NULL,
      normality_tests = list(),
      distribution_summary = list(),
      group_balance = NULL,
      data_range_summary = list(),
      correlation_feasibility = list(),
      recommended_threshold = 0.3,
      recommended_method = "standard",
      recommendations = list(),
      warnings = character(),
      statistical_power = list()
    )
  } else {
    # If no group columns provided, use the original calculation
    quality_metrics <- list(
      sample_size = nrow(data),
      missing_values = sum(is.na(region_data)),
      missing_percent = sum(is.na(region_data)) / (nrow(region_data) * ncol(region_data)) * 100,
      outliers_count = 0,
      outliers_by_region = NULL,
      normality_tests = list(),
      distribution_summary = list(),
      group_balance = NULL,
      data_range_summary = list(),
      correlation_feasibility = list(),
      recommended_threshold = 0.3,
      recommended_method = "standard",
      recommendations = list(),
      warnings = character(),
      statistical_power = list()
    )
  }  
  
  
  # Analyze group balance if group columns provided
  if (!is.null(group_columns) && length(group_columns) > 0) {
    group_balance <- list()
    
    for (group_col in group_columns) {
      if (group_col %in% names(data)) {
        group_counts <- table(data[[group_col]], useNA = "ifany")
        group_balance[[group_col]] <- list(
          counts = as.list(group_counts),
          min_size = min(group_counts),
          max_size = max(group_counts),
          balance_ratio = min(group_counts) / max(group_counts),
          cv = sd(group_counts) / mean(group_counts)  # Coefficient of variation
        )
      }
    }
    quality_metrics$group_balance <- group_balance
  }
  
  # Enhanced outlier detection using multiple methods
  outlier_counts <- numeric(length(region_columns))
  names(outlier_counts) <- region_columns
  outlier_details <- list()
  
  for (i in seq_along(region_columns)) {
    col <- region_columns[i]
    x <- region_data[[col]]
    x <- x[!is.na(x)]
    
    if (length(x) > 3) {
      # Method 1: IQR method
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      iqr_outliers <- x[x < lower_bound | x > upper_bound]
      
      # Method 2: Z-score method (|z| > 3)
      z_scores <- abs((x - mean(x)) / sd(x))
      z_outliers <- x[z_scores > 3]
      
      # Method 3: Modified Z-score using median absolute deviation
      mad_val <- mad(x)
      if (mad_val > 0) {
        modified_z <- 0.6745 * (x - median(x)) / mad_val
        mad_outliers <- x[abs(modified_z) > 3.5]
      } else {
        mad_outliers <- numeric(0)
      }
      
      # Combine outlier detection methods
      all_outliers <- unique(c(iqr_outliers, z_outliers, mad_outliers))
      outlier_counts[i] <- length(all_outliers)
      
      outlier_details[[col]] <- list(
        iqr_outliers = length(iqr_outliers),
        z_outliers = length(z_outliers),
        mad_outliers = length(mad_outliers),
        total_unique_outliers = length(all_outliers),
        outlier_percentage = (length(all_outliers) / length(x)) * 100
      )
    }
  }
  
  quality_metrics$outliers_count <- sum(outlier_counts)
  quality_metrics$outliers_by_region <- outlier_counts
  quality_metrics$outlier_details <- outlier_details
  
  # Enhanced normality testing with multiple tests
  normality_results <- list()
  
  for (col in region_columns) {
    x <- region_data[[col]]
    x <- x[!is.na(x)]
    
    if (length(x) >= 3 && length(x) <= 5000) {
      tests_results <- list()
      
      # Shapiro-Wilk test
      if (length(x) >= 3 && length(x) <= 5000) {
        sw_result <- tryCatch({
          shapiro.test(x)
        }, error = function(e) NULL)
        
        if (!is.null(sw_result)) {
          tests_results$shapiro_wilk <- list(
            statistic = sw_result$statistic,
            p_value = sw_result$p.value,
            normal = sw_result$p.value > 0.05
          )
        }
      }
      
      # Kolmogorov-Smirnov test against normal distribution
      if (length(x) > 7) {
        ks_result <- tryCatch({
          # Suppress warnings about ties in the Kolmogorov-Smirnov test
          suppressWarnings(ks.test(x, "pnorm", mean(x), sd(x)))
        }, error = function(e) NULL)
        
        if (!is.null(ks_result)) {
          tests_results$kolmogorov_smirnov <- list(
            statistic = ks_result$statistic,
            p_value = ks_result$p.value,
            normal = ks_result$p.value > 0.05
          )
        }
      }
      
      # Anderson-Darling test (if available)
      ad_result <- tryCatch({
        if (requireNamespace("nortest", quietly = TRUE)) {
          nortest::ad.test(x)
        } else {
          NULL
        }
      }, error = function(e) NULL)
      
      if (!is.null(ad_result)) {
        tests_results$anderson_darling <- list(
          statistic = ad_result$statistic,
          p_value = ad_result$p.value,
          normal = ad_result$p.value > 0.05
        )
      }
      
      # Skewness and kurtosis
      skewness_val <- tryCatch({
        if (requireNamespace("e1071", quietly = TRUE)) {
          e1071::skewness(x)
        } else {
          # Manual calculation
          n <- length(x)
          mean_x <- mean(x)
          sd_x <- sd(x)
          sum((x - mean_x)^3) / (n * sd_x^3)
        }
      }, error = function(e) NA)
      
      kurtosis_val <- tryCatch({
        if (requireNamespace("e1071", quietly = TRUE)) {
          e1071::kurtosis(x)
        } else {
          # Manual calculation
          n <- length(x)
          mean_x <- mean(x)
          sd_x <- sd(x)
          (sum((x - mean_x)^4) / (n * sd_x^4)) - 3
        }
      }, error = function(e) NA)
      
      tests_results$moments <- list(
        skewness = skewness_val,
        kurtosis = kurtosis_val,
        skew_normal = abs(skewness_val) < 1,  # Generally acceptable range
        kurt_normal = abs(kurtosis_val) < 1   # Generally acceptable range
      )
      
      # Consensus normality assessment
      normality_votes <- sum(c(
        ifelse(!is.null(tests_results$shapiro_wilk), tests_results$shapiro_wilk$normal, NA),
        ifelse(!is.null(tests_results$kolmogorov_smirnov), tests_results$kolmogorov_smirnov$normal, NA),
        ifelse(!is.null(tests_results$anderson_darling), tests_results$anderson_darling$normal, NA),
        ifelse(!is.na(tests_results$moments$skew_normal), tests_results$moments$skew_normal, NA),
        ifelse(!is.na(tests_results$moments$kurt_normal), tests_results$moments$kurt_normal, NA)
      ), na.rm = TRUE)
      
      total_tests <- sum(!is.na(c(
        ifelse(!is.null(tests_results$shapiro_wilk), tests_results$shapiro_wilk$normal, NA),
        ifelse(!is.null(tests_results$kolmogorov_smirnov), tests_results$kolmogorov_smirnov$normal, NA),
        ifelse(!is.null(tests_results$anderson_darling), tests_results$anderson_darling$normal, NA),
        tests_results$moments$skew_normal,
        tests_results$moments$kurt_normal
      )))
      
      tests_results$consensus_normal <- (normality_votes / total_tests) > 0.5
      tests_results$normality_confidence <- normality_votes / total_tests
      
      normality_results[[col]] <- tests_results
    }
  }
  
  quality_metrics$normality_tests <- normality_results
  
  # Data range and distribution summary
  range_summary <- list()
  for (col in region_columns) {
    x <- region_data[[col]]
    x <- x[!is.na(x)]
    
    if (length(x) > 0) {
      range_summary[[col]] <- list(
        min = min(x),
        max = max(x),
        mean = mean(x),
        median = median(x),
        sd = sd(x),
        cv = sd(x) / mean(x),  # Coefficient of variation
        range = max(x) - min(x),
        iqr = IQR(x),
        q25 = quantile(x, 0.25),
        q75 = quantile(x, 0.75)
      )
    }
  }
  quality_metrics$data_range_summary <- range_summary
  
  # Statistical power assessment for correlation detection
  n <- quality_metrics$sample_size
  n_regions <- length(region_columns)
  
  # Estimate power to detect correlations of various effect sizes
  power_analysis <- list()
  effect_sizes <- c(0.3, 0.5, 0.7)  # Small, medium, large
  
  for (r in effect_sizes) {
    # Calculate power using Fisher's z-transformation approximation
    z_r <- 0.5 * log((1 + r) / (1 - r))
    se <- 1 / sqrt(n - 3)
    z_critical <- qnorm(0.975)  # Two-tailed test at α = 0.05
    
    power <- 1 - pnorm(z_critical - abs(z_r) / se) + pnorm(-z_critical - abs(z_r) / se)
    power_analysis[[paste0("r_", r)]] <- power
  }
  
  quality_metrics$statistical_power <- power_analysis
  
  # Correlation feasibility assessment
  quality_metrics$correlation_feasibility <- list(
    sample_to_variables_ratio = n / n_regions,
    sufficient_for_standard = n >= 10,
    sufficient_for_partial = n >= (n_regions + 5),
    sufficient_for_robust = n >= 15,
    partial_correlation_feasible = n > n_regions,
    regularization_recommended = n < (n_regions * 2)
  )
  
  # Generate recommendations based on comprehensive analysis
  recommendations <- generate_advanced_recommendations(quality_metrics, n_regions)
  quality_metrics$recommendations <- recommendations$methods
  quality_metrics$recommended_threshold <- recommendations$threshold
  quality_metrics$recommended_method <- recommendations$primary_method
  quality_metrics$warnings <- recommendations$warnings
  
  return(quality_metrics)
}

#' Generate advanced method recommendations based on comprehensive data quality
#' 
#' @param quality_metrics Results from analyze_data_quality
#' @param n_regions Number of brain regions
#' @return List with method recommendations and rationale
generate_advanced_recommendations <- function(quality_metrics, n_regions) {
  n <- quality_metrics$sample_size
  recommendations <- list(
    methods = list(),
    threshold = 0.3,
    primary_method = "standard",
    warnings = character()
  )
  
  # Sample size based recommendations
  if (n < 10) {
    recommendations$threshold <- 0.6
    recommendations$primary_method <- "standard"
    recommendations$methods$correlation_type <- "standard"
    recommendations$methods$correlation_method <- "spearman"
    recommendations$methods$use_robust <- TRUE
    recommendations$warnings <- c(recommendations$warnings, 
                                  "Very small sample size. Results should be interpreted with caution.")
  } else if (n < 20) {
    recommendations$threshold <- 0.4
    recommendations$primary_method <- ifelse(n >= n_regions, "partial", "standard")
    recommendations$methods$correlation_type <- recommendations$primary_method
    recommendations$methods$use_regularization <- TRUE
  } else if (n < 50) {
    recommendations$threshold <- 0.3
    recommendations$primary_method <- "partial"
    recommendations$methods$correlation_type <- "partial"
    recommendations$methods$use_regularization <- n < (n_regions * 1.5)
  } else {
    recommendations$threshold <- 0.25
    recommendations$primary_method <- "partial"
    recommendations$methods$correlation_type <- "partial"
    recommendations$methods$use_regularization <- FALSE
  }
  
  # Normality-based recommendations
  if (length(quality_metrics$normality_tests) > 0) {
    non_normal_count <- sum(sapply(quality_metrics$normality_tests, function(x) {
      !x$consensus_normal
    }))
    non_normal_percent <- non_normal_count / length(quality_metrics$normality_tests) * 100
    
    if (non_normal_percent > 60) {
      recommendations$methods$correlation_method <- "spearman"
      recommendations$methods$use_robust <- TRUE
      recommendations$warnings <- c(recommendations$warnings, 
                                    "High proportion of non-normal distributions detected.")
    } else if (non_normal_percent > 30) {
      recommendations$methods$correlation_method <- "spearman"
    } else {
      recommendations$methods$correlation_method <- "pearson"
    }
  }
  
  # Outlier-based recommendations
  total_outlier_percent <- (quality_metrics$outliers_count / (n * n_regions)) * 100
  if (total_outlier_percent > 5) {
    recommendations$methods$use_robust <- TRUE
    recommendations$methods$correlation_method <- "spearman"
    recommendations$warnings <- c(recommendations$warnings, 
                                  paste0("High outlier burden detected (", 
                                         round(total_outlier_percent, 1), "% of data points)."))
  }
  
  # Missing data recommendations
  if (quality_metrics$missing_percent > 5) {
    recommendations$methods$impute_missing <- TRUE
    if (quality_metrics$missing_percent > 15) {
      recommendations$methods$use_robust <- TRUE
      recommendations$warnings <- c(recommendations$warnings, 
                                    "Substantial missing data detected. Consider data collection quality.")
    }
  }
  
  # Multiple testing recommendations
  n_comparisons <- (n_regions * (n_regions - 1)) / 2
  if (n_comparisons > 50) {
    recommendations$methods$perform_significance <- TRUE
    recommendations$methods$apply_fdr <- TRUE
    
    if (n_comparisons > 300) {
      recommendations$methods$significance_threshold <- 0.01
    } else if (n_comparisons > 100) {
      recommendations$methods$significance_threshold <- 0.02
    } else {
      recommendations$methods$significance_threshold <- 0.05
    }
  }
  
  # Group balance warnings
  if (!is.null(quality_metrics$group_balance)) {
    for (group_name in names(quality_metrics$group_balance)) {
      balance_info <- quality_metrics$group_balance[[group_name]]
      if (balance_info$min_size < 5) {
        recommendations$warnings <- c(recommendations$warnings, 
                                      paste0("Very small group size detected in ", group_name, 
                                             " (minimum n=", balance_info$min_size, ")."))
      }
      if (balance_info$balance_ratio < 0.5) {
        recommendations$warnings <- c(recommendations$warnings, 
                                      paste0("Unbalanced group sizes in ", group_name, 
                                             " (ratio=", round(balance_info$balance_ratio, 2), ")."))
      }
    }
  }
  
  # Statistical power warnings
  if (quality_metrics$statistical_power$r_0.3 < 0.8) {
    recommendations$warnings <- c(recommendations$warnings, 
                                  "Low statistical power to detect medium effect sizes. Consider larger sample.")
  }
  
  return(recommendations)
}

#' Create a correlation network from brain region data
#' 
#' @param data Data frame containing brain region data
#' @param region_columns Vector of column names containing brain region data
#' @param correlation_threshold Threshold for including edges in the network
#' @param correlation_method Method for correlation calculation (default "pearson")
#' @param correlation_type Type of correlation to use ("standard" or "partial")
#' @param use_robust Whether to use robust correlation (less sensitive to outliers)
#' @param use_regularization Whether to use regularization for partial correlation
#' @return List containing correlation matrix and graph object
#' @export
# Fixed create_correlation_network function with improved edge list handling
#' Create a correlation network from brain region data
#' 
#' @param data Data frame containing brain region data
#' @param region_columns Vector of column names containing brain region data
#' @param correlation_threshold Threshold for including edges in the network
#' @param correlation_method Method for correlation calculation (default "pearson")
#' @param correlation_type Type of correlation to use ("standard" or "partial")
#' @param use_robust Whether to use robust correlation (less sensitive to outliers)
#' @param use_regularization Whether to use regularization for partial correlation
#' @return List containing correlation matrix and graph object
#' @export
create_correlation_network <- function(data, region_columns, 
                                       correlation_threshold = 0.3,
                                       correlation_method = "pearson",
                                       correlation_type = "standard",
                                       use_robust = FALSE,
                                       use_regularization = TRUE) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  # Add input validation to handle list parameters
  # This fixes the "invalid 'type' (list) of argument" error
  if (is.list(correlation_method)) {
    warning("correlation_method is a list, using default 'pearson'")
    correlation_method <- "pearson"
  }
  
  if (is.list(correlation_type)) {
    warning("correlation_type is a list, using default 'standard'")
    correlation_type <- "standard"
  }
  
  if (is.list(correlation_threshold)) {
    warning("correlation_threshold is a list, using default 0.3")
    correlation_threshold <- 0.3
  }
  
  if (is.list(use_robust)) {
    warning("use_robust is a list, using default FALSE")
    use_robust <- FALSE
  }
  
  if (is.list(use_regularization)) {
    warning("use_regularization is a list, using default TRUE")
    use_regularization <- TRUE
  }
  
  # Extract region data
  region_data <- data[, region_columns, drop = FALSE]
  
  # Compute correlation matrix based on method
  if (correlation_type == "standard") {
    if (!use_robust) {
      # Standard correlation
      correlation_matrix <- cor(region_data, use = "pairwise.complete.obs", 
                                method = correlation_method)
    } else {
      # FIXED: More robust implementation of robust correlation
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("Package 'MASS' is required for robust correlation")
      }
      
      n_regions <- length(region_columns)
      correlation_matrix <- matrix(1, n_regions, n_regions)
      rownames(correlation_matrix) <- region_columns
      colnames(correlation_matrix) <- region_columns
      
      # First approach: Try with complete data to avoid individual pairwise calculations
      tryCatch({
        # Remove rows with any missing values
        complete_data <- region_data[complete.cases(region_data), ]
        
        # Only proceed if we have enough complete cases
        if (nrow(complete_data) >= 3) {
          # Calculate robust correlation matrix directly
          rob_cor <- MASS::cov.rob(complete_data)
          
          # Extract the correlation matrix from the result
          rob_cor_matrix <- cov2cor(rob_cor$cov)
          
          # Assign to correlation_matrix
          correlation_matrix <- rob_cor_matrix
        } else {
          # Fall back to pairwise approach if not enough complete cases
          stop("Not enough complete cases for direct robust correlation")
        }
      }, error = function(e) {
        # Fall back to pairwise approach if direct approach fails
        warning(paste("Falling back to pairwise robust correlation:", e$message))
        
        # Calculate correlations pairwise
        for (i in 1:n_regions) {
          for (j in 1:i) {
            if (i != j) {
              # Extract the two columns
              col_i <- region_data[[region_columns[i]]]
              col_j <- region_data[[region_columns[j]]]
              
              # Skip if insufficient data
              valid_indices <- !is.na(col_i) & !is.na(col_j)
              if (sum(valid_indices) < 3) {
                correlation_matrix[i, j] <- 0
                correlation_matrix[j, i] <- 0
                next
              }
              
              # Create a data frame with only these two columns (complete cases only)
              pair_data <- data.frame(
                x = col_i[valid_indices],
                y = col_j[valid_indices]
              )
              
              # Calculate robust correlation
              tryCatch({
                # Try different robust correlation methods
                if (nrow(pair_data) >= 5) {
                  # Preferred method with enough data
                  rob_pair <- MASS::cov.rob(pair_data)
                  # Get correlation from covariance
                  r <- rob_pair$cov[1, 2] / sqrt(rob_pair$cov[1, 1] * rob_pair$cov[2, 2])
                } else if (nrow(pair_data) >= 3) {
                  # Simpler method for very small samples
                  rob_pair <- stats::cor(pair_data$x, pair_data$y, method = "spearman")
                  r <- rob_pair
                } else {
                  # Default to zero if not enough data
                  r <- 0
                }
                
                # Ensure r is within [-1, 1]
                r <- max(min(r, 1), -1)
                
                # Assign the correlation value to both positions in the matrix
                correlation_matrix[i, j] <- r
                correlation_matrix[j, i] <- r
              }, error = function(e) {
                # If robust correlation fails, use regular correlation
                warning(paste("Robust correlation failed for pair", 
                              region_columns[i], "-", region_columns[j], ":", e$message))
                
                # Attempt regular correlation
                tryCatch({
                  r <- stats::cor(pair_data$x, pair_data$y, method = correlation_method)
                  correlation_matrix[i, j] <- r
                  correlation_matrix[j, i] <- r
                }, error = function(e2) {
                  # If all fails, set to zero
                  correlation_matrix[i, j] <- 0
                  correlation_matrix[j, i] <- 0
                })
              })
            }
          }
        }
      })
      
      # Ensure diagonal is 1
      diag(correlation_matrix) <- 1
    }
  } else if (correlation_type == "partial") {
    # Partial correlation
    if (!requireNamespace("corpcor", quietly = TRUE)) {
      stop("Package 'corpcor' is required for partial correlation")
    }
    
    # Check if we have enough samples
    if (nrow(region_data) < length(region_columns)) {
      if (!use_regularization) {
        stop("Not enough samples for partial correlation. Enable regularization.")
      }
    }
    
    # Remove rows with any missing values for partial correlation
    complete_data <- region_data[complete.cases(region_data), ]
    
    if (nrow(complete_data) < 3) {
      stop("Not enough complete observations for partial correlation")
    }
    
    if (use_regularization) {
      # Regularized partial correlation
      partial_matrix <- corpcor::pcor.shrink(complete_data)
    } else {
      # Standard partial correlation
      partial_matrix <- corpcor::pcor(complete_data)
    }
    
    # Ensure proper matrix structure and names
    correlation_matrix <- as.matrix(partial_matrix)
    
    # Set row and column names if they're missing
    if (is.null(rownames(correlation_matrix))) {
      rownames(correlation_matrix) <- colnames(complete_data)
    }
    if (is.null(colnames(correlation_matrix))) {
      colnames(correlation_matrix) <- colnames(complete_data)
    }
    
    # Handle any remaining NA values
    correlation_matrix[is.na(correlation_matrix)] <- 0
    
    # Ensure diagonal is 1 (self-correlation)
    diag(correlation_matrix) <- 1
    
  } else if (correlation_type == "covariance") {
    # Compute covariance
    covariance_matrix <- cov(region_data, use = "pairwise.complete.obs")
    
    # Scale by standard deviations to make comparable to correlation
    std_devs <- apply(region_data, 2, sd, na.rm = TRUE)
    correlation_matrix <- matrix(0, ncol(region_data), ncol(region_data))
    rownames(correlation_matrix) <- colnames(region_data)
    colnames(correlation_matrix) <- colnames(region_data)
    
    for (i in 1:ncol(region_data)) {
      for (j in 1:ncol(region_data)) {
        # Check for zero standard deviations to avoid division by zero
        if (std_devs[i] > 0 && std_devs[j] > 0) {
          correlation_matrix[i, j] <- covariance_matrix[i, j] / (std_devs[i] * std_devs[j])
        } else {
          correlation_matrix[i, j] <- 0
        }
      }
    }
  }
  
  # Validate correlation matrix before proceeding
  if (any(is.na(correlation_matrix))) {
    correlation_matrix[is.na(correlation_matrix)] <- 0
    warning("NA values found in correlation matrix and replaced with 0")
  }
  
  if (any(is.infinite(correlation_matrix))) {
    correlation_matrix[is.infinite(correlation_matrix)] <- 0
    warning("Infinite values found in correlation matrix and replaced with 0")
  }
  
  # Ensure correlation values are within valid range [-1, 1]
  correlation_matrix[correlation_matrix > 1] <- 1
  correlation_matrix[correlation_matrix < -1] <- -1
  
  # Validate correlation_threshold
  if (!is.numeric(correlation_threshold) || length(correlation_threshold) != 1) {
    stop("correlation_threshold must be a single numeric value")
  }
  
  # Calculate statistical significance
  p_value_matrix <- calculate_significance(correlation_matrix, nrow(region_data))
  
  # Ensure rownames and colnames are present before creating edge list
  if (is.null(rownames(correlation_matrix)) || is.null(colnames(correlation_matrix))) {
    rownames(correlation_matrix) <- colnames(correlation_matrix) <- region_columns
  }
  
  # Create a proper undirected edge list with consistent ordering
  n_regions <- nrow(correlation_matrix)
  undirected_edges <- data.frame(
    from = character(),
    to = character(),
    weight = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Only process the lower triangle of the matrix to avoid duplicates
  for (i in 1:n_regions) {
    for (j in 1:i) {  # Only process j up to i to get lower triangle
      if (i != j) {  # Skip self-loops
        # Check if correlation is above threshold
        if (abs(correlation_matrix[i, j]) > correlation_threshold) {
          # Always store edges with consistent ordering
          undirected_edges <- rbind(undirected_edges, data.frame(
            from = rownames(correlation_matrix)[j],  # smaller index first
            to = rownames(correlation_matrix)[i],    # larger index second
            weight = correlation_matrix[i, j],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Skip if no edges remain after filtering
  if (nrow(undirected_edges) == 0) {
    return(list(
      correlation_matrix = correlation_matrix,
      p_value_matrix = p_value_matrix,
      graph = NULL,
      edge_list = NULL,
      message = "No connections above threshold"
    ))
  }
  
  # Try to create the graph with better error handling
  tryCatch({
    # Create graph from undirected edges
    graph <- igraph::graph_from_data_frame(undirected_edges, directed = FALSE)
    
    # Assign weights safely
    if (igraph::ecount(graph) > 0) {
      # Make sure weight vectors match exactly with number of edges
      weights <- undirected_edges$weight
      if (length(weights) == igraph::ecount(graph)) {
        igraph::E(graph)$weight <- abs(weights)
        igraph::E(graph)$original_weight <- weights
      } else {
        # Fall back to uniform weights if mismatch
        warning("Edge count mismatch. Using uniform weights.")
        igraph::E(graph)$weight <- rep(1, igraph::ecount(graph))
        igraph::E(graph)$original_weight <- rep(1, igraph::ecount(graph))
      }
    }
    
    return(list(
      correlation_matrix = correlation_matrix,
      p_value_matrix = p_value_matrix,
      graph = graph,
      edge_list = undirected_edges,
      message = paste0("Created network with ", 
                       igraph::vcount(graph), " nodes and ", 
                       igraph::ecount(graph), " edges")
    ))
  }, error = function(e) {
    warning(paste("Error creating graph:", e$message))
    return(list(
      correlation_matrix = correlation_matrix,
      p_value_matrix = p_value_matrix,
      graph = NULL,
      edge_list = undirected_edges,
      message = paste("Error creating graph:", e$message)
    ))
  })
}

#' Calculate statistical significance of correlations
#' 
#' @param correlation_matrix Correlation matrix
#' @param n_subjects Number of subjects
#' @param method Method for calculating p-values ("parametric" or "permutation")
#' @return Matrix of p-values
#' @export
calculate_significance <- function(correlation_matrix, n_subjects, method = "parametric") {
  n_regions <- nrow(correlation_matrix)
  p_value_matrix <- matrix(NA, n_regions, n_regions)
  rownames(p_value_matrix) <- rownames(correlation_matrix)
  colnames(p_value_matrix) <- colnames(correlation_matrix)
  
  if (method == "parametric") {
    # Parametric test
    for (i in 1:n_regions) {
      for (j in 1:n_regions) {
        if (i != j) {
          r <- correlation_matrix[i, j]
          t_stat <- r * sqrt((n_subjects - 2) / (1 - r^2))
          p_value_matrix[i, j] <- 2 * pt(-abs(t_stat), df = n_subjects - 2)
        } else {
          p_value_matrix[i, j] <- 1  # p-value of 1 for self-connections
        }
      }
    }
  } else if (method == "permutation") {
    # Would need the original data for permutation tests
    # Not implemented here
    p_value_matrix[] <- NA
  }
  
  return(p_value_matrix)
}

#' Apply FDR correction for multiple comparisons
#' 
#' @param p_value_matrix Matrix of p-values
#' @return Matrix of adjusted p-values
#' @export
apply_fdr_correction <- function(p_value_matrix) {
  # Flatten matrix (excluding diagonal)
  p_values <- p_value_matrix[lower.tri(p_value_matrix)]
  
  # Apply FDR correction
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package 'stats' is required for FDR correction")
  }
  
  p_adj <- stats::p.adjust(p_values, method = "fdr")
  
  # Rebuild matrix
  p_adj_matrix <- p_value_matrix
  p_adj_matrix[lower.tri(p_adj_matrix)] <- p_adj
  p_adj_matrix[upper.tri(p_adj_matrix)] <- t(p_adj_matrix)[upper.tri(p_adj_matrix)]
  
  return(p_adj_matrix)
}

#' Analyze distance-weight relationship in brain network
#' 
#' @param correlation_matrix Correlation matrix
#' @param distance_matrix Distance matrix between brain regions
#' @return List with analysis results
#' @export
analyze_distance_weight <- function(correlation_matrix, distance_matrix) {
  # Flatten matrices
  flat_corr <- correlation_matrix[lower.tri(correlation_matrix)]
  flat_dist <- distance_matrix[lower.tri(distance_matrix)]
  
  # Calculate correlation
  dist_weight_corr <- cor(flat_dist, abs(flat_corr), method = "spearman")
  
  # Linear model
  lm_model <- lm(abs(flat_corr) ~ flat_dist)
  
  return(list(
    correlation = dist_weight_corr,
    model = lm_model,
    flat_corr = flat_corr,
    flat_dist = flat_dist
  ))
}

#' Calculate global network metrics
#' 
#' @param graph igraph object
#' @return Data frame with global network metrics
#' @export
calculate_global_metrics <- function(graph) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  # Check if graph is valid
  if (is.null(graph) || igraph::ecount(graph) == 0) {
    return(data.frame(
      Metric = c("Density", "Global_Clustering_Coefficient", 
                 "Average_Path_Length", "Modularity"),
      Value = rep(NA, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  # Compute density
  density <- igraph::edge_density(graph)
  
  # Compute global clustering coefficient
  global_clustering <- tryCatch({
    igraph::transitivity(graph, type = "global")
  }, error = function(e) {
    NA
  })
  
  # Compute average path length
  avg_path_length <- tryCatch({
    igraph::mean_distance(graph, directed = FALSE, weights = igraph::E(graph)$weight)
  }, error = function(e) {
    NA
  })
  
  # Compute community structure and modularity
  community <- tryCatch({
    igraph::cluster_louvain(graph, weights = igraph::E(graph)$weight)
  }, error = function(e) {
    NULL
  })
  
  modularity_value <- if (!is.null(community)) {
    igraph::modularity(community)
  } else {
    NA
  }
  
  # Create data frame with metrics
  metrics_df <- data.frame(
    Metric = c("Density", "Global_Clustering_Coefficient", 
               "Average_Path_Length", "Modularity"),
    Value = c(density, global_clustering, avg_path_length, modularity_value),
    stringsAsFactors = FALSE
  )
  
  return(metrics_df)
}

#' Calculate node-level network metrics
#' 
#' @param graph igraph object
#' @return Data frame with node-level network metrics
#' @export
calculate_node_metrics <- function(graph) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required for this function")
  }
  
  # Check if graph is valid
  if (is.null(graph) || igraph::ecount(graph) == 0 || igraph::vcount(graph) == 0) {
    return(data.frame(
      Node = character(0),
      Degree_Centrality = numeric(0),
      Betweenness_Centrality = numeric(0),
      Closeness_Centrality = numeric(0),
      Eigenvector_Centrality = numeric(0),
      Node_Clustering_Coefficient = numeric(0),
      Community = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get node names
  node_names <- igraph::V(graph)$name
  if (is.null(node_names)) {
    node_names <- as.character(1:igraph::vcount(graph))
  }
  
  # Compute degree centrality
  degree_centrality <- igraph::degree(graph)
  names(degree_centrality) <- node_names
  
  # Compute betweenness centrality
  betweenness_centrality <- tryCatch({
    igraph::betweenness(graph, directed = FALSE, weights = igraph::E(graph)$weight)
  }, error = function(e) {
    rep(0, igraph::vcount(graph))
  })
  names(betweenness_centrality) <- node_names
  
  # Compute closeness centrality
  closeness_centrality <- tryCatch({
    igraph::closeness(graph, weights = igraph::E(graph)$weight)
  }, error = function(e) {
    rep(0, igraph::vcount(graph))
  })
  names(closeness_centrality) <- node_names
  
  # Compute eigenvector centrality
  eigenvector_centrality <- tryCatch({
    igraph::eigen_centrality(graph, weights = igraph::E(graph)$weight)$vector
  }, error = function(e) {
    rep(0, igraph::vcount(graph))
  })
  names(eigenvector_centrality) <- node_names
  
  # Compute local clustering coefficient
  node_clustering_coefficient <- tryCatch({
    igraph::transitivity(graph, type = "local", isolates = "zero")
  }, error = function(e) {
    rep(0, igraph::vcount(graph))
  })
  names(node_clustering_coefficient) <- node_names
  
  # Compute community membership
  community <- tryCatch({
    igraph::cluster_louvain(graph, weights = igraph::E(graph)$weight)
  }, error = function(e) {
    NULL
  })
  
  community_membership <- if (!is.null(community)) {
    igraph::membership(community)
  } else {
    rep(1, igraph::vcount(graph))
  }
  names(community_membership) <- node_names
  
  # Create data frame with metrics
  metrics_df <- data.frame(
    Node = node_names,
    Degree_Centrality = degree_centrality,
    Betweenness_Centrality = betweenness_centrality,
    Closeness_Centrality = closeness_centrality,
    Eigenvector_Centrality = eigenvector_centrality,
    Node_Clustering_Coefficient = node_clustering_coefficient,
    Community = community_membership,
    stringsAsFactors = FALSE
  )
  
  return(metrics_df)
}

#' Run network analysis on multiple groups
#' 
#' @param data Data frame containing brain region data
#' @param region_columns Vector of column names containing brain region data
#' @param group_column Name of the column containing group information
#' @param correlation_threshold Threshold for including edges in the network
#' @param correlation_method Method for correlation calculation (default "pearson")
#' @param correlation_type Type of correlation to use ("standard" or "partial")
#' @param use_robust Whether to use robust correlation
#' @param use_regularization Whether to use regularization for partial correlation
#' @param perform_significance Whether to perform significance testing
#' @param apply_fdr Whether to apply FDR correction
#' @return List with network analysis results for each group
#' @export
run_network_analysis_by_group <- function(data, region_columns, group_column,
                                          correlation_threshold = 0.3,
                                          correlation_method = "pearson",
                                          correlation_type = "standard",
                                          use_robust = FALSE,
                                          use_regularization = TRUE,
                                          perform_significance = TRUE,
                                          apply_fdr = TRUE) {
  # Add input validation to handle list parameters
  if (is.list(correlation_method)) {
    warning("correlation_method is a list, using default 'pearson'")
    correlation_method <- "pearson"
  }
  
  if (is.list(correlation_type)) {
    warning("correlation_type is a list, using default 'standard'")
    correlation_type <- "standard"
  }
  
  if (is.list(correlation_threshold)) {
    warning("correlation_threshold is a list, using default 0.3")
    correlation_threshold <- 0.3
  }
  
  if (is.list(use_robust)) {
    warning("use_robust is a list, using default FALSE")
    use_robust <- FALSE
  }
  
  if (is.list(use_regularization)) {
    warning("use_regularization is a list, using default TRUE")
    use_regularization <- TRUE
  }
  
  if (is.list(perform_significance)) {
    warning("perform_significance is a list, using default TRUE")
    perform_significance <- TRUE
  }
  
  if (is.list(apply_fdr)) {
    warning("apply_fdr is a list, using default TRUE")
    apply_fdr <- TRUE
  }
  
  # Get unique groups
  unique_groups <- unique(data[[group_column]])
  
  # Initialize result lists
  networks <- list()
  global_metrics <- data.frame()
  node_metrics <- data.frame()
  significance_matrices <- list()
  
  # Run analysis for each group
  for (group in unique_groups) {
    # Subset data for the current group
    group_data <- data[data[[group_column]] == group, ]
    
    # Check if we have enough data
    if (nrow(group_data) < 3) {
      warning(paste("Not enough data for group:", group))
      next
    }
    
    # Debug information
    cat("Processing group:", group, "\n")
    cat("Group data dimensions:", nrow(group_data), "x", ncol(group_data), "\n")
    cat("Region columns:", paste(region_columns, collapse=", "), "\n")
    
    # Verify all region columns exist in the data
    missing_cols <- setdiff(region_columns, names(group_data))
    if (length(missing_cols) > 0) {
      warning(paste("Missing region columns in group", group, ":", 
                    paste(missing_cols, collapse=", ")))
      next
    }
    
    # Create network with error handling
    tryCatch({
      network_result <- create_correlation_network(
        group_data, 
        region_columns, 
        correlation_threshold, 
        correlation_method,
        correlation_type,
        use_robust,
        use_regularization
      )
      
      # Store network
      networks[[group]] <- network_result
      
      # Store significance matrices
      if (perform_significance) {
        if (apply_fdr) {
          significance_matrices[[group]] <- apply_fdr_correction(network_result$p_value_matrix)
        } else {
          significance_matrices[[group]] <- network_result$p_value_matrix
        }
      }
      
      # If network was created successfully, calculate metrics
      if (!is.null(network_result$graph)) {
        # Calculate global metrics
        group_global_metrics <- calculate_global_metrics(network_result$graph)
        group_global_metrics$Group <- group
        global_metrics <- rbind(global_metrics, group_global_metrics)
        
        # Calculate node metrics
        group_node_metrics <- calculate_node_metrics(network_result$graph)
        group_node_metrics$Group <- group
        node_metrics <- rbind(node_metrics, group_node_metrics)
      }
    }, error = function(e) {
      warning(paste("Error in network analysis for group", group, ":", e$message))
    })
  }
  
  return(list(
    networks = networks,
    global_metrics = global_metrics,
    node_metrics = node_metrics,
    significance_matrices = significance_matrices
  ))
}

#' Compare networks between two groups
#' 
#' @param network1 First network result
#' @param network2 Second network result
#' @param comparison_type Type of comparison ("edge", "node", "global")
#' @param significance_threshold P-value threshold for significant differences
#' @return List with comparison results
#' @export
compare_networks <- function(network1, network2, comparison_type = "edge", 
                             significance_threshold = 0.05) {
  if (comparison_type == "edge") {
    # Compare edge weights
    corr1 <- network1$correlation_matrix
    corr2 <- network2$correlation_matrix
    
    # Calculate difference
    diff_matrix <- corr2 - corr1
    
    # Calculate z-scores for differences (Fisher's z-transformation)
    n1 <- nrow(network1$p_value_matrix)
    n2 <- nrow(network2$p_value_matrix)
    
    z1 <- 0.5 * log((1 + corr1) / (1 - corr1))
    z2 <- 0.5 * log((1 + corr2) / (1 - corr2))
    
    z_diff <- z2 - z1
    se <- sqrt(1/(n1-3) + 1/(n2-3))
    z_score <- z_diff / se
    
    # Calculate p-values
    p_values <- 2 * pnorm(-abs(z_score))
    
    # Apply significance threshold
    sig_diff <- diff_matrix
    sig_diff[p_values > significance_threshold] <- 0
    
    return(list(
      diff_matrix = diff_matrix,
      z_score = z_score,
      p_values = p_values,
      sig_diff = sig_diff
    ))
  } else if (comparison_type == "node") {
    # Compare node metrics
    nodes1 <- network1$node_metrics
    nodes2 <- network2$node_metrics
    
    if (is.null(nodes1) || is.null(nodes2)) {
      return(NULL)
    }
    
    # Merge node metrics
    merged_metrics <- merge(
      nodes1, nodes2, 
      by = "Node", 
      suffixes = c("_1", "_2")
    )
    
    # Calculate differences
    metrics_to_compare <- c("Degree_Centrality", "Betweenness_Centrality", 
                            "Closeness_Centrality", "Eigenvector_Centrality")
    
    for (metric in metrics_to_compare) {
      merged_metrics[[paste0(metric, "_diff")]] <- 
        merged_metrics[[paste0(metric, "_2")]] - merged_metrics[[paste0(metric, "_1")]]
    }
    
    return(merged_metrics)
  } else if (comparison_type == "global") {
    # Compare global metrics
    global1 <- network1$global_metrics
    global2 <- network2$global_metrics
    
    if (is.null(global1) || is.null(global2)) {
      return(NULL)
    }
    
    # Calculate differences
    diff_metrics <- data.frame(
      Metric = global1$Metric,
      Value_1 = global1$Value,
      Value_2 = global2$Value,
      Difference = global2$Value - global1$Value,
      stringsAsFactors = FALSE
    )
    
    return(diff_metrics)
  }
}

#' Generate a standardized color palette for brain areas
#' 
#' @param brain_areas List of brain areas
#' @param seed Random seed for color generation
#' @return Named vector of colors for brain areas
#' @export
generate_area_colors <- function(brain_areas, seed = 123) {
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package 'RColorBrewer' is required for this function")
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Get number of areas
  n_areas <- length(brain_areas)
  
  # Generate colors
  if (n_areas <= 8) {
    colors <- RColorBrewer::brewer.pal(max(n_areas, 3), "Set2")
    if (n_areas < 3) colors <- colors[1:n_areas]
  } else if (n_areas <= 12) {
    colors <- RColorBrewer::brewer.pal(n_areas, "Paired")
  } else {
    # For more than 12 areas, use a combination of palettes
    colors <- c(
      RColorBrewer::brewer.pal(8, "Set2"),
      RColorBrewer::brewer.pal(max(n_areas - 8, 3), "Set1")
    )
    if (n_areas > 16) {
      # If we need even more colors, add from another palette
      additional <- RColorBrewer::brewer.pal(min(n_areas - 16, 8), "Dark2")
      colors <- c(colors, additional)
    }
    colors <- colors[1:n_areas]
  }
  
  # Assign names
  names(colors) <- names(brain_areas)
  
  return(colors)
}

#' Generate colors for groups
#' 
#' @param groups Vector of group names
#' @return Named vector of colors for groups
#' @export
generate_group_colors <- function(groups) {
  n_groups <- length(groups)
  
  if (n_groups <= 8) {
    colors <- RColorBrewer::brewer.pal(max(n_groups, 3), "Set1")
    if (n_groups < 3) colors <- colors[1:n_groups]
  } else {
    colors <- rainbow(n_groups)
  }
  
  # Assign names
  names(colors) <- groups
  
  return(colors)
}

#' Create a network plot using igraph
#' 
#' @param graph igraph object
#' @param node_metrics Data frame with node metrics (optional)
#' @param color_by How to color nodes (options: "brain_area", "community", "metric")
#' @param size_by Which metric to use for node size (default NULL for uniform size)
#' @param area_colors Named vector of colors for brain areas
#' @param layout Layout algorithm to use (default "fr" for Fruchterman-Reingold)
#' @param title Plot title
#' @param uniform_node_size Size value to use when uniform sizing is selected (default 15)
#' @param show_edge_labels Whether to show edge labels
#' @param show_node_labels Whether to show node labels
#' @param significant_edges Matrix of significance values to filter edges
#' @param significance_threshold Threshold for significant edges
#' @return A ggplot2 object with the network visualization
#' @export
plot_network <- function(graph, node_metrics = NULL, 
                         color_by = "brain_area",
                         size_by = NULL, # NULL for uniform sizing, or metric name for variable sizing
                         area_colors = NULL,
                         layout = "fr",
                         title = "Network Plot",
                         uniform_node_size = 15,
                         show_edge_labels = FALSE,
                         show_node_labels = TRUE,
                         significant_edges = NULL,
                         significance_threshold = 0.05) {
  if (!requireNamespace("igraph", quietly = TRUE) ||
      !requireNamespace("ggraph", quietly = TRUE)) {
    stop("Packages 'igraph' and 'ggraph' are required for this function")
  }
  
  # Check if graph is valid
  if (is.null(graph) || igraph::ecount(graph) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = "No network connections above threshold") +
             ggplot2::theme_void() +
             ggplot2::labs(title = title))
  }
  
  # Filter edges by significance if provided
  if (!is.null(significant_edges)) {
    # Create a copy of the graph to filter
    filtered_graph <- graph
    
    # Check if we have no edges, if so just return the empty graph message
    if (igraph::ecount(filtered_graph) == 0) {
      return(ggplot2::ggplot() +
               ggplot2::annotate("text", x = 0, y = 0, 
                                 label = "No network connections above threshold") +
               ggplot2::theme_void() +
               ggplot2::labs(title = title))
    }
    
    # Get edge list with error handling
    tryCatch({
      edge_list <- igraph::as_data_frame(graph, what = "edges")
      
      # Check each edge against significance matrix
      edges_to_remove <- c()
      for (i in 1:nrow(edge_list)) {
        from_node <- edge_list$from[i]
        to_node <- edge_list$to[i]
        
        # Make sure nodes exist in the significance matrix
        if (from_node %in% rownames(significant_edges) && 
            to_node %in% colnames(significant_edges)) {
          # Check p-value against threshold
          if (significant_edges[from_node, to_node] > significance_threshold) {
            edges_to_remove <- c(edges_to_remove, i)
          }
        }
      }
      
      # Remove non-significant edges if any
      if (length(edges_to_remove) > 0) {
        filtered_graph <- igraph::delete_edges(graph, edges_to_remove)
      }
      
      # Use the filtered graph for plotting
      graph <- filtered_graph
      
      # Check if any edges remain
      if (igraph::ecount(graph) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0, y = 0, 
                                   label = "No significant connections above threshold") +
                 ggplot2::theme_void() +
                 ggplot2::labs(title = title))
      }
    }, error = function(e) {
      warning(paste("Error filtering edges by significance:", e$message))
      # Continue with the original graph if there's an error
    })
  }
  
  # Use the provided uniform_node_size parameter for fixed sizing
  fixed_node_size <- uniform_node_size  
  
  # Add node attributes from metrics if provided
  if (!is.null(node_metrics)) {
    # Check if the graph has vertex names
    if (is.null(igraph::V(graph)$name)) {
      # If no names, assign default names
      igraph::V(graph)$name <- paste0("Node", 1:igraph::vcount(graph))
    }
    
    # Match nodes in graph with metrics
    for (node in igraph::V(graph)$name) {
      # Find the node in metrics data
      metric_row <- node_metrics[node_metrics$Node == node, , drop = FALSE]
      if (nrow(metric_row) > 0) {
        # Add attributes to the graph
        if ("Brain_Area" %in% names(metric_row) && color_by == "brain_area") {
          igraph::V(graph)[node]$area <- as.character(metric_row$Brain_Area)
        }
        if ("Community" %in% names(metric_row) && color_by == "community") {
          igraph::V(graph)[node]$community <- metric_row$Community
        }
        # Add size metric if specified and exists in metrics
        if (!is.null(size_by) && size_by %in% names(metric_row) && size_by != "uniform") {
          igraph::V(graph)[node]$size <- metric_row[[size_by]]
        }
      }
    }
  }
  
  # Set color attribute based on selection
  if (color_by == "brain_area" && "area" %in% igraph::vertex_attr_names(graph)) {
    if (is.null(area_colors)) {
      # Create default colors if not provided
      unique_areas <- unique(igraph::vertex_attr(graph, "area"))
      if (length(unique_areas) > 0) {
        area_colors <- setNames(
          scales::hue_pal()(length(unique_areas)),
          unique_areas
        )
      } else {
        # Default single color if no areas found
        area_colors <- c("Unknown" = "#1F78B4")
      }
    }
    
    # Create a vector for vertex colors, with error handling
    vertex_colors <- rep("#1F78B4", igraph::vcount(graph)) # Default color
    for (i in 1:igraph::vcount(graph)) {
      v_name <- igraph::V(graph)[i]$name
      v_area <- igraph::vertex_attr(graph, "area", index = i)
      if (!is.null(v_area) && v_area %in% names(area_colors)) {
        vertex_colors[i] <- area_colors[v_area]
      }
    }
    igraph::V(graph)$color <- vertex_colors
    
  } else if (color_by == "community" && "community" %in% igraph::vertex_attr_names(graph)) {
    # Color by community membership
    communities <- unique(igraph::vertex_attr(graph, "community"))
    if (length(communities) > 0) {
      comm_colors <- setNames(
        scales::hue_pal()(length(communities)),
        communities
      )
      
      # Assign colors with error handling
      vertex_colors <- rep("#1F78B4", igraph::vcount(graph)) # Default color
      for (i in 1:igraph::vcount(graph)) {
        v_comm <- igraph::vertex_attr(graph, "community", index = i)
        if (!is.null(v_comm) && !is.na(v_comm) && v_comm %in% names(comm_colors)) {
          vertex_colors[i] <- comm_colors[v_comm]
        }
      }
      igraph::V(graph)$color <- vertex_colors
    } else {
      igraph::V(graph)$color <- "#1F78B4" # Default color
    }
  } else if (color_by == "metric" && !is.null(size_by) && "size" %in% igraph::vertex_attr_names(graph)) {
    # Color by the same metric used for sizing
    sizes <- igraph::vertex_attr(graph, "size")
    if (length(sizes) > 0 && !all(is.na(sizes))) {
      igraph::V(graph)$color <- scales::gradient_n_pal(
        c("#F8766D", "#00BA38", "#619CFF")
      )(scales::rescale(sizes))
    } else {
      igraph::V(graph)$color <- "#1F78B4" # Default color
    }
  } else {
    # Default coloring
    igraph::V(graph)$color <- "#1F78B4"
  }
  
  # Process node sizes for variable sizing
  if (!is.null(size_by) && size_by != "uniform" && "size" %in% igraph::vertex_attr_names(graph)) {
    # Scale sizes between min_size and max_size
    sizes <- igraph::vertex_attr(graph, "size")
    if (length(sizes) > 0 && !all(is.na(sizes))) {
      min_size <- 5
      max_size <- 20
      
      # Check if we have different values before scaling
      if (max(sizes, na.rm = TRUE) > min(sizes, na.rm = TRUE)) {
        scaled_sizes <- scales::rescale(sizes, to = c(min_size, max_size))
        igraph::V(graph)$scaled_size <- scaled_sizes
      } else {
        # If all sizes are the same, use the average of min and max size
        igraph::V(graph)$scaled_size <- rep((min_size + max_size) / 2, length(sizes))
      }
    } else {
      # Fallback to uniform size
      igraph::V(graph)$scaled_size <- rep(fixed_node_size, igraph::vcount(graph))
    }
  } else {
    # Set uniform size for all nodes using the provided parameter
    igraph::V(graph)$scaled_size <- rep(fixed_node_size, igraph::vcount(graph))
  }
  
  # Create layout based on selected algorithm
  if (layout == "circle" && "area" %in% igraph::vertex_attr_names(graph)) {
    # Create custom layout for circle that groups by brain area
    # Get all node names and their brain areas
    node_names <- igraph::V(graph)$name
    node_areas <- igraph::vertex_attr(graph, "area")
    
    # Define brain area order (you can customize this order)
    # Default brain areas in your application structure
    area_order <- c(
      "Dorsal HPC", 
      "Ventral HPC", 
      "Subiculum", 
      "Nucleus Accumbens", 
      "Frontal", 
      "Amygdala", 
      "Retrosplenial",
      "Other"  # Keep "Other" at the end
    )
    
    # Filter to only include areas that exist in the graph
    area_order <- area_order[area_order %in% unique(node_areas)]
    
    # Add any areas not in the predefined list at the end
    extra_areas <- setdiff(unique(node_areas), area_order)
    if (length(extra_areas) > 0) {
      area_order <- c(area_order, extra_areas)
    }
    
    # Create a factor with specified order for sorting
    area_factor <- factor(node_areas, levels = area_order)
    
    # Create a data frame for sorting
    node_df <- data.frame(
      name = node_names,
      area = node_areas,
      area_factor = area_factor,
      stringsAsFactors = FALSE
    )
    
    # Sort by brain area (using the ordered factor)
    node_df <- node_df[order(node_df$area_factor), ]
    
    # Create a vector of ordered nodes
    ordered_nodes <- node_df$name
    
    # Calculate positions on a circle
    n <- length(node_names)
    theta <- seq(0, 2 * pi - (2 * pi / n), length.out = n)
    x_coords <- cos(theta)
    y_coords <- sin(theta)
    circle_coords <- cbind(x_coords, y_coords)
    
    # Create a layout matrix in the original node order
    graph_layout <- matrix(0, nrow = length(node_names), ncol = 2)
    rownames(graph_layout) <- node_names
    
    # Fill in coordinates according to the ordered nodes
    for (i in 1:length(ordered_nodes)) {
      node <- ordered_nodes[i]
      graph_layout[node, ] <- circle_coords[i, ]
    }
  } else if (layout == "fr") {
    graph_layout <- igraph::layout_with_fr(graph)
  } else if (layout == "kk") {
    graph_layout <- igraph::layout_with_kk(graph)
  } else if (layout == "lgl") {
    # Large Graph Layout - good for large networks
    graph_layout <- tryCatch({
      igraph::layout_with_lgl(graph)
    }, error = function(e) {
      # Fall back to FR if LGL fails
      warning("LGL layout failed, falling back to FR layout")
      igraph::layout_with_fr(graph)
    })
  } else if (layout == "circle") {
    graph_layout <- igraph::layout_in_circle(graph)
  } else if (layout == "grid") {
    graph_layout <- tryCatch({
      igraph::layout_on_grid(graph)
    }, error = function(e) {
      # Fall back to FR if grid fails
      warning("Grid layout failed, falling back to FR layout")
      igraph::layout_with_fr(graph)
    })
  } else if (layout == "star") {
    graph_layout <- tryCatch({
      igraph::layout_as_star(graph)
    }, error = function(e) {
      # Fall back to FR if star fails
      warning("Star layout failed, falling back to FR layout")
      igraph::layout_with_fr(graph)
    })
  } else {
    # Default to Fruchterman-Reingold
    graph_layout <- igraph::layout_with_fr(graph)
  }
  
  # Check for edge attributes and add if missing
  if (!("original_weight" %in% igraph::edge_attr_names(graph))) {
    igraph::E(graph)$original_weight <- 1  # Default positive weight
  }
  
  # Add explicit edge color attribute based on sign
  igraph::E(graph)$edge_color <- ifelse(igraph::E(graph)$original_weight > 0, "red", "blue")
  
  # Create plot using ggraph
  tryCatch({
    p <- ggraph::ggraph(graph, layout = graph_layout) + 
      # Add edges first (lowest layer)
      ggraph::geom_edge_link(
        ggplot2::aes(
          width = abs(igraph::E(graph)$original_weight),
          colour = igraph::E(graph)$edge_color
        ),
        alpha = 0.7
      ) 
    
    # Add edge labels if requested
    if (show_edge_labels) {
      p <- p + ggraph::geom_edge_text(
        ggplot2::aes(
          label = round(igraph::E(graph)$original_weight, 2)
        ),
        size = 3,
        angle = 0
      )
    }
    
    # Add nodes (middle layer)
    p <- p + ggraph::geom_node_point(
      ggplot2::aes(color = igraph::V(graph)$color, size = igraph::V(graph)$scaled_size)
    )
    
    # Add node labels if requested
    if (show_node_labels) {
      p <- p + ggraph::geom_node_text(
        ggplot2::aes(label = name),
        size = 3.5,
        color = "black",
        fontface = "bold",
        repel = FALSE,  # Don't repel to keep centered
        show.legend = FALSE
      )
    }
    
    # Use ggraph functions for edge aesthetics
    p <- p + ggraph::scale_edge_colour_identity(
      name = "Correlation",
      guide = "legend",
      labels = c("Positive", "Negative")
    ) +
      ggraph::scale_edge_width_continuous(
        name = "Strength",
        range = c(0.3, 2)
      ) +
      ggplot2::scale_color_identity() +
      ggplot2::scale_size_identity() +
      ggplot2::labs(title = title) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    # Create legend for brain areas if coloring by area
    if (color_by == "brain_area" && "area" %in% igraph::vertex_attr_names(graph)) {
      # Get unique areas and their colors
      unique_areas <- unique(igraph::vertex_attr(graph, "area"))
      if (length(unique_areas) > 0 && !is.null(area_colors)) {
        area_colors_used <- area_colors[unique_areas]
        
        # Add manual color scale for the legend
        p <- p + 
          ggplot2::guides(
            edge_colour = ggplot2::guide_legend(title = "Correlation"),
            edge_width = ggplot2::guide_legend(title = "Strength")
          ) +
          ggplot2::scale_color_identity(
            name = "Brain Area",
            breaks = area_colors_used,
            labels = unique_areas,
            guide = "legend"
          )
      }
    }
    
    return(p)
  }, error = function(e) {
    warning(paste("Error in plot_network:", e$message))
    # Return a basic error plot
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0, y = 0, 
                               label = paste("Error generating plot:", e$message)) +
             ggplot2::theme_void() +
             ggplot2::labs(title = title))
  })
}

#' Safely save plots to a file
#' 
#' @param plot ggplot2 object to save
#' @param filename Output filename
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution in DPI
#' @return Invisibly returns TRUE if successful, FALSE if not
#' @export
save_plot_safely <- function(plot, filename, width = 10, height = 8, dpi = 300) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("Package 'ggplot2' is required for this function")
    return(invisible(FALSE))
  }
  
  tryCatch({
    ggplot2::ggsave(filename, plot, width = width, height = height, dpi = dpi)
    return(invisible(TRUE))
  }, error = function(e) {
    warning(paste("Error saving plot:", e$message))
    return(invisible(FALSE))
  })
}

#' Format network metrics for export
#' 
#' @param node_metrics Data frame with node metrics
#' @param global_metrics Data frame with global metrics
#' @param output_file Path to save the Excel file
#' @return Invisibly returns TRUE if successful
#' @export
format_metrics_for_export <- function(node_metrics, global_metrics, output_file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for this function")
  }
  
  # Create workbook
  wb <- openxlsx::createWorkbook()
  
  # Add sheet with all node metrics
  openxlsx::addWorksheet(wb, "All_Node_Metrics")
  openxlsx::writeData(wb, "All_Node_Metrics", node_metrics)
  
  # Add sheet with all global metrics
  openxlsx::addWorksheet(wb, "All_Global_Metrics")
  openxlsx::writeData(wb, "All_Global_Metrics", global_metrics)
  
  # Add sheets for each group's node metrics
  unique_groups <- unique(node_metrics$Group)
  for (group in unique_groups) {
    group_data <- node_metrics[node_metrics$Group == group, ]
    sheet_name <- paste0(gsub("[^a-zA-Z0-9]", "_", group), "_Node_Metrics")
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet_name, group_data)
  }
  
  # Add sheets for each group's global metrics
  for (group in unique_groups) {
    group_data <- global_metrics[global_metrics$Group == group, ]
    if (nrow(group_data) > 0) {
      sheet_name <- paste0(gsub("[^a-zA-Z0-9]", "_", group), "_Global_Metrics")
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, group_data)
    }
  }
  
  # Save workbook
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  
  return(invisible(TRUE))
}
