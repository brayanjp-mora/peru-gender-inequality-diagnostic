
packages = c("tidyverse", "survey", "here")

check_packages <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
setwd(here())
rm(check_packages)

# Load Local Configuration (if exists)
if (file.exists(here::here("config.R"))) {
  source(here::here("config.R"))
}

#=====================================================================
#--------------------- DATA LOADING FUNCTIONS ------------------------
#=====================================================================
#----------------------- WHY AND HOW TO USE --------------------------
# 
# 
# 
# 
# 
#=====================================================================
load_income_data <- function(mode = "auto", sector_filter = "Private") {
  
  # 1. Define Paths
  path_private <- here::here("Data/processed_data/processed_income_step1.rds")
  path_public  <- here::here("Data/processed_data/processed_income_public.rds")
  
  # 2. Smart Loading Logic
  # If the private file exists, we use it (Owner Mode).
  # If not, we fall back to the public file (Public Mode).
  if (file.exists(path_private) && mode != "public") {
    target_path <- path_private
    message(" [INFO] Loading PRIVATE (Full) Dataset.")
  } else {
    target_path <- path_public
    message(" [INFO] Loading PUBLIC (Anonymized) Dataset.")
  }

  data <- readRDS(file = target_path) %>%
    drop_na(dwincome_monthly) %>% 
    filter(ethnicity != "no sabe")
    
  if (!is.null(sector_filter)) {
    data <- data %>% filter(sector_var == sector_filter)
  }
  return(data)
}

load_geomdep_data <- function(
	path = "data/cleaned_data/geometry_dep.rds") {
		data <- readRDS(file = path)
		return(data)
}

load_geomprov_data <- function(
	path = "data/cleaned_data/geometry_prov.rds") {
		data <- readRDS(file = path)
		return(data)
}

#=====================================================================
profile <- c(
	"commute_distance_cat", "age_categories", "relation_to_head", 
	"marital_status", "ethnicity", "class_of_worker", "sector_var",
	"contract_var", "education_group", "workplace_location", 
	"pay_frequency", "cno", "rev4")

#*********************************************************************
#********************************************************************* 

#=====================================================================
#-------------------- GAP ANALYSIS FUNCTION  -------------------------
#=====================================================================
#----------------------- WHY AND HOW TO USE --------------------------
# 
# 
# 
# 
# 
#=====================================================================
gap_analysis <- function(
	data_survey, variable, sample_check = TRUE, 
	threshold = NULL, filter_by = NULL, hide_details = TRUE) {	

	# Create the survey design object
	income_design <- svydesign(
		ids = ~ conglome,
		weights = ~ fac500a,
		strata = ~ estrato,
		data = data_survey,
		nest = TRUE
	)

	# Calculate median income by the specified variable and sex
	median_results <- svyby(
		formula = as.formula(paste("~", "dwincome_monthly")),
		by = as.formula(paste0("~", variable, " + sex")),
		design = income_design,
		FUN = svyquantile,
		quantiles = 0.5,
		ci = TRUE,
		vartype = "se"
	)

	results <- median_results %>%
		as_tibble() %>%
		# Pivot the data from long to wide format
		pivot_wider(
			id_cols = all_of(variable),
			names_from = sex,
			values_from = c(dwincome_monthly, se.dwincome_monthly)
		) %>%
		# Rename the newly created columns
		rename(
			med_incomew = dwincome_monthly_mujer,
			med_incomem = dwincome_monthly_hombre,
			se_w = `se.dwincome_monthly_mujer`,
			se_m = `se.dwincome_monthly_hombre`
		) %>%
		mutate( #Calculating Gap significance
			gap = med_incomem - med_incomew,
			se_gap = sqrt(se_m^2 + se_w^2),
			ci_lower_gap = gap - 1.96 * se_gap,
			ci_upper_gap = gap + 1.96 * se_gap,
			significant_gap = sign(ci_lower_gap) == sign(ci_upper_gap)
    ) %>% 
		mutate( #Calculating Women's Median Income significance
			ci_lower_w = med_incomew - 1.96 * se_w,
			ci_upper_w = med_incomew + 1.96 * se_w,
      significant_w = sign(ci_lower_w) == sign(ci_upper_w)
    ) %>% 
    mutate( #Calculating Men's Median Income significance
			ci_lower_m = med_incomem - 1.96 * se_m,
			ci_upper_m = med_incomem + 1.96 * se_m,
      significant_m = sign(ci_lower_m) == sign(ci_upper_m)
		) %>%
		mutate(
			cv_w = se_w/med_incomew*100,
			cv_m = se_m/med_incomem*100,
			cv_gap = abs(se_gap/gap*100),
			max_cv = pmax(cv_w, cv_m, cv_gap, na.rm = TRUE)
		) %>%
		mutate(
			gap_ratio =
				((med_incomem - med_incomew)/med_incomem)*100
		) %>%
		mutate(
			deprived = {
				income_check <- if (!is.null(threshold)) {
					med_incomew <= threshold
				} else {
					med_incomew <= mean(med_incomew, na.rm = TRUE)
				}
				ifelse(income_check & gap_ratio >= mean(gap_ratio),
					"yes", "no")
			}
		)

	if (sample_check) {
		sample_counts <- data_survey %>%
			# Use .data[[variable]] to handle the string variable name
			count(.data[[variable]], sex, name = "n") %>%
			pivot_wider(
				names_from = sex,
				values_from = n,
				values_fill = 0
			) %>%
			mutate(
				sample_check = if_else(
					.data$hombre >= 100 & .data$mujer >= 100, "Pass", "Fail"
				)
			) %>%
			select(all_of(variable), sample_check)

		results <- results %>% left_join(sample_counts, by = variable)
	}

	results <- results %>%
		mutate(data_reliability = case_when(
      # Reliable Check
				sample_check == "Pass" & significant_gap == TRUE &
        significant_w == TRUE & significant_m == TRUE &
				max_cv <= 15 ~ "a: reliable",
      # Acceptable Check
				sample_check == "Pass" & significant_gap == TRUE &
        significant_w == TRUE & significant_m == TRUE &
				max_cv <= 30 ~ "b: acceptable (use with caution)",
      # Everything else is Unreliable
				TRUE ~ "unreliable"
		)) %>% 
		{
			if (!is.null(filter_by)) {
				if ("data_reliability_a" %in% filter_by) {
					. <- filter(., data_reliability == "a: reliable")
				} else if ("data_reliability" %in% filter_by) {
					. <- filter(., data_reliability != "unreliable")
				}
				if ("deprived" %in% filter_by) {
					. <- filter(., deprived == "yes")
				}
			}
			.
		} %>%
		relocate(c("gap_ratio", "gap", "data_reliability", "deprived"), .after = med_incomew) %>%
		relocate(
			gap_ratio, .before = gap,
		) %>%
			arrange(med_incomew)

	if (hide_details) {
		vars_to_hide <- c(
			"se_m", "se_w", "se_gap", "ci_lower", "ci_upper", "significant_gap",
      "significant_w", "significant_m", 
			"cv_w", "cv_m", "cv_gap", "max_cv", "sample_check",
			"ci_lower_w", "ci_upper_w", "ci_lower_m", "ci_upper_m"
		)
		
		results <- results %>% select(-any_of(vars_to_hide))
	}

	return(results)
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#----------------- BOOTSTRAP DEPRIVED ANALYSIS ---------------------
#=====================================================================
bootstrap_deprived <- function(data, variables, n_replicates = 100, ...) {
  
  # This will store the string representation of each path found
  path_results <- character(n_replicates)
  
  # Set up a progress bar
  pb <- txtProgressBar(min = 0, max = n_replicates, style = 3)
  
  # Loop for the number of bootstrap replicates
  for (i in 1:n_replicates) {
    # Create a bootstrap sample of the data (sampling with replacement)
    bootstrap_sample <- data %>% sample_n(size = nrow(data), replace = TRUE)
    
    # Run the find_deprived_group function on the bootstrap sample
    # We use 'try' to catch any errors if a sample doesn't yield a path
    path <- try(
      find_deprived_group(data = bootstrap_sample, variables = variables, ...),
      silent = TRUE
    )
    
    # If a path was found, format it and store it
    if (!inherits(path, "try-error") && length(path) > 0) {
      path_string <- format_deprived_path(path) %>%
        mutate(path_step = paste(`Filter Variable`, `Filter Value`, sep = " == ")) %>%
        pull(path_step) %>%
        paste(collapse = " -> ")
      path_results[i] <- path_string
    } else {
      path_results[i] <- "No Path Found"
    }
    
    # Update the progress bar
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # Count the occurrences of each path and return as a tibble
  return(as_tibble(table(path_results)) %>% arrange(desc(n)))
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#--------------------- PROFILE ANALYSIS FUNCTION ---------------------
#=====================================================================
profile_analysis <- function(data, variable) {
	# Filter the data based on the provided conditions
	filtered_data <- data

	# Create the survey design object
	profile_design <- svydesign(
		ids = ~ conglome,
		weights = ~ fac500a,
		strata = ~ estrato,
		data = filtered_data,
		nest = TRUE
	)

	# Create a formula for the svytable
	formula <- as.formula(paste("~", variable))

	# Calculate weighted frequencies
	freq_table <- svytable(formula, design = profile_design)

	# Convert to a tibble and calculate proportions
	results_tbl <- freq_table %>%
		as_tibble() %>%
		rename(weighted_freq = n) %>%
		mutate(proportion_pct = (weighted_freq / sum(weighted_freq)) * 100) %>%
		arrange(desc(proportion_pct))

	return(results_tbl)
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#----------------- MULTIPLE PROFILE ANALYSIS FUNCTION ----------------
#=====================================================================

profile_analysis_multiple <- function(data, variables, ...) {
  # Use map to apply the profile_analysis function to each variable
  results_list <- purrr::map(variables, ~profile_analysis(data, .x))
  
  # Set the names of the list elements to the variable names
  names(results_list) <- variables
  
  return(results_list)
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#----------------- PROFILE COMPARISON ANALYSIS FUNCTION ----------------
#=====================================================================
profile_comparison <- function(data, variable, ...) {
  
  # Create the survey design object
  profile_design <- svydesign(
    ids = ~ conglome,
    weights = ~ fac500a,
    strata = ~ estrato,
    data = data,
    nest = TRUE
  )
  
  # Create a formula for svytable, including sex
  formula <- as.formula(paste("~", variable, "+ sex"))
  
  # Calculate weighted frequencies grouped by variable and sex
  freq_table <- svytable(formula, design = profile_design)
  
  # Process the results
  results_tbl <- freq_table %>%
    as_tibble() %>%
    group_by(sex) %>%
    mutate(proportion_pct = (n / sum(n)) * 100) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = all_of(variable),
      names_from = sex,
      values_from = c(n, proportion_pct),
      names_sep = "_"
    ) %>%
		rename(
			weighted_freq_men = n_hombre,
			weighted_freq_women = n_mujer,
			proportion_pct_men = proportion_pct_hombre,
			proportion_pct_women = proportion_pct_mujer
		) %>% 
		mutate(
			proportion_diff = proportion_pct_men - proportion_pct_women,
			status = case_when(
				proportion_diff > 0 ~ "More Men",			
				proportion_diff < 0 ~ "More Women",
 				TRUE ~ "Equal"
			)
		) %>% 
    arrange(desc(abs(proportion_diff)))
  
  return(results_tbl)
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#------------- MULTIPLE PROFILE COMPARISON ANALYSIS FUNCTION ---------
#=====================================================================
profile_comparison_multiple <- function(data, variables, ...) {
  # Use map to apply the profile_comparison function to each variable
  results_list <- purrr::map(variables, ~profile_comparison(data, .x, ...))
  
  # Set the names of the list elements to the variable names
  names(results_list) <- variables
  
  return(results_list)
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#----------------- AUTOMATED DEPRIVED ANALYSIS FUNCTION --------------
#=====================================================================
find_deprived_group <- function(data, variables, stochastic_n = 1, ...) {
  
  # Initialize with the full dataset and all variables
  current_data <- data
  remaining_vars <- variables
  filter_path <- list()
  
  # Loop until we can't drill down further
  while (length(remaining_vars) > 0) {
    
    message(paste("\nAnalyzing", nrow(current_data), "observations..."))
    
    # Store results for each variable at this level
    level_results <- list()
    
    # Analyze each remaining variable on the current dataset
    for (var in remaining_vars) {
      analysis <- gap_analysis(current_data, var, filter_by = c("data_reliability", "deprived"), ...)
      if (nrow(analysis) > 0) {
        level_results[[var]] <- analysis
      }
    }
    
    # If no variable at this level yields a reliable "Deprived" group, stop.
    if (length(level_results) == 0) {
      message("No further reliable 'Deprived' groups found. Stopping.")
      break
    }
    
    # Combine all first-row results into a single tibble to find the best next step
    top_candidates <- purrr::map_dfr(names(level_results), ~{
      level_results[[.x]][1, ] %>% 
        mutate(source_var = .x)
    }) %>% 
      arrange(med_incomew)
    
    # If there are no candidates, stop
    if(nrow(top_candidates) == 0) {
      message("No candidates found for the next step. Stopping.")
      break
    }
    
    # --- Stochastic Path Selection ---
    # If stochastic_n > 1, randomly sample from the top candidates.
    # Otherwise, pick the best one (the default "greedy" behavior).
    if (nrow(top_candidates) > 1 && stochastic_n > 1) {
      sample_size <- min(nrow(top_candidates), stochastic_n)
      best_candidate <- top_candidates %>% slice_head(n = sample_size) %>% slice_sample(n = 1)
    } else {
      best_candidate <- top_candidates[1, ]
    }
    best_var <- best_candidate$source_var
    filter_value <- best_candidate[[best_var]]
    
    # Apply the filter for the next iteration
    message(paste("--> Filtering by:", best_var, "==", filter_value, 
                  "(Women's Median Income:", round(best_candidate$med_incomew, 2), 
                  ", Gap Ratio:", round(best_candidate$gap_ratio, 1), "%)"))
    
    current_data <- current_data %>% filter(.data[[best_var]] == filter_value)
    
    # Store the full context of the chosen step
    filter_path <- append(filter_path, setNames(list(best_candidate), best_var))
    remaining_vars <- setdiff(remaining_vars, best_var)
  }
  
  return(filter_path)
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#----------------- FORMAT Deprived PATH FUNCTION ------------------
#=====================================================================
format_deprived_path <- function(path_list) {
  # Use map_dfr to iterate through the list and create a dataframe
  summary_df <- purrr::map_dfr(seq_along(path_list), ~{
    step_name <- names(path_list)[.x]
    step_data <- path_list[[.x]]
    
    tibble(
      Step = .x,
      `Filter Variable` = step_name,
      `Filter Value` = as.character(step_data[[step_name]]),
      `Women's Median Income` = step_data$med_incomew,
      `Gap Ratio (%)` = step_data$gap_ratio,
      `Data Reliability` = step_data$data_reliability
    )
  })
  
  return(summary_df)
}
#-----------------------------  END ----------------------------------
#=====================================================================
