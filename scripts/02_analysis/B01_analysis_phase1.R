# Sourcing the setup file with functions and data
source(here::here("code", "00_analysis_setup.r"))

#=====================================================================
#----------------- BOOTSTRAPPED WORST-OFF ANALYSIS -------------------
#=====================================================================
## NOTES: 
## - This takes 29 hours to run. The results are pre-saved in the results folder for convenience.
## - To run the code, you can select everything inside the BOOTSRAPPED WORST-OFF ANALYSIS section
##   and toggle the line command
#---------------------------------------------------------------------
## Define a master list of variables to sample from.
# master_vars <- c(
#  "contract_var", "age_categories", "education_group",
#  "ethnicity", "dominio_summary", "pay_frequency", "marital_status",
#	"commute_distance_cat"
#)

## --- Randomized Bootstrap Exploration ---
## This loop will run the bootstrap analysis multiple times,
## each time with a different random subset of variables.
## ---# ---# ---# ---# ---# ---# ---# ---# ---# ---# ---# ---# ---# ---
# num_explorations <- 50
# all_bootstrap_results <- list()

# for (i in 1:num_explorations) {
  
#   message(paste("\n--- Starting Exploration Run", i, "of", num_explorations, "---\n"))
  
#   ## Create a random subset of variables for this run
#   ## It will sample exactly 5 variables from the master_vars list.
#   analysis_vars <- sample(master_vars, 5)
  
#   message(paste("Using variables:", paste(analysis_vars, collapse = ", ")))
  
#   ## Run the bootstrap analysis with the random set of variables
#   bootstrap_results <- bootstrap_worst_off(
#     data = dwincome, 
#     variables = analysis_vars,
#     n_replicates = 500, ## Using 500 for speed, can be increased
#     stochastic_n = 3
#   )
  
#   all_bootstrap_results[[i]] <- bootstrap_results
# }

# ## --- Summarize All Exploration Runs ---
# ## Combine all the results from the list into a single dataframe
# combined_results <- bind_rows(all_bootstrap_results)

# ## Aggregate the results to get total counts for each unique path
# final_summary <- combined_results %>%
#   group_by(path_results) %>%
#   summarise(total_n = sum(n), .groups = 'drop') %>%
#   arrange(desc(total_n))

# ## Print the final summary table, showing the most frequent paths at the top
# print("--- Aggregated Results Across All Explorations ---")
# print(final_summary, n = 100)
# saveRDS(final_summary, "data/analysis_results/worst_off_summary.rds")
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#--------------- WORST OFF GROUP, ANALYSIS AND PROFILE ---------------
#=====================================================================

#-----------------------------  END ----------------------------------
#=====================================================================

