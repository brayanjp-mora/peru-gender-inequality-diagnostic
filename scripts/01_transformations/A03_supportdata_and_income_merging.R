#Setting up the Environment
packages = c(
	"tidyverse", "tidylog", "janitor", "readxl")

check_pacakges <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
setwd(here())

#=====================================================================
#---------------------  CONSUMER PRICE INDEX -------------------------
#=====================================================================
#explanation:






#=====================================================================
cpi <- list.files("Data/raw_data/cpi", full.names = TRUE) #data file
cpi <- read_xlsx(cpi, sheet = 2, col_names = FALSE) %>% 
       rename(month_year = ...1,
              cpi = ...2) %>% 
       slice(-c(1,2))

#---------------------------------------------------------------------
months <- c( #creating a dictionary for the months of the year
       "Ene" = "01", "Feb" = "02", "Mar" = "03", "Abr" = "04",
       "May" = "05", "Jun" = "06", "Jul" = "07", "Ago" = "08",
       "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dic" = "12"
     )       
     
cpi <- cpi %>% # Creating date variables:
       mutate(month = str_sub(month_year, 1, 3),
              year = as.numeric(str_sub(month_year, 4, 5)),
              n_month = months[month],
              n_year = ifelse(year < 90, 2000 + year, 1900 + year),
              date = ymd(paste(n_year, n_month, "01", sep = "-"))) %>% 
       arrange(desc(date))

cpi_annual_average <- cpi %>% #Calculating the average cpi:
       select(n_year, cpi) %>% 
       rename(year = n_year) %>% 
       group_by(year) %>% 
       summarise(mean_cpi = sum(as.double(cpi))/12) %>% 
       arrange(desc(year))

harmonization_factor <- cpi_annual_average %>% 
       mutate(factor = 114/mean_cpi) %>% # To express every value from 20xx to 2024 soles
       filter(between(year, 2017, 2024))  

#---------------------------------------------------------------------
#Results:
# harmon_factor 
rm(cpi, cpi_annual_average, months)
#-----------------------------  END ----------------------------------
#=====================================================================




#=====================================================================
#-------------------------  EXCHANGE RATE ----------------------------
#=====================================================================
pooled_exchange_rate <- list.files(
       path = "Data/raw_data/exchange_rate", 
       full.names = TRUE)
pooled_exchange_rate <- read_xls(pooled_exchange_rate, sheet = 1) %>% 
			slice(-c(1,2)) %>% 
			select(-c(...3, ...4)) %>% 
			row_to_names(row_number = 1) %>% 
			clean_names() %>% 
			filter(country_code == "PER") %>% 
			pivot_longer(cols = x1960:x2024,
						names_to = "year") %>% 
			mutate(year = str_remove(year, "x"),
				year = as.double(year), 
				value = as.double(value)) %>% 
			rename(exchange = value) 

#---------------------------------------------------------------------
pooled_exchange_rate <- pooled_exchange_rate %>% 
	filter(year >= 2017) %>% 
	summarise(exchange_rate = sum(exchange)/8)
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#-------------------------  EXCHANGE RATE ----------------------------
#=====================================================================
income <- list.files(
       path = "Data/processed_data", 
       pattern = "step2", 
       full.names = TRUE)
income <- read_rds(income) 

#---------------------------------------------------------------------
cleaned_income <- income %>% 
       left_join(x = ., y = harmonization_factor, by = "year") %>% 
       mutate(harmonized_dwincome = i524e1 * factor) %>%
       mutate(harmonized_iwincome = i530a * factor) %>%
       mutate(dwincome_monthly = harmonized_dwincome/12) %>%
       mutate(iwincome_monthly = harmonized_iwincome/12) %>% 
       mutate(exchange_rate = pooled_exchange_rate$exchange_rate) %>% 
       mutate(dwincome_monthly_usd = dwincome_monthly / exchange_rate) %>% 
       mutate(iwincome_monthly_usd = iwincome_monthly / exchange_rate) %>% 
       relocate(exchange_rate, .after = iwincome_monthly_usd) %>% 
       relocate(c(harmonized_dwincome, harmonized_iwincome), .after = exchange_rate) %>% 
       select(- c(mean_cpi, factor))

#---------------------------------------------------------------------
saveRDS(cleaned_income, file = "Data/cleaned_data/cleaned_income.rds")
#-----------------------------  END ----------------------------------
#=====================================================================

# rm(list = ls()) # Use to remove current variables
# gc()
