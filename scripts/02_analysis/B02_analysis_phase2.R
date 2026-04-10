# Sourcing the setup file with functions and data
source(here::here("code", "00_analysis_setup.r"))

#=====================================================================
#------------ DATA ANALYSIS - OVERALL INCOME GAP BY SEX --------------
#=====================================================================
income_design <- svydesign(
	ids = ~ conglome, 
	weights = ~ fac500a,
	strata = ~ estrato,
	data = dwincome,
	nest = TRUE
)

#---------------------------------------------------------------------
median_overall <- svyby(
	formula = ~ dwincome_monthly,
	by = ~ sex,
	design = income_design,
	FUN = svyquantile,
	quantiles = 0.5,
	ci = TRUE,
	vartype = "se"
)

#---------------------------------------------------------------------
overall_gap <- median_overall %>% 
	as_tibble() %>%
	pivot_wider(
		names_from = sex, 
		values_from = c("dwincome_monthly", "se.dwincome_monthly")
	) %>% 
	rename(
		med_incomem = dwincome_monthly_hombre,
		med_incomew = dwincome_monthly_mujer,
		se_m = se.dwincome_monthly_hombre,
		se_w = se.dwincome_monthly_mujer
	) %>% 
	mutate(
		gap = med_incomem - med_incomew,
		se_gap = sqrt(se_m^2 + se_w^2),
		ci_lower = gap - 1.96 * se_gap,
		ci_upper = gap + 1.96 * se_gap,
		significant = sign(ci_lower) == sign(ci_upper)
	) %>% 
	mutate(
		coeff_variation = se_gap/gap*100,
		cv_diagnostic = case_when(
			coeff_variation <= 30 ~ "Reliable",
			between(coeff_variation, 30.1, 35) ~ "Use with Caution",
			TRUE ~ "unreliable")
	) %>% 
	mutate(
		gap_ratio = 
	((med_incomem - med_incomew)/med_incomem)*100
	) %>% 
	relocate(c("gap", "gap_ratio", "significant", "cv_diagnostic"), .after = med_incomew)

#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#--------------- DATA ANALYSIS - GENERAL ANALYSIS  -------------------
#=====================================================================
overall_gap
gap_analysis(dwincome, "ethnicity") %>% print(n = Inf)
gap_analysis(dwincome %>% filter(ethnicity == "aymara"), "contract_var") %>% print(n = Inf)
gap_analysis(dwincome, "dominio_summary") %>% print(n = Inf)
gap_analysis(dwincome, "income_group") %>% print(n = Inf)
gap_analysis(dwincome, "sector_var") %>% print(n = Inf)
gap_analysis(dwincome, "contract_var") %>% print(n = Inf)
gap_analysis(dwincome, "income_quartile") %>% print(n = Inf)
gap_analysis(dwincome, "pay_frequency") %>% print(n = Inf)
#-----------------------------  END ----------------------------------
#=====================================================================

#=====================================================================
#------- PP GAP ANLLYSIS AND PROFILE BY SEX & VARIABLES --------------
#=====================================================================
gap_analysis(dwincome %>% 
		filter(department %in% dep_filter), "education_group")
dep_filter <- gap_analysis(
		dwincome, "department", 
		filter_by = c("data_reliability", "worst_off")) %>% 
	pull(department)

profile_comparison_multiple(
	dwincome %>% 
		filter(department %in% dep_filter), 
	profile)

#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#------------------ PROFILE ANALYSIS BY PROVINCE ---------------------
#=====================================================================
gap_analysis(dwincome, "province", threshold = 500) 
prov_filter <- gap_analysis(
		dwincome, "province", 
		threshold = 500, 
		filter_by = c("data_reliability", "worst_off")) %>% 
	pull(province)

profile_comparison_multiple(
	dwincome 
		%>% filter(province %in% prov_filter), 
	profile)

#-----------------------------  END ----------------------------------
#=====================================================================


colnames(dwincome)
