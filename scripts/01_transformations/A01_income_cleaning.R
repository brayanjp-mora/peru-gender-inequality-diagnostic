# Setting up the Environment
source(here::here("code", "00_cleaning_setup.r"), encoding = "UTF-8")
# survey_download(module_code = "05")
#=====================================================================
#-----------------------------  VARIABLES ----------------------------
#=====================================================================
vars_500 <- c(
	  "aÑo", "p207", "p208a", "ubigeo", "p301a",
	  "p203", "p209","p558c", "p505r4", "p506r4", 
		"p507", "p510", "p511a", "p523", "i524e1", 
		"i530a", "p558d2_1", "p513t", "p513a1", 
		"p513a2", "p558d2_2", "p512a", 
		"fac500a", "conglome", "estrato", "dominio",
    paste0("p560t_0", 1:9), paste0("p560a1_0", 1:9))

#=====================================================================
#-----  SURVEY VARIALES ASSESSMENT (OPTIONAL / DIAGNOSTIC ONLY) ------
#=====================================================================
check_variable_consistency("module05", vars = vars_500)
get_levels(module = "module05", vars = paste0("p560a1_0", 1:9))
get_labels(module = "module05", vars = "p560t_01")
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#--------------- DATA PROCESSING TO SELECT VARIABLES -----------------
#=====================================================================
process_module_files(
	"module05", 
	vars = vars_500, 
	output_prefix = "income"
)

#---------------------------------------------------------------------
rm(vars_500, process_module_files)
gc()
#-------------------------------- END --------------------------------
#=====================================================================


#=====================================================================
#------ SURVEY(2017:2024) DATA CLEANING/PROCESSING - MODULE05 --------
#=====================================================================
income_files <- list.files(
	"Data/processed_data", 
	pattern = "income_\\d{4}.rds$", 
	full.names = TRUE)
income <- map_dfr(income_files, readRDS)
cno_rev4_cleaning <- function(df) {
  df %>%
    mutate(
      p505r4 = str_sub(str_pad(as.double(p505r4), 4, "left", "0"), 1, 1)
    ) %>%
    left_join(cno_2015, by = "p505r4") %>%
    mutate(
      p506r4 = as.double(p506r4),
      p506r4 = str_sub(str_pad(p506r4, 4, "left", "0"), 1, 2),
      p506r4 = as.double(p506r4),
      alpha_p506r4 = case_when(
        between(p506r4, 1, 3) ~ "A", between(p506r4, 5, 9) ~ "B",
        between(p506r4, 10, 33) ~ "C", p506r4 == 35 ~ "D",
        between(p506r4, 36, 39) ~ "E", between(p506r4, 41, 43) ~ "F",
        between(p506r4, 45, 47) ~ "G", between(p506r4, 49, 53) ~ "H",
        between(p506r4, 55, 56) ~ "I", between(p506r4, 58, 63) ~ "J",
        between(p506r4, 64, 66) ~ "K", p506r4 == 68 ~ "L",
        between(p506r4, 69, 75) ~ "M", between(p506r4, 77, 82) ~ "N",
        p506r4 == 84 ~ "O", p506r4 == 85 ~ "P",
        between(p506r4, 86, 88) ~ "Q", between(p506r4, 90, 93) ~ "R",
        between(p506r4, 94, 96) ~ "S", between(p506r4, 97, 98) ~ "T",
        p506r4 == 99 ~ "U", TRUE ~ NA_character_
      )
    ) %>%
    left_join(rev4, by = "alpha_p506r4")
}

#=====================================================================
public_transport <- function(df) {
  df %>%
    mutate(
      uses_public_transport = case_when(
        if_any(starts_with("p560t_"), ~.x == 1) ~ "yes", 
        TRUE ~ "no")
      ) %>% 
    mutate(
      transport_frequency = case_when(
        if_any(starts_with("p560a1_"), ~.x == 1) ~ "daily",
        if_any(starts_with("p560a1_"), ~.x %in% c(2, 9, 10, 11)) ~ "frequent",
        if_any(starts_with("p560a1_"), ~ !is.na(.x) & !(.x %in% c(1, 2, 9, 10, 11))) ~ "occasional",
        TRUE ~ "never"
      ))
}

#=====================================================================
rename_and_convert <- function(df) {
  df %>%
    mutate(
      year = as.double(year),
      across(where(is.labelled), as_factor),
      across(where(is.factor), as.character)
    ) %>%
    rename(
      pay_frequency = p523, sex = p207, age = p208a, sector = p510,
      last_year_of_education = p301a, relation_to_head = p203,
      marital_status = p209, contract = p511a, class_of_worker = p507,
      workplace_location = p558d2_1, exact_workplace_location = p558d2_2,
      survey_device = ticuest01a, ethnicity = p558c, years_worked = p513a1,
      months_worked = p513a2, thours_worked_lastweek = p513t,
      num_of_workers = p512a
    )
}

#=====================================================================
clean_categorical_variables <- function(df) {
  df %>%
    mutate(
      across(c(estrato, sector, last_year_of_education, relation_to_head, contract, workplace_location, num_of_workers), clean_text),
      sector = if_else(sector == "empresas especiales de servicios (service)", "empresa de servicios especiales (service)", sector),
      last_year_of_education = case_when(
        last_year_of_education == "secun. incompleta" ~ "secundaria incompleta",
        last_year_of_education == "secun. completa" ~ "secundaria completa",
        last_year_of_education %in% c("post-grado universitario", "postgrado universitario") ~ "maestria/doctorado",
        TRUE ~ last_year_of_education
      ),
      relation_to_head = case_when(
        relation_to_head == "jefe/jefa del hogar" ~ "jefe/jefa",
        relation_to_head == "esposo/esposa" ~ "esposo(a)/companero(a)",
        relation_to_head == "hijo/hija" ~ "hijo(a)/hijastro(a)",
        relation_to_head == "nieto" ~ "nieto(a)",
        relation_to_head == "panel" ~ NA_character_,
        TRUE ~ relation_to_head
      ),
      contract = case_when(
        str_starts(contract, "contrato indefinido") ~ "Contrato indefinido, nombrado, permanente",
        str_starts(contract, "contrato a plazo fijo") ~ "Contrato a plazo fijo (Sujeto a modalidad)",
        contract == "esta en periodo de prueba" ~ "Está en periodo de prueba",
        str_starts(contract, "convenios de formacion") ~ "Convenios de Formación Laboral Juvenil / Prácticas Pre-profesionales",
        str_starts(contract, "contrato por locacion") ~ "Contrato por locación de servicios (Honorarios profesionales, RUC)",
        str_starts(contract, "regimen especial de contratacion") | str_starts(contract, "regimes especial de contratacion") ~ "Régimen Especial de Contratación Administrativa (CAS)",
        TRUE ~ contract
      ),
      workplace_location = if_else(str_starts(workplace_location, "aqui"), "aqui, en este distrito", workplace_location),
      survey_device = case_when(
        survey_device == "cuestionario en hojas" ~ "cuestionario en hojas",
        survey_device %in% c("cuestionario en pda", "cuestionario en tablet") ~ "cuestionario en CAPI",
        TRUE ~ survey_device
      )
    )
}


#=====================================================================
create_derived_features <- function(df) {
  df %>%
    mutate(
      dominio = clean_text(dominio),
      dominio_summary = case_when(
        str_starts(dominio, "sierra") ~ "sierra",
        str_starts(dominio, "costa") ~ "costa",
        str_starts(dominio, "selva") ~ "selva",
        TRUE ~ NA_character_
      ),
      tenure_months = (years_worked * 12) + months_worked
    )
}

#=====================================================================
categories <- function(df) {
  df %>%
    mutate(age_categories = case_when(
		between(age, 14, 25) ~ "14–25 years old",
		between(age, 26, 40) ~ "26–40 years old",
		between(age, 41, 55) ~ "41–55 years old",
		between(age, 56, 100) ~ "56+ years old",
		TRUE ~ NA_character_)) %>%
	mutate(sector_var = case_when(
		sector == "administracion publica" ~ "Public",
		sector == "fuerzas armadas, policia nacional del peru (militares)" ~ "Public",
		sector == "empresa publica" ~ "Public",
		sector == "empresa o patrono privado" ~ "Private",
		sector == "empresa de sertvicios especiales (service)" ~ "Private",
		TRUE ~ NA_character_)) %>%
	mutate(contract_var = case_when(
		contract == "Contrato indefinido, nombrado, permanente" ~ "Indefinite Contract",
		contract == "Está en periodo de prueba"  ~ "Indefinite Contract",
		contract == "Sin contrato" ~ "No Contract",
		TRUE ~ "Other type of contract")) %>%
  mutate(education_group = case_when(
    last_year_of_education %in% c("sin nivel", "inicial", "basica especial", 
                                  "primaria incompleta", "primaria completa") ~ "Basic",
    last_year_of_education %in% c("secundaria incompleta", "secundaria completa",
											 "superior no universitaria incompleta", "superior universitaria incompleta") ~ "Intermediate",
    last_year_of_education %in% c( 
                                  "superior no universitaria completa",
                                  "superior universitaria completa", 
                                  "maestria/doctorado") ~ "Advanced",
    TRUE ~ NA_character_ # Handle NAs
  )) 
}

#=====================================================================
processed_income_step1 <- income %>%
  cno_rev4_cleaning() %>%
  public_transport() %>%
  rename_and_convert() %>%
  clean_categorical_variables() %>%
  categories() %>% 
  create_derived_features() %>%
  select(
    -c(
      "p506r4", "p505r4", 
      "alpha_p506r4", paste0("p560t_0", 1:9),
      paste0("p560a1_0", 1:9)
    )
  )

#=====================================================================
# 1. Save PRIVATE Dataset (Full Data)
saveRDS(
	processed_income_step1, 
	file = here::here("Data/processed_data/processed_income_step1.rds"))

# 2. Save PUBLIC Dataset (Anonymized Data)
processed_income_step1 %>%
  ensure_data_privacy(threshold = 200) %>%
  saveRDS(file = here::here("Data/processed_data/processed_income_public.rds"))
#-------------------------------- END --------------------------------
#=====================================================================


#=====================================================================
#-------------------------- DATA REVIEW ------------------------------
#=====================================================================
glimpse(processed_income_step1)
# rm(list = ls()) Use to clean the environment!
# gc()a
