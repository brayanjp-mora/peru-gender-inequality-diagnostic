packages = c(
	"tidyverse", "janitor", "haven", "zip",
	"stringi", "here")

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
rm(list = ls())

#=====================================================================
#-----------  SURVEY VARIALES ASSESSMENT (FUNCTIONS)------------------
#=====================================================================
# Function to check if all required variables are present in the data files
check_variable_consistency <- function(module, vars) {
  # List all files in the specified module directory
	files <- list.files(file.path("data/raw_data", module), full.names = TRUE)
  # Loop through each file and check for missing variables
	for(i in seq_along(files)){
    # Read only the variable names from the .dta file
		data <- names(read_dta(files[i], n_max = 0))
    # Identify missing variables
		missing <- setdiff(vars, data)
    # If there are missing variables, print them
		file <- basename(files[i])
		print(paste0(file, " is missing:"))
		print(missing)
	}
  }

#---------------------------------------------------------------------
# Function to extract variable levels from .dta files
get_levels <- function(module, vars) {
  # List all files in the specified module directory
	files <- list.files(file.path("data/raw_data", module), full.names = TRUE)
  get_vars_levels <- function(file, vars) {
    meta <- read_dta(file, n_max = 0)
    existing_vars <- intersect(vars, names(meta))
    
    if (length(existing_vars) == 0) return(NULL)
    
    levels_list <- map(existing_vars, ~ attr(meta[[.x]], "labels"))
    names(levels_list) <- existing_vars
    
    if (length(vars) == 1) return(levels_list[[1]])
    return(levels_list)
  }
  # Apply the get_vars_levels function to each file
	results <- map(files, get_vars_levels, vars)
  # Name the results with the base names of the files
  names(results) <- basename(files)
  # Return the results
	return(results)
}
#---------------------------------------------------------------------
# Function to extract variable labels from .dta files
get_labels <- function(module, vars) {
  # List all files in the specified module directory
	files <- list.files(file.path("data/raw_data", module), full.names = TRUE)
  get_vars_label <- function(file, vars) {
    meta <- read_dta(file, n_max = 0)
    existing_vars <- intersect(vars, names(meta))
    
    if (length(existing_vars) == 0) return(NULL)
    
    labels_list <- map(existing_vars, ~ attr(meta[[.x]], "label"))
    names(labels_list) <- existing_vars
    
    if (length(vars) == 1) return(labels_list[[1]])
    return(labels_list)
  }
  # Apply the get_vars_label function to each file
	results <- map(files, get_vars_label, vars)
  names(results) <- basename(files)
	return(results)
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#---------- FUNCTION TO SELECT VARIABLES ACROSS ALL FILES ------------
#=====================================================================
process_module_files <- function(module, vars, output_prefix) {
	files <- list.files(file.path("data/raw_data", module), full.names = TRUE)

	process_single_file <- function(file, vars, output_prefix) {
		df <- read_dta(file)

		# keep only existing ones
		df <- df %>% select(any_of(vars))

		# add missing ones as NA
		missing_vars <- setdiff(vars, names(df))
		if (length(missing_vars) > 0) {
			df[missing_vars] <- NA
		}

		# rename year and save
		df <- df %>% rename(year = aÑo)
		year <- unique(df$year)
		output_path <- file.path("data/processed_data", paste0(output_prefix, "_", year, ".rds"))
		saveRDS(df, output_path)
		message("Finished processing (selecting variables): ", year)
	}
	walk(files, ~ process_single_file(.x, vars = vars, output_prefix = output_prefix))
}
#-----------------------------  END ----------------------------------
#=====================================================================


#=====================================================================
#--------------- FUNCTION TO CLEAN COMMON TEXT ISSUES ----------------
#=====================================================================
clean_text <- function(text) {
  text <- str_to_lower(text) # Convert to lowercase
  text <- stri_trans_general(text, "Latin-ASCII") # Remove accents
  text <- str_trim(text) # Remove leading/trailing spaces
  return(text)
}
#-----------------------------  END ----------------------------------
#=====================================================================

#=====================================================================
#------------------------ STANDARDIZED CODES  ------------------------
#=====================================================================
# ISCO AND ISIC GUIDES (PERU ADAPTED)
#Creating ISCO-08 (Peruvian Adpated) big groups 
cno_2015 <- tibble(
	p505r4 = as.character(c(0:9)),
	cno = c(
		"Ocupaciones militares y policiales", 
		"Miembros del Poder Ejecutivo, Legislativo, Judicial y personal directivo de la administración pública y privada",
		"Profesionales, Científicos Intelectuales",
		"Profesionales técnicos",
		"Jefes y empleados administrativos",
		"Trabajadores de los servicios y vendedores de comercios y mercados",
		"Agricultores y trabajadores calificados agropecuarios, forestales pesqueros",
		"Trabajadores de la construcción, edificación, productos artesanales, electricidad y las telecomunicaciones",
		"Operadores de maquinaria industrial, ensambladores y conductores de transporte",
		"Ocupaciones elementales"
	)
) %>% 
	mutate(
		cno = stri_trans_general(cno, "Latin-ASCII"),
		cno = str_to_lower(cno),
		cno = str_trim(cno))

#Creating ISIC (Peruvian Adpated) big groups
rev4 <- tibble(
	alpha_p506r4 = LETTERS[1:21],
	rev4 = c(
		"Agricultura, ganadería, silvicultura y pesca",
		"Explotación de minas y canteras",
		"Industrias manufactureras",
		"Suministro de electricidad, gas, vapor y aire acondicionado",
		"Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación",
		"Construcción",
		"Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas",
		"Transporte y almacenamiento",
		"Actividades de alojamiento y servicios de comidas",
		"Información y comunicaciones",
		"Actividades financieras y de seguros",
		"Actividades inmobiliarias",
		"Actividades profesionales, científicas y técnicas",
		"Actividades de servicios administrativos y de apoyo",
		"Administración pública y defensa; planes de seguridad social de afiliación obligatoria",
		"Enseñanza",
		"Actividades de atención de la salud humana y de asistencia social",
		"Actividades artísticas, de entretenimiento y recreativas",
		"Otras actividades de servicios",
		"Actividades de los hogares como empleadores; actividades no diferenciadas de los hogares como productores de bienes y servicios para uso propio",
		"Actividades de organizaciones y órganos extraterritoriales"
	)
) %>% 
	mutate(
		rev4 = stri_trans_general(rev4, "Latin-ASCII"),
		rev4 = str_to_lower(rev4),
		rev4 = str_trim(rev4))


