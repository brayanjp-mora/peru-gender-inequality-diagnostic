#Setting up the Environment
packages = c(
	"tidyverse", "tidylog", "janitor", "readxl",
	"sf", "stringi", "here", "geosphere")

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
#---------------------------------------------------------------------
geo_file <- list.files(
                  path = "Data/raw_data/Geography", 
                  pattern = "shp$", 
                  full.names = TRUE
                )

#=====================================================================
#-----------------------GEOGRAPHY SUPPORT FILES-----------------------
#=====================================================================
metropolitan_lima <- st_read(
  geo_file, 
  layer = "per_admbnda_adm2_ign_20200714"
) %>% 
  filter(ADM2_PCODE == "PE1501"
) %>% 
  select(ADM2_ES, ADM2_PCODE
) %>% 
  mutate(
    ADM2_ES = case_when(
      ADM2_ES == "Lima" ~ "Metropolitan Lima"
    )) %>% 
  rename(
    ADM1_ES = ADM2_ES,
    ADM1_PCODE = ADM2_PCODE)

#---------------------------------------------------------------------
geo_dep <- st_read(
    geo_file, 
    layer = "per_admbnda_adm1_ign_20200714"
) %>% 
  bind_rows(x = ., y = metropolitan_lima) 
#---------------------------------------------------------------------

geo_missing_districts <- list.files(
  "Data/raw_data/Geography", 
  pattern = "DISTRITOS", 
  full.names = TRUE)
geo_missing_districts <- st_read(geo_missing_districts) %>% 
  filter(UBIGEO == "120606" | UBIGEO == "120604") %>% 
  mutate(
    POINT_X = st_coordinates(st_centroid(geometry))[, 1],
    POINT_Y = st_coordinates(st_centroid(geometry))[, 2]) %>% 
  select(
    UBIGEO, POINT_X, POINT_Y, DISTRITO
  ) %>% 
  rename(
    ADM3_PCODE = UBIGEO,
    ADM3_ES = DISTRITO) %>% 
  mutate(across(where(is.character), str_to_title)) %>% 
  mutate(ADM3_PCODE = stri_c("PE", ADM3_PCODE)) %>% 
  st_drop_geometry()

#-------------------------------- END --------------------------------
#=====================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#=====================================================================
#-----------LAT & LON BY DEPARTMENT, PROVINCE AND DISTRICT------------
#=====================================================================
lat_lon_dis <- st_read(
            geo_file, 
            layer = "per_admbndp_admALL_ign_itos_20200714"
        ) %>% 
          select(
            ADM3_PCODE, POINT_X, POINT_Y,
            ADM3_ES
        ) %>% 
          bind_rows(., geo_missing_districts) %>%
          rename(
            ubi_district = ADM3_PCODE,
            lon_dis = POINT_X,
            lat_dis = POINT_Y,
            district = ADM3_ES
        ) %>% 
          mutate(
            ubi_district = str_sub(ubi_district, 3, 8)
        )  %>% 
          st_drop_geometry(geometry)

#---------------------------------------------------------------------
lat_lon_dep <- geo_dep %>% 
  mutate(
    lon_dep = st_coordinates(st_centroid(geometry))[, 1],
    lat_dep = st_coordinates(st_centroid(geometry))[, 2]) %>%
  select(ADM1_ES, ADM1_PCODE, lon_dep, lat_dep) %>% 
  rename(
    department= ADM1_ES, 
    ubi_department = ADM1_PCODE
  ) %>% 
    mutate(
      ubi_department = case_when(
        str_detect(ubi_department, pattern = "^PE\\d{2}$") ~ str_sub(ubi_department, 3, 4),
        TRUE ~ str_sub(ubi_department, 3, 6))
  ) %>% 
  st_drop_geometry()

#---------------------------------------------------------------------
lat_lon_prov <- st_read(
  geo_file, 
  layer = "per_admbnda_adm2_ign_20200714"
) %>% 
  mutate(
    lon_prov = st_coordinates(st_centroid(geometry))[, 1],
    lat_prov = st_coordinates(st_centroid(geometry))[, 2]
) %>% 
  select(ADM2_PCODE, ADM2_ES, lon_prov, lat_prov) %>% 
  rename(
    province = ADM2_ES,
    ubi_province = ADM2_PCODE) %>% 
  mutate(ubi_province = str_sub(ubi_province, 3, 6)) %>% 
  mutate(province = case_when(
    province == "Lima" ~ "Metropolitan Lima",
    TRUE ~ as.character(province)
  )) %>% 
  st_drop_geometry()

#---------------------------------------------------------------------
#Results to be joined with processed_income:
# lat_lon_dep
# lat_lon_dis
# lat_lon_prov 
#-------------------------------- END --------------------------------
#=====================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#=====================================================================
#----JOINING LAT & LON OF DEP, PROV, AND DIST TO PROCESSED_INCOME-----
#=====================================================================
processed_income <- list.files( #reading processed_income file
  path = "Data/processed_data", 
  pattern = "processed_income_step1\\.rds$",
  full.names = TRUE) 
processed_income <- read_rds(processed_income)

#---------------------------------------------------------------------
processed_income <- processed_income %>% 
  rename(ubi_district = ubigeo) %>% 
  #creating ubi_province from ubi_district
  mutate(
    ubi_province = str_sub(ubi_district, 1, 4)
  ) %>% 
  #adding 1501 (Metropolitan Lima) to ubi_department
  mutate(
    ubi_department = str_sub(ubi_district, 1, 2),
    ubi_department = case_when(
      str_detect(ubi_district, "^1501") ~ "1501",
      TRUE ~ as.character(ubi_department)
    )
  )  %>% 
  relocate("ubi_district", .before = "ubi_province")

#---------------------------------------------------------------------
processed_income_step2 <- processed_income %>% 
  #Joining lat_lon by their respective geographic location (ubi_*)
  left_join(., lat_lon_dep, by = "ubi_department") %>% 
  left_join(., lat_lon_prov, by = "ubi_province") %>% 
  left_join(., lat_lon_dis, by = "ubi_district") %>% 
  relocate("district", .before = "lon_dis")
#-------------------------------- END --------------------------------
#===================================================================== 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#=====================================================================
#------------------- COMMUTING DISTANCE CALCULATION ------------------
#=====================================================================
processed_income_step2 <- processed_income_step2 %>% 
  mutate(
    home_lat = lat_dis,
    home_lon = lon_dis
  ) %>% 
  mutate(
    exact_workplace_location = as.character(exact_workplace_location)
  ) %>% 
  left_join(
    x = ., 
    y = lat_lon_dis, 
    by = c("exact_workplace_location" = "ubi_district")
  ) %>% 
  rename(
    work_lon = lon_dis.y,
    work_lat = lat_dis.y,
    lat_dis = lat_dis.x,
    lon_dis = lon_dis.x
  ) %>% 
  mutate(
    commute_distance_km = distHaversine(
      p1 = cbind(home_lat, home_lon), 
      p2 = cbind(work_lat, work_lon)),
    commute_distance_km = commute_distance_km/1000
  ) %>% 
  mutate(commute_distance_cat = case_when(
    workplace_location == "aqui, en este distrito" ~ "Short",
    between(commute_distance_km, 1, 5) ~ "Short",
    between(commute_distance_km, 6, 25) ~ "Medium",
    between(commute_distance_km, 26, 50) ~ "Long",
    TRUE ~ "Very Long"
  )) %>% 
  select(-district.x) %>% 
  rename(district = district.y)

#---------------------------------------------------------------------
#saving processed_income_step2 
saveRDS(processed_income_step2, 
  file = "Data/processed_data/processed_income_step2.rds")
#-------------------------------- END --------------------------------
#===================================================================== 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#=====================================================================
#-----------LAT & LON BY DEPARTMENT, PROVINCE AND DISTRICT------------
#=====================================================================
geometry_dep <- geo_dep %>% 
  # Selecting only the geographic location (geom in there by default)
  select(ADM1_PCODE, ADM1_ES) %>% 
  # Renaming the geographic location to ubi_dep* to match our income data
  rename(
    ubi_department = ADM1_PCODE,
    department = ADM1_ES
  ) %>%
  # Adding 1501 (Metropolitan Lima) as a department to ubi_department
  mutate(ubi_department = case_when(
    # removing the "PE" from geolocation codes.
    # The department codes are the first two digits of each geolocation
    str_detect(ubi_department, "^PE\\d{2}$") ~ str_sub(ubi_department, 3, 4),
    # Metropolitan Lima is considered a province with special legal status.
    # Its geolocation code is 4 digits long.
    # Hence, we are subtracting 4 digits from it.  
    TRUE ~ str_sub(ubi_department, 3, 6)
  ))

#---------------------------------------------------------------------
geometry_prov <- st_read(
          geo_file, 
          layer = "per_admbnda_adm2_ign_20200714"
      ) %>% 
      select(ADM2_PCODE, ADM2_ES) %>% 
      rename(
        ubi_province = ADM2_PCODE,
        province = ADM2_ES) %>% 
      mutate(ubi_province = str_sub(ubi_province, 3, 6))

#---------------------------------------------------------------------
saveRDS(geometry_dep, 
  file = "Data/cleaned_data/geometry_dep.rds")
saveRDS(geometry_prov, 
  file = "Data/cleaned_data/geometry_prov.rds")

#-------------------------------- END --------------------------------
#=====================================================================

#---------------------------------------------------------------------
# rm(list = ls()) # Use to remove current variables
# gc()


