# The purpose of this R script is to load data from Redcap and save the dataframes

# Load Packages --------------------------------------------
# Set a default CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install packages
install.packages("remotes", repos = "http://cran.us.r-project.org")
install.packages("usethis", repos = "http://cran.us.r-project.org")
install.packages("REDCapR", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("lubridate", repos = "http://cran.us.r-project.org")
install.packages("purr", repos = "http://cran.us.r-project.org")
install.packages("ggrepel", repos = "http://cran.us.r-project.org")
install.packages("dplyr", repos = "http://cran.us.r-project.org")

# Load libraries
library(remotes)
library(usethis)
library(REDCapR)
library(ggplot2)
library(lubridate)
library(purrr)
library(ggrepel)
library(dplyr)

# If running locally, your API token will be stored here:
# usethis::edit_r_environ()
# Otherwise, store on github secrets - see README.md for more details

# Load Data --------------------------------------------

agri_casa_token <- Sys.getenv("agri_casa_token")
namru_biofire_token <- Sys.getenv("namru_biofire_token")
influenza_token <- Sys.getenv("influenza_token")

uri <- "https://redcap.ucdenver.edu/api/"

influenza <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token       = influenza_token
  )$data

agri_casa <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token       = agri_casa_token
  )$data

namru_biofire <- 
  REDCapR::redcap_read(
    redcap_uri  = uri, 
    token       = namru_biofire_token
  )$data


# ----------------------------------------------------------------------------
# --------------------Prepare Influenza dataset ------------------------------
# ----------------------------------------------------------------------------

# Create epiweek
influenza$epiweek_recolec <- floor_date(influenza$fecha_recolec, unit = "week", week_start = 1)

# Issue 1: How to deal with follow-up results (if sample is unprocessed or indeterminant (3 or 5))
# We can create a new column for this
create_new_column <- function(df, col1, col2, new_col) {
  df[[new_col]] <- ifelse(is.na(df[[col1]]), NA,
                          ifelse(df[[col1]] == 1, 1,
                                 ifelse(df[[col1]] == 2, 2,
                                        ifelse(df[[col1]] == 3 & is.na(df[[col2]]), 3,
                                               ifelse(df[[col1]] == 3 & !is.na(df[[col2]]) & df[[col2]] == 3, df[[col2]], NA)
                                        )
                                 )
                          )
  )
  return(df)
}

# Columns to iterate over
columns_to_iterate <- list(
  list("resul_inf_a", "resul_inf_a_2", "resul_inf_a_all"),
  list("resul_rsv", "resul_vsr_2", "resul_rsv_all"),
  list("resul_inf_b", "resul_inf_b_2", "resul_inf_b_all"),
  list("resul_ocr_sars", "resul_pcr_sasrs2", "resul_sars_all"), # note typo here in redcap column name
  list("resul_covid_19", "resul_covid_19_2", "resul_covid_19_all")
)

# Apply the function to each pair of columns
for (cols in columns_to_iterate) {
  influenza <- create_new_column(influenza, cols[[1]], cols[[2]], cols[[3]])
}

# Create a new column summarizing SARS and COVID
influenza$resul_sars_covid_all <- ifelse(is.na(influenza$resul_sars_all) & is.na(influenza$resul_covid_19_all), NA,
                                         ifelse(is.na(influenza$resul_sars_all) & !is.na(influenza$resul_covid_19_all), influenza$resul_covid_19_all,
                                              ifelse(is.na(influenza$resul_covid_19_all) & !is.na(influenza$resul_sars_all), influenza$resul_sars_all,
                                                    min(influenza$resul_covid_19_all, influenza$resul_sars_all))))

# Issue 2: What if the same individual is tested multiple times in the same week?
# We don't care if the result is the same
# We do care if one is positive (always prefer the positive value)
# Since the minimum value is the preferred data type (1 = positve, 2 = negative), select for minimum value
influenza_by_indiv_by_epiweek <- influenza %>%
  dplyr::filter(!is.na(resul_inf_a_all)) %>%
  dplyr::group_by(record_id, epiweek_recolec) %>%
  dplyr::slice(which.min(resul_inf_a_all))

filter_group_slice <- function(data, column) {
  data %>%
    dplyr::filter(!is.na(!!sym(column))) %>%
    dplyr::group_by(record_id, epiweek_recolec) %>%
    dplyr::slice(which.min(!!sym(column)))%>%
    dplyr::select(c(record_id, epiweek_recolec, column))
}

columns_to_process <- c("resul_inf_a_all", "resul_rsv_all", "resul_inf_b_all",
                        "resul_sars_all", "resul_covid_19_all", 
                        "resul_sars_covid_all")

# Create a place to store summarized data
summary_dataframes <- list()

# Apply the function to each column and create new dataframes
for (col in columns_to_process) {
  summary_dataframes[[col]] <- filter_group_slice(influenza, col)
}

# Merge all dataframes together so that we have data per person per week
merged_summary <- Reduce(function(x, y) merge(x, y, by = c("record_id", "epiweek_recolec"), all = TRUE), summary_dataframes)

# Plot a specific disease over time
influenza_summary <- merged_summary%>%
  dplyr::group_by(epiweek_recolec)%>%
  dplyr::summarise(
    # Count total number of individuals tested per week
    count_inf_a_all = sum(!is.na(resul_inf_a_all), na.rm = TRUE),
    # Count total number of negatives per week
    count_inf_a_neg = sum(resul_inf_a_all == 2, na.rm = TRUE),
    # Count total number of positives per week
    count_inf_a_pos = sum(resul_inf_a_all == 1, na.rm = TRUE),
    # Count percentage of positives to negatives per week
    pct_inf_a_pos = ifelse(count_inf_a_pos==0, 0,
                           count_inf_a_pos/sum(count_inf_a_neg+count_inf_a_pos, na.rm=TRUE)*100)
    
  )

generate_summary <- function(data, column) {
  data %>%
    dplyr::group_by(epiweek_recolec) %>%
    dplyr::summarise(
      count_all = sum(!is.na(.data[[column]]), na.rm = TRUE),
      count_neg = sum(.data[[column]] == 2, na.rm = TRUE),
      count_pos = sum(.data[[column]] == 1, na.rm = TRUE),
      count_undetermined = sum(.data[[column]] == 3, na.rm = TRUE),
      count_unprocessed = sum(.data[[column]] == 5, na.rm = TRUE),
      pct_pos = ifelse(count_pos == 0, 0,
                       count_pos / sum(count_neg + count_pos, na.rm = TRUE) * 100)
    ) %>%
    dplyr::mutate(disease = column)
}

# Columns to process
columns_to_process <- c("resul_inf_a_all", "resul_rsv_all", "resul_inf_b_all",
                        "resul_sars_all", "resul_covid_19_all",
                        "resul_sars_covid_all")

# Apply the function to each column and combine results into a long dataframe
summary_combined <- lapply(columns_to_process, function(col) generate_summary(merged_summary, col)) %>%
  dplyr::bind_rows()%>%
  tidyr::pivot_wider(names_from = disease,
              values_from = c(count_all, count_neg, count_pos, pct_pos))

# Save the summary dataframe
influenza_csv_file <- "docs/influenza_summary_updated.csv"
write.csv(summary_combined, file = influenza_csv_file, row.names = FALSE)

# ----------------------------------------------------------------------------
# --------------------Prepare Agri-Casa dataset ------------------------------
# ----------------------------------------------------------------------------
# Create epiweek
agri_casa$epiweek_v_rutina <- floor_date(agri_casa$fecha_visita_vig_rut, unit = "week", week_start = 1)

columns_sintomas_vigilancia_rutina <- c("tos_vig_rut", "dol_gargan_vig_rut", "dol_cabeza_vig_rut",
                                        "cong_nasal_vig_rut", "fiebre_vig_rut", "dol_cuerp_musc_vig_rut",
                                        "fatica_vig_rut", "vomitos_vig_rut", "diarrea_vig_rut",
                                        "dif_resp_vig_rut", "perd_olf_gust_vig_rut", "nausea_vig_rut",
                                        "sibilancias_vig_rut", "mala_alim_vig_rut", "letargo_vig_rut")

# New symptoms only
columns_sintomas_nuevos_v_rutina <- c("sintomas_nuevos_nuevos___1",
                                      "sintomas_nuevos_nuevos___2",
                                      "sintomas_nuevos_nuevos___3",
                                      "sintomas_nuevos_nuevos___4",
                                      "sintomas_nuevos_nuevos___5",
                                      "sintomas_nuevos_nuevos___6",
                                      "sintomas_nuevos_nuevos___7",
                                      "sintomas_nuevos_nuevos___8",
                                      "sintomas_nuevos_nuevos___9",
                                      "sintomas_nuevos_nuevos___10",
                                      "sintomas_nuevos_nuevos___11",
                                      "sintomas_nuevos_nuevos___12",
                                      "sintomas_nuevos_nuevos___13",
                                      "sintomas_nuevos_nuevos___14",
                                      "sintomas_nuevos_nuevos___15")

# Select only important columns
agri_casa_summary <- agri_casa%>%
                    dplyr::select(realizado_vig_rut, sintoma_nuevo, epiweek_v_rutina,
                                  all_of(columns_sintomas_nuevos_v_rutina))

# Save the summary dataframe
agri_casa_csv_file <- "docs/agri_casa_summary_updated.csv"
write.csv(agri_casa_summary, file = agri_casa_csv_file, row.names = FALSE)

# ----------------------------------------------------------------------------
# --------------------Prepare BIOFIRE dataset ------------------------------
# ----------------------------------------------------------------------------

# Create epiweek
namru_biofire$epiweek_recoleccion <- floor_date(namru_biofire$fecha_recoleccion, unit = "week", week_start = 1)

# Columns of interest
columns_of_interest_biofire <- c(
  "patogenos_positivos_sangre___1",
  "patogenos_positivos_sangre___2",
  "patogenos_positivos_sangre___3",
  "patogenos_positivos_sangre___4",
  "patogenos_positivos_sangre___5",
  "patogenos_positivos_sangre___6",
  "patogenos_positivos_sangre___7",
  "patogenos_positivos_sangre___8",
  "patogenos_positivos_sangre___9",
  "patogenos_positivos_sangre___10",
  "patogenos_positivos_sangre___11",
  "patogenos_positivos_sangre___12",
  "patogenos_positivos_sangre___13",
  "patogenos_positivos_sangre___14",
  "patogenos_positivos_sangre___15",
  "patogenos_positivos_sangre___16",
  "patogenos_positivos_sangre___17",
  "patogenos_positivos_sangre___18",
  "patogenos_positivos_sangre___19",
  "patogenos_positivos_hisnaso___1",
  "patogenos_positivos_hisnaso___2",
  "patogenos_positivos_hisnaso___3",
  "patogenos_positivos_hisnaso___4",
  "patogenos_positivos_hisnaso___5",
  "patogenos_positivos_hisnaso___6",
  "patogenos_positivos_hisnaso___7",
  "patogenos_positivos_hisnaso___8",
  "patogenos_positivos_hisnaso___9",
  "patogenos_positivos_hisnaso___10",
  "patogenos_positivos_hisnaso___11",
  "patogenos_positivos_hisnaso___12",
  "patogenos_positivos_hisnaso___13",
  "patogenos_positivos_hisnaso___14",
  "patogenos_positivos_hisnaso___15",
  "patogenos_positivos_hisnaso___16",
  "patogenos_positivos_hisnaso___17",
  "patogenos_positivos_hisnaso___18",
  "patogenos_positivos_hisnaso___19",
  "patogenos_positivos_hisnaso___20",
  "patogenos_positivos_hisnaso___21"
)


# Vector mapping column names to pathogen names
pathogen_names <- c(
  "patogenos_positivos_sangre___1" = "Chikungunya",
  "patogenos_positivos_sangre___2" = "Fiebre hemorrágica de Crimean-Congo",
  "patogenos_positivos_sangre___3" = "Dengue",
  "patogenos_positivos_sangre___4" = "Ebola",
  "patogenos_positivos_sangre___5" = "Lassa",
  "patogenos_positivos_sangre___6" = "Marburg",
  "patogenos_positivos_sangre___7" = "West Nile",
  "patogenos_positivos_sangre___8" = "Fiebre amarilla",
  "patogenos_positivos_sangre___9" = "Zika",
  "patogenos_positivos_sangre___10" = "Bacillus anthracis",
  "patogenos_positivos_sangre___11" = "Francisella tularensis",
  "patogenos_positivos_sangre___12" = "Leptospira spp.",
  "patogenos_positivos_sangre___13" = "Salmonella enterica serovar Typhi",
  "patogenos_positivos_sangre___14" = "Salmonella enterica serovar Paratyphi A",
  "patogenos_positivos_sangre___15" = "Yersinia pestis",
  "patogenos_positivos_sangre___16" = "Leishmania spp.",
  "patogenos_positivos_sangre___17" = "Plasmodium spp.",
  "patogenos_positivos_sangre___18" = "P. falciparum",
  "patogenos_positivos_sangre___19" = "P. vivax/ovale",
  "patogenos_positivos_hisnaso___1" = "Adenovirus",
  "patogenos_positivos_hisnaso___2" = "Coronavirus HKU1",
  "patogenos_positivos_hisnaso___3" = "Coronavirus NL63",
  "patogenos_positivos_hisnaso___4" = "Coronavirus 229E",
  "patogenos_positivos_hisnaso___5" = "Coronavirus OC43",
  "patogenos_positivos_hisnaso___6" = "Metapneumovirus humano",
  "patogenos_positivos_hisnaso___7" = "Human Rhinovirus/ Enterovirus",
  "patogenos_positivos_hisnaso___8" = "Influenza A",
  "patogenos_positivos_hisnaso___9" = "Influenza A/H1",
  "patogenos_positivos_hisnaso___10" = "Influenza A/H1-2009",
  "patogenos_positivos_hisnaso___11" = "Influenza A/H3",
  "patogenos_positivos_hisnaso___12" = "Influenza B",
  "patogenos_positivos_hisnaso___13" = "Parainfluenza 1",
  "patogenos_positivos_hisnaso___14" = "Parainfluenza 2",
  "patogenos_positivos_hisnaso___15" = "Parainfluenza 3",
  "patogenos_positivos_hisnaso___16" = "Parainfluenza 4",
  "patogenos_positivos_hisnaso___17" = "Virus Sincitial Respiratorio",
  "patogenos_positivos_hisnaso___18" = "Bordetella pertussis",
  "patogenos_positivos_hisnaso___19" = "Chlamydophila pneumonia",
  "patogenos_positivos_hisnaso___20" = "Mycoplasma pneumoniae",
  "patogenos_positivos_hisnaso___21" = "Severe Acute Respiratory Syndrome Coronavirus 2 (SARS-CoV-2)"
)

# Calculate the most recent epiweek for each column that has a 1
most_recent_epiweek_df <- namru_biofire %>%
  # Gather columns of interest into long format
  tidyr::pivot_longer(cols = starts_with("patogenos_positivos"),
               names_to = "Pathogen",
               values_to = "Value") %>%
  # Filter to rows where Value is 1
  filter(Value == 1) %>%
  # Group by Pathogen and find the most recent epiweek
  group_by(Pathogen) %>%
  summarise(
    most_recent_epiweek = epiweek_recoleccion[which.max(epiweek_recoleccion[Value == 1])]
  ) %>%
  ungroup() %>%
  arrange(most_recent_epiweek)


# Calculate total counts of 1s for each column
counts <- namru_biofire %>%
  dplyr::summarise(across(all_of(columns_of_interest_biofire), ~ sum(. == 1, na.rm = TRUE)))

biofire_count <- data.frame(
  Pathogen = names(counts),
  Count = as.vector(t(counts))
)%>%
  mutate(sample_type = case_when(
    grepl("sangre", Pathogen, ignore.case = TRUE) ~ "sangre",
    grepl("hisnaso", Pathogen, ignore.case = TRUE) ~ "nasofaríngeo",
    TRUE ~ "other"  # Default case if neither "sangre" nor "hisnaso" is found
  ))%>%
  dplyr::filter(!Count==0)%>%
  dplyr::mutate(Patógeno = pathogen_names[Pathogen])

result_biofire <- merge(biofire_count, most_recent_epiweek_df, by="Pathogen", all=TRUE)%>%
  dplyr::select(Patógeno, "Numero de personas" = Count, "Tipo de Muestra" = sample_type, "Semana más reciente con resultado positivo" = most_recent_epiweek)

namru_biofire_csv_file <- "docs/namru_biofire_summary_updated.csv"
write.csv(biofire_summary, file = namru_biofire_csv_file, row.names = FALSE)