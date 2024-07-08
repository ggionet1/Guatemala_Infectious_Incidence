# The purpose of this R script is to load data from Redcap and save the dataframes

# Load Packages --------------------------------------------
# Set a default CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install packages
install.packages("remotes")
install.packages("usethis")
install.packages("REDCapR")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("purr")
install.packages("ggrepel")
install.packages("dplyr")

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