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
install.packages("tidyr", repos = "http://cran.us.r-project.org")
install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("digest", repos = "http://cran.us.r-project.org")


# Load libraries
library(remotes)
library(usethis)
library(REDCapR)
library(ggplot2)
library(lubridate)
library(purrr)
library(ggrepel)
library(dplyr)
library(digest)

# If running locally, your API token will be stored here:
# usethis::edit_r_environ()
# Otherwise, store on github secrets - see README.md for more details

# Load Data --------------------------------------------

agri_casa_token <- Sys.getenv("agri_casa_token")
namru_biofire_token <- Sys.getenv("namru_biofire_token")
influenza_token <- Sys.getenv("influenza_token")
salt_token <- Sys.getenv("salt_token")


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

# Looking at muestras from FunSalud Laboratory (only from symptomatic individuals)------------------

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
                                                       pmin(influenza$resul_covid_19_all, influenza$resul_sars_all))))

# Create a new column summarizing Influenza
influenza <- influenza%>%
  dplyr::mutate(resul_inf_all = ifelse(is.na(resul_inf_a_all) & is.na(resul_inf_b_all), NA,
                                         ifelse(is.na(resul_inf_a_all) & (!is.na(resul_inf_b_all)), resul_inf_b_all,
                                                ifelse((!is.na(resul_inf_a_all)) & is.na(resul_inf_b_all), resul_inf_a_all,
                                                       pmin(resul_inf_a_all, resul_inf_b_all, na.rm = TRUE)))))


# Create a new column summarizing all viruses together
influenza$resul_virus_all <- pmin(influenza$resul_inf_a_all,
                                 influenza$resul_inf_b_all,
                                 influenza$resul_rsv_all,
                                 influenza$resul_sars_all,
                                 influenza$resul_covid_19_all, na.rm = TRUE)


# Next, we want to filter out the samples that are not regularly taken
# From what I understand,
# we only want samples taken during the visita sintomática to understand the trends

influenza_muestras <- influenza %>%
  dplyr::filter(tipo_visita_lab___2==1)


# Issue 2: What if the same individual is tested multiple times in the same week?
# We don't care if the result is the same
# We do care if one is positive (always prefer the positive value)
# Since the minimum value is the preferred data type (1 = positve, 2 = negative), select for minimum value

filter_group_slice <- function(data, column) {
  data %>%
    dplyr::filter(!is.na(!!sym(column))) %>%
    dplyr::group_by(record_id, epiweek_recolec) %>%
    dplyr::slice(which.min(!!sym(column)))%>%
    dplyr::select(c(record_id, epiweek_recolec, column))
}

columns_to_process <- c("resul_inf_a_all", 
                        "resul_inf_b_all",
                        "resul_inf_all",
                        "resul_rsv_all", 
                        "resul_sars_all",
                        "resul_covid_19_all", 
                        "resul_sars_covid_all",
                        "resul_virus_all")

# Create a place to store summarized data
summary_dataframes <- list()

# Apply the function to each column and create new dataframes
for (col in columns_to_process) {
  summary_dataframes[[col]] <- filter_group_slice(influenza_muestras, col)
}

# Merge all dataframes together so that we have data per person per week
merged_summary <- Reduce(function(x, y) merge(x, y, by = c("record_id", "epiweek_recolec"), all = TRUE), summary_dataframes)

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

# Apply the function to each column and combine results into a long dataframe
summary_combined <- lapply(columns_to_process, function(col) generate_summary(merged_summary, col)) %>%
  dplyr::bind_rows()%>%
  tidyr::pivot_wider(names_from = disease,
                     values_from = c(count_all, count_neg, count_pos, pct_pos))


# Save the summary dataframe -----------------------------------------------
influenza_csv_file <- "docs/influenza_summary_updated.csv"
write.csv(summary_combined, file = influenza_csv_file, row.names = FALSE)

# ----------------------------------------------------------------------------
# --------------------Prepare Agri-Casa dataset ------------------------------
# ----------------------------------------------------------------------------

# CREATE A VIRUS INCIDENCE TRACKER DATASET---------------------------------------

# Begin processing pcr positive data --------------------------------------------------

# Crear fecha
agri_casa$epiweek_muestra_funsalud <-floor_date(agri_casa$fecha_recoleccion_m, unit = "week", week_start = 1)

# Look at singleplex results (symtomatic and visitas intensivas) -------------
# Only contains information for SARS-COV-2

# Make follow-up values numeric and match the code for the COBAS results
agri_casa$ctgen_rep_resul_num <- ifelse(
                                    is.na(agri_casa$ctgen_rep_resul), NA,
                                        ifelse(
                                          agri_casa$ctgen_rep_resul=="POSITIVO", 1,
                                               ifelse(
                                                 agri_casa$ctgen_rep_resul=="NEGATIVO", 2,
                                                      ifelse(
                                                        agri_casa$ctgen_rep_resul=="INVÁLIDO" | agri_casa$ctgen_rep_resul=="INCONCLUSO", 3, NA)
                                                 )
                                          )
                                    )

# Create overall numeric variable using original and repeated results
agri_casa$vctg_resul_num <- ifelse(
  is.na(agri_casa$vctg_resul) & is.na(agri_casa$ctgen_rep_resul_num), NA,
  ifelse(
    agri_casa$vctg_resul == "POSITIVO", 1,
    ifelse(
      agri_casa$vctg_resul == "NEGATIVO", 2,
      ifelse(
        (agri_casa$vctg_resul == "INVÁLIDO" | agri_casa$vctg_resul == "INCONCLUSO") & !is.na(agri_casa$ctgen_rep_resul_num),
        agri_casa$ctgen_rep_resul_num,
        ifelse(
          (agri_casa$vctg_resul == "INVÁLIDO" | agri_casa$vctg_resul == "INCONCLUSO") & is.na(agri_casa$ctgen_rep_resul_num),
          3,
          NA)
        )
      )
    )
  )

# Check that no information was lost
# table(is.na(agri_casa$vctg_resul_num))[1] >= table(is.na(agri_casa$vctg_resul))[1]


# Look at COBAS results (PCR for symptomatics) and singleplex --------------------------------
# Issue 1: How to deal with follow-up results (if sample is unprocessed or indeterminant)
# We can create a new column for this
create_new_column_agri_casa <- function(df, col1, col2, new_col) {
  df[[new_col]] <- ifelse(is.na(df[[col1]]) & is.na(df[[col2]]), NA,
                          ifelse(df[[col1]] == 1, 1,
                                 ifelse(df[[col1]] == 2, 2,
                                        ifelse(df[[col1]] == 3 & is.na(df[[col2]]), 3,
                                               ifelse(df[[col1]] == 4 & is.na(df[[col2]]), 4,
                                                      # Use second result if first results are inconclusive or invalid
                                                      # If anything else, then put NA
                                                      ifelse(df[[col1]] >= 3 & !is.na(df[[col2]]), df[[col2]], NA)
                                               )
                                        )
                                 )
                          )
  )
  return(df)
}

# Columns to iterate over
columns_to_iterate_agricasa <- list(
  list("sars_cov2", "sarvs_cov2_repit", "sars_cov2_all_funsalud"),
  list("inf_a", "inf_a_repit", "inf_a_all_funsalud"),
  list("inf_b", "inf_b_repit", "inf_b_all_funsalud"),
  list("vsr", "vsr_repit", "vsr_all_funsalud"))

# Apply the function to each pair of columns
for (cols in columns_to_iterate_agricasa) {
  agri_casa <- create_new_column_agri_casa(agri_casa, cols[[1]], cols[[2]], cols[[3]])
}

# Look at VSG results only? NO HAY NADA EN ESTA SECCION ----------------------
# table(agri_casa$pcr_srv, useNA = "always")

# Find positive or negative status by week per individual ----------------------
# Issue 2: What if the same individual is tested multiple times in the same week?
# We don't care if the result is the same
# We do care if one is positive (always prefer the positive value)
# Since the minimum value is the preferred data type (1 = positve, 2 = negative), select for minimum value
filter_group_slice_agri_casa <- function(data, column) {
  data %>%
    dplyr::filter(!is.na(!!sym(column))) %>%
    dplyr::group_by(record_id, epiweek_muestra_funsalud) %>%
    dplyr::slice(which.min(!!sym(column)))%>%
    dplyr::select(c(record_id, epiweek_muestra_funsalud, column))
}

columns_to_process_agri_casa <- c("sars_cov2_all_funsalud",
                                  "inf_a_all_funsalud",
                                  "inf_b_all_funsalud",
                                  "vsr_all_funsalud",
                                  "vctg_resul_num")

# Create a place to store summarized data
summary_dataframes_agri_casa <- list()

# Apply the function to each column and create new dataframes
for (col in columns_to_process_agri_casa) {
  summary_dataframes_agri_casa[[col]] <- filter_group_slice_agri_casa(agri_casa, col)
}

# Merge all dataframes together so that we have data per person per week
merged_summary_agri_casa <- Reduce(function(x, y) merge(x, y, by = c("record_id", "epiweek_muestra_funsalud"),
                                                        all = TRUE), summary_dataframes_agri_casa)

# Create a combined column for sars_cov2 using singleplex and COBAS results---------------
merged_summary_agri_casa <- merged_summary_agri_casa%>%
  dplyr::mutate(sars_cov2_singleplex_cobas = ifelse(is.na(vctg_resul_num) &
                                                                is.na(sars_cov2_all_funsalud), NA,
                                                              pmin(vctg_resul_num, sars_cov2_all_funsalud, na.rm = TRUE)))


# Begin processing intensive visit data --------------------------------------------------

# ISSUE 3: PCR won't always capture everyone.
# It is better to check how many people went through a visita intensiva

# Start by filtering for the first visit in the first week
first_intensive_df <- agri_casa%>%
  # Filter for first visit on first week, with subject = 1 (patient)
  dplyr::filter(semana_seg_visit_intens==1 & num_vist_sem_visit_intens==1 & tipo_sujeto_visit_intens==1)

# Create a date column for the positive test by week
first_intensive_df$date_positive_intens <- ifelse(!is.na(first_intensive_df$fecha_prue_visit_intens),
                                                  first_intensive_df$fecha_prue_visit_intens,
                                                  ifelse( !is.na(first_intensive_df$fecha_visit_intens), 
                                                          first_intensive_df$fecha_visit_intens, NA))

first_intensive_df$date_positive_intens <- as.Date(first_intensive_df$date_positive_intens, origin = "1970-01-01")

# Create an epiweek column to obscure personal protective information
first_intensive_df$epiweek_positive_intes <- floor_date(first_intensive_df$date_positive_intens, unit = "week", week_start = 1)


# Create denominator -----------------------------------------------------------------------------
# ISSUE 4: We need a denominator. How many people were considered/eligible for the visita intensiva or a pcr test?
# We can look by week at the total number of individuals undergoing visita intensiva y vigilancia de rutina
# if PCR test, then underwent a visita de rutina or a visita intensiva around the same time.
# We will not use total number of PCR tests as the denominator, as PCR tests are only run if an individual is symptomatic
# or a part of the visita intensiva (someone else in their household is symptomatic)

incidence_intens_denominator <- agri_casa%>%
  # Filter for accomplished vigilancia de rutina or accomplished visita intensiva
  dplyr::filter(realizado_vig_rut==1 | se_realizo_visit_intens==1)

# Rows for vigilancia de rutina and visita intensivas are mutually exclusive
# table(incidence_intens_denominator$realizado_vig_rut, incidence_intens_denominator$se_realizo_visit_intens, useNA = "always")

# Create date variable and epiweek variable based on routine and intensive visits
incidence_intens_denominator_count <- incidence_intens_denominator%>%
  mutate(date_denominator = ifelse(realizado_vig_rut==1, fecha_visita_vig_rut,
                                                        ifelse(se_realizo_visit_intens==1, fecha_visit_intens, NA)),
         epiweek_denominator = floor_date(as.Date(date_denominator, origin = "1970-01-01"), unit = "week", week_start = 1))%>%
  # select one row per individual per week
  dplyr::group_by(record_id, epiweek_denominator)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()%>%
  # count number of individuals
  dplyr::group_by(epiweek_denominator)%>%
  dplyr::summarise(denominator = n_distinct(record_id, na.rm = TRUE))


# Combine PCR and Intensive Data ----------------------------------------------------------------

merged_summary_agri_casa_subset <- merged_summary_agri_casa %>%
  dplyr::select("record_id",
                "epiweek_muestra_funsalud",
                "sars_cov2_singleplex_cobas",
                "inf_a_all_funsalud",
                "inf_b_all_funsalud",
                "vsr_all_funsalud")

# Separate the enfermedad_activacion column into three separate columns
# a no is a 2, to match the coding from funsalud pcr tests
first_intensive_df_subset <- first_intensive_df%>%
  dplyr::select(record_id, epiweek_positive_intes,
                enfermedad_activacion)%>%
  dplyr::mutate(sars_cov_intens = ifelse(is.na(enfermedad_activacion), NA,
    ifelse(enfermedad_activacion==1, 1, 2)),
    influenza_intens = ifelse(is.na(enfermedad_activacion), NA,
                              ifelse(enfermedad_activacion==2, 1, 2)),
    vsr_intens = ifelse(is.na(enfermedad_activacion), NA,
                              ifelse(enfermedad_activacion==3, 1, 2)))

pcr_intens_visit_incidence_summary_pre <- merge(merged_summary_agri_casa_subset, first_intensive_df_subset, by.x=c("record_id", "epiweek_muestra_funsalud"),
      by.y=c("record_id", "epiweek_positive_intes"), all=TRUE)


# Create overall columns
pcr_intens_visit_incidence_summary <- pcr_intens_visit_incidence_summary_pre%>%
  dplyr::mutate(
    sars_cov2_all = pmin(sars_cov2_singleplex_cobas, sars_cov_intens, na.rm = TRUE),
    influenza_all = pmin(inf_a_all_funsalud, inf_b_all_funsalud, influenza_intens, na.rm = TRUE),
    vsr_all = pmin(vsr_all_funsalud, vsr_intens, na.rm = TRUE)
  )%>%
  # because our denominator is coming from another dataset (total number of individuals with routine and intensive vists)
  # we can filter out any rows where there is not a 1 (positive result) in our columns of interest
  dplyr::filter((sars_cov2_all==1 | influenza_all==1 | vsr_all==1))


# Clean data to only include NEW cases -----------------------------------------
# ISSUE 5: we also want to filter out any results where a person was positive the previous three weeks
# We already have only ONE value per week (the minimum value)
# We filter because we are only looking at NEW infections
generate_summary_agri_casa <- function(data, column) {
  # Ensure epiweek_muestra_funsalud is in Date format
  data <- data %>%
    mutate(epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud, origin = "1970-01-01"))
  
  # Arrange data by record_id and epiweek_muestra_funsalud
  data <- data %>%
    dplyr::arrange(record_id, epiweek_muestra_funsalud)
  
  # Create a lagged column to check the value in the previous week
  data <- data %>%
    group_by(record_id) %>%
    mutate(last_record_date = lag(epiweek_muestra_funsalud, order_by = epiweek_muestra_funsalud),
           last_record_value = lag(.data[[column]], order_by = epiweek_muestra_funsalud),
           previous_week_date = epiweek_muestra_funsalud - 21) %>%
    ungroup()
  
  # Keep rows where the last_record_date is NA (have not been positive before)
  # Keep rows where the last record occured more than three weeks ago
  data <- data %>%
    dplyr::filter(is.na(last_record_date) | (previous_week_date <= last_record_date))
  
  # Generate the summary
  summary <- data %>%
    group_by(epiweek_muestra_funsalud) %>%
    summarise(
      count_pos = sum(.data[[column]]==1, na.rm = TRUE)
    ) %>%
    mutate(disease = column)
  
  return(summary)
}

# Columns to process
columns_to_process_agri_casa <- c("sars_cov2_all",
                                  "influenza_all",
                                  "vsr_all")

# Apply the function to each column and combine results into a long dataframe
summary_pos_agri_casa <- lapply(columns_to_process_agri_casa,
                                     function(col) generate_summary_agri_casa(pcr_intens_visit_incidence_summary, col)) %>%
  dplyr::bind_rows()%>%
  tidyr::pivot_wider(names_from = disease,
                     values_from = c(count_pos))


# Combine the denominator by epiweek with the positive cases-----------
summary_pos_denom_agri_casa <- merge(summary_pos_agri_casa, incidence_intens_denominator_count,
                                    by.x="epiweek_muestra_funsalud", by.y="epiweek_denominator", all=TRUE)%>%
  dplyr::filter(! is.na(epiweek_muestra_funsalud))%>%
  # NAs should be 0 because if NA, means no disease detected that week
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)),
         epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud, origin = "1970-01-01"))%>%
  # Create overall virus of interest counter
  mutate(virus_all = rowSums(across(c(sars_cov2_all, influenza_all, vsr_all)), na.rm = TRUE))



# Look at ILI syndrome counts--------------------------------------
# Create epiweek
agri_casa$epiweek_v_rutina <- floor_date(agri_casa$fecha_visita_vig_rut, unit = "week", week_start = 1)


# Create a dataset for ILI syndromic illness (difficulty breathing, fever, cough)
# We need to match the symptoms in the routine visits with the symptoms in the intensive visits
agri_symptoms_ILI <- agri_casa%>%
    dplyr::select("record_id",
                  "fecha_visita_vig_rut",
                  "realizado_vig_rut",
                  "fecha_visit_intens",
                  "se_realizo_visit_intens",
                  "tos_visit_ints",
                  "tos_flm_visit_ints",
                  "sensa_fiebre_visit_ints",
                  "falta_aire_visit_ints",
                  "tos_vig_rut",
                  "fiebre_vig_rut",
                  "dif_resp_vig_rut")%>%
  # only those with data are included
  dplyr::filter(se_realizo_visit_intens == 1 | realizado_vig_rut == 1)%>%
  # create an epiweek variable
  mutate(date_symptoms = ifelse(realizado_vig_rut==1 & is.na(se_realizo_visit_intens), fecha_visita_vig_rut,
                                   ifelse(se_realizo_visit_intens==1 & is.na(realizado_vig_rut), fecha_visit_intens, NA)),
         epiweek_symptoms = floor_date(as.Date(date_symptoms, origin = "1970-01-01"), unit = "week", week_start = 1))%>%
  dplyr::group_by(epiweek_symptoms)%>%
  dplyr::summarise(tos_count = n_distinct(record_id[tos_visit_ints >= 2 | tos_flm_visit_ints >= 2 | tos_vig_rut == 1]),
                   fiebre_count = n_distinct(record_id[sensa_fiebre_visit_ints >= 2 | fiebre_vig_rut == 1]),
                   falta_aire_count = n_distinct(record_id[falta_aire_visit_ints >= 2 | dif_resp_vig_rut == 1]),
                   total_ili_count = n_distinct(record_id[tos_visit_ints >= 2 | tos_flm_visit_ints >= 2 | tos_vig_rut == 1 |
                                                            sensa_fiebre_visit_ints >= 2 | fiebre_vig_rut == 1 |
                                                            falta_aire_visit_ints >= 2 | dif_resp_vig_rut == 1]))



# Combine with testing data and denominators
summary_combined_agri_casa <- merge(summary_pos_denom_agri_casa, agri_symptoms_ILI,
                                    by.x="epiweek_muestra_funsalud", by.y="epiweek_symptoms", all=TRUE)

# Save the summary dataframe--------------------------------------
agri_casa_csv_file <- "docs/agri_casa_summary_updated.csv"
write.csv(summary_combined_agri_casa,
          file = agri_casa_csv_file, row.names = FALSE)


# CREATE A SYMPTOM TRACKER DATASET--------------------------------------------

# The goal here is to look at how many people have experienced the following symptoms in the last 24 hours
# Please note that this does not mean NEW symptoms; symptoms could have carried over from the last week
columns_sintomas_vigilancia_rutina <- c("tos_vig_rut", "dol_gargan_vig_rut", "dol_cabeza_vig_rut",
                                        "cong_nasal_vig_rut", "fiebre_vig_rut", "dol_cuerp_musc_vig_rut",
                                        "fatica_vig_rut", "vomitos_vig_rut", "diarrea_vig_rut",
                                        "dif_resp_vig_rut", "perd_olf_gust_vig_rut", "nausea_vig_rut",
                                        "sibilancias_vig_rut", "mala_alim_vig_rut", "letargo_vig_rut")

columns_sintomas_v_intens <- c("tos_visit_ints",
                               "tos_flm_visit_ints",
                               "gargt_irrit_visit_ints",
                               "dlr_cabeza_visit_ints",
                               "congest_nasal_visit_ints",
                               "sensa_fiebre_visit_ints",
                               "dlr_cuerp_dgnl_visit_ints",
                               "fatiga_visit_ints",
                               "dlr_cuello_visit_ints",
                               "interrup_sue_visit_ints",
                               "silbd_resp_visit_ints",
                               "falta_aire_visit_ints",
                               "perd_apet_visit_ints",
                               "nausea_visit_ints",
                               "diarrea_visit_ints",
                               "vomito_visit_ints",
                               "dism_gust_visit_ints",
                               "dism_olf_visit_ints",
                               "dism_aud_visit_ints",
                               "dism_bal_visit_ints",
                               "mala_alim_visit_ints",
                               "letargo_visit_ints"
)

# Select only relevant symptoms (recorded in intensive and routine visits)
agri_casa_symptoms <- agri_casa%>%
  dplyr::select(c("record_id",
                  fecha_visita_vig_rut,
                  realizado_vig_rut,
                  fecha_visit_intens,
                  se_realizo_visit_intens,
                  all_of(columns_sintomas_vigilancia_rutina),
                  all_of(columns_sintomas_v_intens)))%>%
  dplyr::filter(se_realizo_visit_intens == 1 | realizado_vig_rut == 1)%>%
  # create an epiweek variable
  mutate(date_symptoms = ifelse(realizado_vig_rut==1 & is.na(se_realizo_visit_intens), fecha_visita_vig_rut,
                                ifelse(se_realizo_visit_intens==1 & is.na(realizado_vig_rut), fecha_visit_intens, NA)),
         epiweek_symptoms = floor_date(as.Date(date_symptoms, origin = "1970-01-01"), unit = "week", week_start = 1))


# ISSUE 6: We don't know what someone will choose on the symptom tracker (there are a lot of possible combinations!)
# Need to save the dataset by individual, as one individual can have more than one of the symptoms specified here at the same time
# Otherwise, we would double-count that person.
# However the epiweek denominator shouldn't change, so we will want to export that in our summary dataset just for ease
# Add denominator -----------------------------------
agri_casa_symptoms_summary = merge(agri_casa_symptoms, incidence_intens_denominator_count,
                                   by.x="epiweek_symptoms", by.y="epiweek_denominator", all=TRUE)%>%
  dplyr::filter(! is.na(epiweek_symptoms))%>%
  # NAs should be 0 because if NA, means no disease detected that week
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)),
         epiweek_symptoms = as.Date(epiweek_symptoms, origin = "1970-01-01"))%>%
  # Eliminate dates if possible
  dplyr::select(-c(fecha_visita_vig_rut, realizado_vig_rut, fecha_visit_intens, se_realizo_visit_intens))


# NOTE: we did not include individuals who went through the seguimiento largo, per Dan's thoughts that
# there were not many illnesses there or symptoms.
# if we need to increase our denominator, it would be most accurate to use data from the seguimiento largo
# however, in theory, it should not change the overall positivity rate of symptoms 
# (only some participants are at risk of disease and population should be equal to vigilancia de rutina)


# For additional data security, we will re-code participants-----------------------

# Hash the record_id column to create anonymized IDs
# We use the sha256 algorithm because it is deterministic but obscures similarities between individual IDs
agri_casa_symptoms_summary_anonymized <- agri_casa_symptoms_summary %>%
  mutate(
    anonymized_id = sapply(record_id, function(x) digest::digest(x, algo = "sha256"))
  )%>%
  dplyr::select(-record_id)


# Save the summary dataframe------------------------------------
# agri_casa_symptom_csv_file <- "/Users/gabigionet/Library/CloudStorage/OneDrive-UCB-O365/PhD_Projects/Trifinio/Guatemala_Infectious_Incidence/docs/agri_casa_symptom_summary_updated.csv"
agri_casa_symptom_csv_file <- "docs/agri_casa_symptom_summary_updated.csv"
write.csv(agri_casa_symptoms_summary_anonymized, file = agri_casa_symptom_csv_file, row.names = FALSE)


# ----------------------------------------------------------------------------
# --------------------Prepare BIOFIRE dataset ------------------------------
# ----------------------------------------------------------------------------


# Create epiweek--------------------------------------------------------------

namru_biofire$epiweek_recoleccion <- floor_date(namru_biofire$fecha_recoleccion, unit = "week", week_start = 1)

# Is each individual is currently mentioned once in the dataset?
nrow(unique(namru_biofire%>%
              dplyr::filter((! is.na(result_sangre_complt)) | (! is.na(result_hispd_nasof)))%>%
              select(record_id))
     )==nrow(namru_biofire%>%
               dplyr::filter((! is.na(result_sangre_complt)) | (! is.na(result_hispd_nasof)))%>%
               select(record_id))

# Find columns with pathogen results -----------------------------------------
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

# Output data ------------------------------------------------------------------
# We want to output a list with the epiweek of a given test, the result (neg or pos), the pathogen, and the coded ID.
# Find total amount of tests run, as determined by result_sangre_complt and result_hispd_nasof
# Por ahora, excluimos los heces (no son colectados)
# table(namru_biofire$result_heces)

namru_biofire_subset <- namru_biofire%>%
  dplyr::select(record_id,
                #id_agri_lab,
                #id_agricasa_lab,
                #id_norovirus_lab,
                #id_pulsoximetria_lab,
                #id_xeno_lab, # No es de una persona -- es de un mosquito
                epiweek_recoleccion,
                result_sangre_complt,
                result_hispd_nasof,
                all_of(columns_of_interest_biofire))%>%
  dplyr::filter((! is.na(result_sangre_complt)) | (! is.na(result_hispd_nasof)))

# Clean data to only include NEW cases -----------------------------------------
# We want to filter out any results where a person was positive the previous three weeks
# We already have only ONE value per week (the minimum value)
# We filter because we are only looking at NEW infections
generate_summary_biofire <- function(data, column) {
  # Ensure epiweek_recoleccion is in Date format
  data <- data %>%
    mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion, origin = "1970-01-01"))
  
  # Arrange data by record_id and epiweek_recoleccion
  data <- data %>%
    dplyr::arrange(record_id, epiweek_recoleccion)
  
  # Create a lagged column to check the value in the previous week
  data <- data %>%
    group_by(record_id) %>%
    mutate(last_record_date = lag(epiweek_recoleccion, order_by = epiweek_recoleccion),
           last_record_value = lag(.data[[column]], order_by = epiweek_recoleccion),
           diff_last_record_date = epiweek_recoleccion - last_record_date) %>%
    ungroup()%>%
    # Replace a 1 with a 0 if the last_record_date with a 1 in that column occurred less than three weeks ago
    mutate(!!sym(column) := ifelse(
      (diff_last_record_date < 21) & (last_record_value==1) & (.data[[column]] == 1), 0, .data[[column]]))
  
  return(data)
}

# Apply the function to each column and combine results into a long dataframe
# Initialize the result with the original data
namru_biofire_summary <- namru_biofire_subset

# Loop through each column and apply the generate_summary_biofire function
for (column in columns_of_interest_biofire) {
  temp_result <- generate_summary_biofire(namru_biofire_subset, column)
}


# For additional data security, we will re-code participants-----------------------

# Hash the record_id column to create anonymized IDs
# We use the sha256 algorithm because it is deterministic
namru_biofire_summary_anonymized <- namru_biofire_summary %>%
  mutate(
    anonymized_id = sapply(record_id, function(x) digest::digest(x, algo = "sha256"))
  )%>%
  dplyr::select(-record_id)

# Save file ---------------------------------------------------
namru_biofire_csv_file <- "docs/namru_biofire_summary_updated.csv"
#namru_biofire_csv_file <- "/Users/gabigionet/Library/CloudStorage/OneDrive-UCB-O365/PhD_Projects/Trifinio/Guatemala_Infectious_Incidence/docs/namru_biofire_summary_updated.csv"

write.csv(namru_biofire_summary_anonymized, file = namru_biofire_csv_file, row.names = FALSE)
