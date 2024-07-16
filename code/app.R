#library(shiny) #keep commented for app to be deployed on R Shiny
library(bslib)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(shinythemes)
library(reactable)


# Load dataframes ---------
influenza_summary <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/docs/influenza_summary_updated.csv")
agri_casa_symptom_summary <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/docs/agri_casa_symptom_summary_updated.csv")
agri_casa_incidence_summary <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/docs/agri_casa_summary_updated.csv")
namru_biofire_summary <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/docs/namru_biofire_summary_updated.csv")


# Define any needed functions -------------------------
# Function to format date labels in Spanish
format_date_spanish <- function(x) {
  months <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  paste(months[as.numeric(format(x, "%m"))], format(x, "\n %Y"), sep = " ")
}

# Define any variables needed for both server and ui ------------

# Mapping of symptoms to their corresponding columns
# Symptom mapping with formatted names
symptom_map <- list(
  "Tos" = c("tos_visit_ints >= 2", "tos_flm_visit_ints >= 2", "tos_vig_rut == 1"),
  "Fiebre" = c("sensa_fiebre_visit_ints >= 2", "fiebre_vig_rut == 1"),
  "Falta de Aire" = c("falta_aire_visit_ints >= 2", "dif_resp_vig_rut == 1"),
  "Dolor de Garganta" = c("gargt_irrit_visit_ints >= 2", "dol_gargan_vig_rut == 1"),
  "Dolor de Cabeza" = c("dlr_cabeza_visit_ints >= 2", "dol_cabeza_vig_rut == 1"),
  "Congestión Nasal" = c("congest_nasal_visit_ints >= 2", "cong_nasal_vig_rut == 1"),
  "Dolor de Cuerpo" = c("dlr_cuerp_dgnl_visit_ints >= 2", "dol_cuerp_musc_vig_rut == 1"),
  "Fatiga" = c("fatiga_visit_ints >= 2", "fatica_vig_rut == 1"),
  "Silbidos al Respirar" = c("silbd_resp_visit_ints >= 2", "sibilancias_vig_rut == 1"),
  "Náuseas" = c("nausea_visit_ints >= 2", "nausea_vig_rut == 1"),
  "Diarrea" = c("diarrea_visit_ints >= 2", "diarrea_vig_rut == 1"),
  "Vómitos" = c("vomito_visit_ints >= 2", "vomitos_vig_rut == 1"),
  "Pérdida de Olfato/Gusto" = c("dism_gust_visit_ints >= 2", "dism_olf_visit_ints >= 2", "perd_olf_gust_vig_rut == 1"),
  "Pérdida de Audición/Balance" = c("dism_aud_visit_ints >= 2", "dism_bal_visit_ints >= 2"),
  "Mala Alimentación" = c("mala_alim_visit_ints >= 2", "mala_alim_vig_rut == 1"),
  "Letargo" = c("letargo_visit_ints >= 2", "letargo_vig_rut == 1")
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
  "patogenos_positivos_sangre___13" = "Salmonella enterica, Typhi",
  "patogenos_positivos_sangre___14" = "Salmonella enterica, Paratyphi A",
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
  "patogenos_positivos_hisnaso___21" = "SARS-CoV-2",
  "Negativo_sangre" = "Negativo",
  "Negativo_hisnaso" = "Negativo"
)

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
  "patogenos_positivos_hisnaso___21",
  "Negativo_sangre",
  "Negativo_hisnaso"
)

# Create list without negatives separated by sample type
columns_sangre <- columns_of_interest_biofire[grep("sangre",
                                                   columns_of_interest_biofire)]
columns_sangre <- columns_sangre[columns_sangre != "Negativo_sangre"]

columns_hisnaso <- columns_of_interest_biofire[grep("hisnaso",
                                                    columns_of_interest_biofire)]
columns_hisnaso <- columns_hisnaso[columns_hisnaso != "Negativo_hisnaso"]


# ------------------------------------------------------
# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----

# Define UI for Tab 1
ui_tab1 <- function() {
  fluidRow(
    column(6,
           # Dropdown menu for selecting disease
           radioButtons("virus", "Virus:",
                        c("Influenza A" = "resul_inf_a_all",
                          "Influenza B" = "resul_inf_b_all",
                          "Influenza (todos)" = "resul_inf_all",
                          "VSR" = "resul_rsv_all",
                          "SARS-CoV-2 confirmado por PCR" = "resul_sars_all",
                          "SARS-CoV-2 confirmado por prueba rápida de antígenos" = "resul_covid_19_all",
                          "SARS-CoV-2 (confirmado por PCR o prueba rápida)" = "resul_sars_covid_all",
                          "Todos" = "resul_virus_all")
                        )
    ),
    column(6,
           # Date range input
           dateRangeInput("date_range_input_tab1", "Elige las fechas:",
                          start = "2020-01-01", end = Sys.Date())
    ),
    column(12,
           # Plot output for the graph
           plotOutput("disease_plot_tab1")
    )
  )
}


# Define UI for Tab 2

ui_tab2 <- function() {
  fluidPage(
    titlePanel("Enfermedades Respiratorias: síntomas y número de personas con resultados positivos"),
    
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(
          "date_range_input_tab2",
          "Filtra período del tiempo:",
          start = "2023-10-02",
          end = Sys.Date()
        ),
        # Add the virus selection and date range input similar to Tab 1
        radioButtons("virus_agri", "Virus:",
                     c("SARS-CoV-2" = "sars_cov2_all",
                       "Influenza" = "influenza_all",
                       "VSR" = "vsr_all",
                       "Todos" = "virus_all")),
          # Checkboxes for symptom selection
          checkboxGroupInput("symptoms", "Seleccionar Síntomas:",
                             choices = names(symptom_map),
                             selected = NULL)  # Initially no symptoms selected
        ),
      mainPanel(
        fluidRow(
          column(12, plotOutput("pct_plot_tab2")),  # Combined percentage incident symptom plot
          column(12, plotOutput("incidence_plot_tab2")),  #  Incidence count plot
          column(12, plotOutput("symptoms_plot_tab2"))  # Symptom count plot
        )
      )
    )
  )
}


# Define UI for Tab 3
ui_tab3 <- function() { 
  fluidPage(
    titlePanel("Enfermedades Detectadas por Namru/Biofire"),
    fluidRow(column=6,
      # Date range input
      dateRangeInput("date_range_input_tab3", "Elige las fechas:",
                     start = "2020-06-29", end = Sys.Date()),
      column(12, reactableOutput("table_tab3")),
      column(12, plotOutput("combined_plot_tab3"))
        ),
  )
}

# Define UI for Tab 4
ui_tab4 <- function() { 
  fluidPage(
    titlePanel("Todos Datos"),
    fluidRow(column=6,
             # Date range input
             dateRangeInput("date_range_input_tab3", "Elige las fechas:",
                            start = "2020-06-29", end = Sys.Date()))
             
    )
}

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Enfermedades Respiratorias Infectuosas en Guatemala"),
  
  # Theme
  theme = shinytheme("united"),
  
  # Main panel content goes here
  tabsetPanel(
    # Define the three tabs
    tabPanel("Estudio de Influenza", ui_tab1()),
    tabPanel("Estudio AGRI-CASA", ui_tab2()),
    tabPanel("Estudio NAMRU-Biofire", ui_tab3()),
    tabPanel("Estudio NAMRU-Biofire", ui_tab4())
    )
  )


# Define server logic ----
server <- function(input, output) {
  
  # Influenza -----------------------------------------------------------------------
  
  # Reactive expression to filter data based on selected disease and date range
  filtered_data <- reactive({
    
    # Filter data based on selected date range
    subset(influenza_summary, epiweek_recolec >= input$date_range_input_tab1[1] & 
             epiweek_recolec <= input$date_range_input_tab1[2])
  })
  
  # Render the plot based on filtered data
  output$disease_plot_tab1 <- renderPlot({
    filtered <- filtered_data()
    count_all_column_name <- paste0("count_all_", input$virus)
    count_pos_column_name <- paste0("count_pos_", input$virus)
    pct_pos_column_name <- paste0("pct_pos_", input$virus)
    
    virus_labels <- c("resul_inf_a_all" = "Influenza A",
                      "resul_inf_b_all" = "Influenza B",
                      "resul_inf_all" = "Influenza (todos)",
                      "resul_rsv_all" = "VSR",
                      "resul_sars_all" = "SARS-CoV-2 confirmado por PCR",
                      "resul_covid_19_all" = "SARS-CoV-2 confirmado por prueba rápida de antígenos", 
                      "resul_sars_covid_all" = "SARS-CoV-2 (confirmado por PCR o prueba rápida)",
                      "resul_virus_all" = "Todos"
                      )
    selected_virus_label <- virus_labels[[input$virus]]
    
    filtered%>%
      dplyr::mutate(epiweek_recolec_date = as.Date(epiweek_recolec),
                    count_all_column_name = ifelse(is.na(count_all_column_name), 0, count_all_column_name)
      )%>%
      ggplot(aes(x = epiweek_recolec_date)) +
      geom_bar(aes(y = .data[[count_all_column_name]], fill = "Total"), stat = "identity", color="black") +
      geom_bar(aes(y = .data[[count_pos_column_name]], fill = "Prueba Positiva"), stat = "identity", color="black") +
      scale_fill_manual(values = c("Total" = "grey", "Prueba Positiva" = "red")) +
      #geom_text_repel(
      #  aes(y = .data[[count_all_column_name]], 
      #      label = ifelse(filtered[[pct_pos_column_name]] > 0, paste0(round(filtered[[pct_pos_column_name]]), "%"), "")),
      #  box.padding = 0.5,  # Adjust this value as needed
      #  point.padding = 1,  # Adjust this value as needed
      #  segment.curvature = 0.2,  # Adjust this value as needed
      #  vjust = -0.5, 
      #  color = "black", 
      #  size = 3)+
      theme_classic() +
      labs(title = paste("Resultados de Pruebas de", selected_virus_label),
           x = "Epiweek (Semana cuando se detectó la infección por primera vez)",
           y = "Número de individuos",
           fill = "Resultado") +
      scale_y_continuous(breaks = seq(0, max(filtered[[count_all_column_name]], na.rm=TRUE), by = 1))+
      scale_x_date(labels = format_date_spanish)
    
  })
  
  # Agri-Casa Incidence-----------------------------------------------------------------------
  
  # Reactive expression to filter data based on selected disease and date range
  filtered_agri_incidence <- reactive({
    # Filter data based on selected date range
    subset(agri_casa_incidence_summary, epiweek_muestra_funsalud >= input$date_range_input_tab2[1] & 
             epiweek_muestra_funsalud <= input$date_range_input_tab2[2])
  })
  
  # Render the plot based on filtered data
  output$incidence_plot_tab2 <- renderPlot({
    
    # Load dataset
    filtered_agri <- filtered_agri_incidence()
    
    # For code
    count_pos_column_name_agri <- paste0(input$virus_agri)

    # For labels
    virus_labels_agri <- c("sars_cov2_all" = "SARS-CoV-2",
                           "influenza_all" = "Influenza",
                           "vsr_all" = "VSR",
                           "virus_all" = "Todos")
    selected_virus_label_agri <- virus_labels_agri[[input$virus_agri]]
    
    filtered_agri %>%
      dplyr::mutate(epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud)) %>%
      ggplot(aes(x = epiweek_muestra_funsalud)) +
      geom_bar(aes(y = denominator, fill = "Total"), stat = "identity", color="black") +
      geom_bar(aes(y = total_ili_count, fill = "Experimentan tos, fiebre, \n o falta de aire"), stat = "identity", color="black") +
      geom_bar(aes(y = .data[[count_pos_column_name_agri]], fill = "Primera Prueba Positiva"), stat = "identity", color="black") +
      scale_fill_manual(values = c("Total" = "grey", "Primera Prueba Positiva" = "red", "Experimentan tos, fiebre, \n o falta de aire" = "orange")) +
      theme_classic() +
      theme(
        legend.position = "top",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      labs(title = paste("Número de individuos \n por semana en la Vigilancia de Rutina \n o Vigilancia Intensa con", selected_virus_label_agri),
           x = "Epiweek (Semana cuando se detectó la infección por primera vez)",
           y = "Número de individuos",
           fill = "") +
      scale_y_continuous(breaks = seq(0, max(filtered_agri$denominator, na.rm=TRUE), by = 25)) +
      scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
  }, width = 500, height = 400)
  
  
  # Agri-Casa Symptoms-----------------------------------------------------------------------
  # Reactive expression for filtered data
  filtered_data_tab2 <- reactive({
    
    selected_symptoms <- input$symptoms
    
    # Filter data based on selected symptoms
    filtered <- agri_casa_symptom_summary %>%
      filter(!is.na(epiweek_symptoms))  # Ensure epiweek_symptoms are not NA
    
    # Combine conditions for selected symptoms
    conditions_list <- lapply(selected_symptoms, function(symptom) {
      symptom_conditions <- symptom_map[[symptom]]
      paste(symptom_conditions, collapse = " | ")
    })
    
    conditions <- paste(unlist(conditions_list), collapse = " | ")
    
    # Count number of distinct anonymized_ids where any condition is met for the selected symptoms
    counts <- filtered %>%
      group_by(epiweek_symptoms) %>%
      summarise(count = sum(rowSums(sapply(conditions, function(cond) eval(parse(text = cond))), na.rm = TRUE) > 0, na.rm = TRUE)) %>%
      mutate(count = replace(count, is.na(count), 0))  # Replace NA with 0
    
    # Merge with original data to ensure all epiweek_symptoms are retained
    all_weeks <- agri_casa_symptom_summary %>%
      distinct(epiweek_symptoms)  # Get all unique epiweek_symptoms
    
    counts <- left_join(all_weeks, counts, by = "epiweek_symptoms") %>%
      mutate(count = replace(count, is.na(count), 0))  # Replace NA with 0
    
    # Merge with denominator from agri_casa_symptom_summary
    counts_w_denom <- left_join(counts, 
                                     (agri_casa_symptom_summary %>% 
                                        dplyr::select(epiweek_symptoms, denominator)%>%
                                        dplyr::distinct(epiweek_symptoms, denominator, .keep_all = TRUE)), by = "epiweek_symptoms")
    

    return(counts_w_denom)
  })
  
  # Render plot based on filtered data
  output$symptoms_plot_tab2 <- renderPlot({
    req(input$symptoms)  # Ensure symptoms are selected
    
    agri_casa_simptomas <- filtered_data_tab2()
    
    # Plot
    agri_casa_simptomas %>%
      mutate(epiweek_symptoms = as.Date(epiweek_symptoms)) %>%
      ggplot(aes(x = epiweek_symptoms)) +
      geom_bar(aes(y = denominator, fill = "Total"), stat = "identity", color="black") +
      geom_bar(aes(y = count, fill = "Experimentan Síntomas"), stat = "identity", color="black") +
      labs(
        title = "Individuos con los síntomas especificados \n por semana en la Vigilancia de Rutina \n o Vigilancia Intensa",
        x = "Semana",
        y = "Número de individuos", 
        fill=""
      ) +
      scale_fill_manual(values = c("Total" = "grey", "Experimentan Síntomas" = "gold")) +
      theme_classic() +
      theme(
        legend.position = "top",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
  }, width = 500, height = 300)
  
  
  # Create a percent positivity graph with everything overlaying each other----------------------------------
  
  
  # Render plot based on filtered data
  output$pct_plot_tab2 <- renderPlot({
    
    # Load dataset
    filtered_agri <- filtered_agri_incidence()
    
    # Find column
    count_pos_column_name_agri <- paste0(input$virus_agri)
    
    # For labels
    virus_labels_agri <- c("sars_cov2_all" = "SARS-CoV-2",
                           "influenza_all" = "Influenza",
                           "vsr_all" = "VSR",
                           "virus_all" = "Todos")
    selected_virus_label_agri <- virus_labels_agri[[input$virus_agri]]
    
    if(is.null(input$symptoms)){
      
      # Add new columns
      filtered_agri$pct_prueba_positiva <- ifelse(filtered_agri$denominator <=0, 0,
                                                     (filtered_agri[[count_pos_column_name_agri]] / filtered_agri$denominator)*100)
      
      filtered_agri$pct_ILI_symptoms <- ifelse(filtered_agri$denominator <=0, 0,
                                                  (filtered_agri$total_ili_count / filtered_agri$denominator)*100)
      
      filtered_agri %>%
        mutate(epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud)) %>%
        ggplot(aes(x = epiweek_muestra_funsalud)) +
        geom_bar(aes(y = pct_ILI_symptoms, fill = "Experimentan fiebre, tos, \n o falta de aire"), stat = "identity", color = "black", alpha = 0.5) +
        geom_bar(aes(y = pct_prueba_positiva, fill = "Primera Prueba Positiva"), stat = "identity", color = "black", alpha = 0.5) +
        labs(
          title = paste("Porcentaje de individuos \n por semana en la Vigilancia de Rutina \n o Vigilancia Intensa con", selected_virus_label_agri),
          x = "Semana",
          y = "% de individuos", 
          fill = ""
        ) +
        scale_fill_manual(
          values = c("Experimentan fiebre, tos, \n o falta de aire" = "orange", "Primera Prueba Positiva" = "red"),
          labels = c("Experimentan fiebre, tos, \n o falta de aire" = "Experimentan fiebre, tos, \n o falta de aire",
                     "Primera Prueba Positiva" = "Primera Prueba Positiva")
        ) +
        theme_classic() +
        theme(
          legend.position = "top",
          plot.margin = margin(10, 10, 10, 10)
        ) +
        scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
    }
    
    else{
      
   
    agri_casa_simptomas_2 <- filtered_data_tab2()
    
    
  # Merge dataset
    agri_casa_pct_df <- merge(x=agri_casa_simptomas_2, y=filtered_agri, by=c("epiweek_symptoms", "denominator"),
                             by.y=c("epiweek_muestra_funsalud", "denominator"), all=TRUE)
    
    # Add new columns
    agri_casa_pct_df$pct_prueba_positiva <- ifelse(agri_casa_pct_df$denominator <=0, 0,
                                                   (agri_casa_pct_df[[count_pos_column_name_agri]] / agri_casa_pct_df$denominator)*100)
    
    agri_casa_pct_df$pct_combo_symptoms <- ifelse(agri_casa_pct_df$denominator <=0, 0,
                                                  (agri_casa_pct_df$count / agri_casa_pct_df$denominator)*100)
    
    agri_casa_pct_df$pct_ILI_symptoms <- ifelse(agri_casa_pct_df$denominator <=0, 0,
                                                (agri_casa_pct_df$total_ili_count / agri_casa_pct_df$denominator)*100)

    agri_casa_pct_df %>%
      mutate(epiweek_symptoms = as.Date(epiweek_symptoms)) %>%
      ggplot(aes(x = epiweek_symptoms)) +
      geom_bar(aes(y = pct_combo_symptoms, fill = "Experimentan Síntomas"), stat = "identity", color = "black", alpha = 0.5) +
      geom_bar(aes(y = pct_prueba_positiva, fill = "Primera Prueba Positiva"), stat = "identity", color = "black", alpha = 0.5) +
      labs(
        title = paste("Porcentaje de individuos \n por semana en la Vigilancia de Rutina \n o Vigilancia Intensa con", selected_virus_label_agri),
        x = "Semana",
        y = "% de individuos", 
        fill = ""
      ) +
      scale_fill_manual(
        values = c("Experimentan Síntomas" = "gold", "Primera Prueba Positiva" = "red"),
        labels = c("Experimentan Síntomas" = "Experimentan Síntomas",
                   "Primera Prueba Positiva" = "Primera Prueba Positiva")
      ) +
      theme_classic() +
      theme(
        legend.position = "top",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
    
  }}, width = 500, height = 300)
  
  # --------------------------------------------------------------------------
  #                             BIOFIRE
  # --------------------------------------------------------------------------
  
  # Count the number of positive and negative results for each
  # Calculate total counts of 1s for each column
  biofire_total_tested_pre <- namru_biofire_summary%>%
    group_by(epiweek_recoleccion)%>%
    dplyr::summarise(count_sangre_total = max(0,
                                              sum(result_sangre_complt==1 | result_sangre_complt==2, na.rm = TRUE)),
                     count_nasof_total = max(0,
                                             sum(result_hispd_nasof==1 | result_hispd_nasof==2, na.rm=TRUE)),
                     )
  
  # Add negative column
  namru_biofire_summary_anonymized_wneg <- namru_biofire_summary%>%
  mutate(Negativo_sangre = ifelse(result_sangre_complt==2, 1, 0), 
         Negativo_hisnaso = ifelse(result_hispd_nasof==2, 1, 0))
  
  # Function to create summary dataset for specified columns
  create_individual_summaries <- function(data, columns_to_summarize) {
    summaries <- lapply(columns_to_summarize, function(column) {
      summary <- data %>%
        group_by(epiweek_recoleccion) %>%
        summarise(count = sum(.data[[column]] == 1, na.rm = TRUE))
      names(summary)[2] <- paste("count_", column, sep="")
      return(summary)
    })
    
    # Merge summaries into one dataframe
    summary_data <- Reduce(function(x, y) full_join(x, y, by = "epiweek_recoleccion"), summaries)
    
    return(summary_data)
  }
  
  # Apply counting positive tests by week to all columns of interest
  namru_biofire_summary_counts_premerge <- create_individual_summaries(namru_biofire_summary_anonymized_wneg, columns_of_interest_biofire)
  
  # Add totals
  namru_biofire_summary_counts_pre <- merge(biofire_total_tested, namru_biofire_summary_counts_premerge, by="epiweek_recoleccion", all=TRUE)
  # If epiweek is not recorded, do not count it
  namru_biofire_summary_counts_pre <- namru_biofire_summary_counts_pre %>%
    filter(!is.na(epiweek_recoleccion))
  
  # ISSUE: Including all weeks despite sparse testing
  # Not all weeks are included, so we need to make a list of all Epiweeks and merge into dataset
  # Define start and end dates
  start_date <- as.Date("2020-06-29")
  end_date <- Sys.Date()
  # Generate sequence of dates from start to end
  all_dates <- seq(start_date, end_date, by = "1 day")
  # Filter for Mondays
  mondays <- all_dates[format(all_dates, "%A") == "Monday"]
  # Convert to format "YYYY-MM-DD"
  mondays <- format(mondays, "%Y-%m-%d")
  all_epiweeks <- tibble("epiweek_recoleccion" = mondays)
  namru_biofire_summary_counts <- merge(namru_biofire_summary_counts_pre,
                                        all_epiweeks, by="epiweek_recoleccion", all=TRUE)
  # Every time there is an NA in the dataset (because we just added a date in), replace with a 0
  namru_biofire_summary_counts[is.na(namru_biofire_summary_counts)] <- 0
  # Do same thing with all tests
  biofire_total_tested <- merge(biofire_total_tested_pre,
                                        all_epiweeks, by="epiweek_recoleccion", all=TRUE)
  # Every time there is an NA in the dataset (because we just added a date in), replace with a 0
  biofire_total_tested[is.na(biofire_total_tested)] <- 0
  
  # Get list of column names
  colnames_namru_counts_pre <- colnames(namru_biofire_summary_counts)
  colnames_namru_counts <- setdiff(colnames_namru_counts, c("epiweek_recoleccion"))
  colnames_namru_counts_sangre <- grep("^count_patogenos_positivos_sangre", 
                                       colnames_namru_counts, value = TRUE)
  colnames_namru_counts_hisnaso <- grep("^count_patogenos_positivos_hisnaso", 
                                        colnames_namru_counts, value = TRUE)
  colnames_namru_counts_wtotals <- setdiff(colnames_namru_counts, c("epiweek_recoleccion",
                                                                    "count_sangre_total",
                                                                    "count_nasof_total"))
  colnames_namru_counts_wtotals_wneg <- setdiff(colnames_namru_counts, c("epiweek_recoleccion",
                                                                    "count_sangre_total",
                                                                    "count_nasof_total",
                                                                    "count_Negativo_sangre",
                                                                    "count_Negativo_hisnaso"))
  

  # Define reactive expression to filter and transform data
  filtered_biofire_data <- reactive({
    # Filter data based on selected date range
    namru_biofire_summary_counts_filtered <- subset(namru_biofire_summary_counts,
                                                    epiweek_recoleccion >= input$date_range_input_tab3[1] &
                                                      epiweek_recoleccion <= input$date_range_input_tab3[2])
    
    # Perform data transformation as needed
    namru_long_data <- namru_biofire_summary_counts_filtered %>%
      pivot_longer(cols = colnames_namru_counts_wtotals_wneg, 
                   names_to = "pathogen_code", 
                   values_to = "count",
                   values_drop_na = TRUE) %>%
      mutate(pathogen_code = recode(pathogen_code, !!!setNames(names(pathogen_names), paste0("count_", names(pathogen_names)))),
    `Patógeno` = pathogen_names[as.character(pathogen_code)],
    `Todas pruebas de sangre` = count_sangre_total,
    `Todas pruebas naso/orofaríngeo` = count_nasof_total,
    `Tipo de Muestra` = case_when(
      grepl("sangre", pathogen_code, ignore.case = TRUE) ~ "Sangre",
      grepl("hisnaso", pathogen_code, ignore.case = TRUE) ~ "Naso/orofaríngeo",
      TRUE ~ NA_character_)) %>%
    dplyr::select(-c("count_sangre_total", "count_nasof_total"))
    
    namru_biofire_filtered_df <- namru_long_data %>%
      group_by(Patógeno)%>%
      dplyr::summarize(
        `# Personas Positivas` = sum(count, na.rm=TRUE),
        `Tipo de Muestra` = `Tipo de Muestra`)%>%
      slice(1)
    
    biofire_total_counts_sangre <- sum(namru_biofire_summary_counts_filtered$count_sangre_total,
                                       na.rm=TRUE)
    biofire_total_counts_nasof <- sum(namru_biofire_summary_counts_filtered$count_nasof_total,
                                       na.rm=TRUE)
    
    namru_biofire_filtered_df$`Todas Personas Probadas` <- ifelse(
      namru_biofire_filtered_df$`Tipo de Muestra`=="Sangre",
      biofire_total_counts_sangre,
      ifelse(
        namru_biofire_filtered_df$`Tipo de Muestra`=="Naso/orofaríngeo",
        biofire_total_counts_nasof, NA))
    
  namru_biofire_filtered_df
  })

# Render the table using reactable
output$table_tab3 <- renderReactable({
  reactable(filtered_biofire_data())
})
  
  # Create graphs ----------------------------------------------------------------

# Prepare Sample Count Data -----------------------------------------
# Define reactive expression to filter and transform data
filtered_biofire_plot_df <- reactive({
  # Filter data based on selected date range
  namru_biofire_summary_counts_filtered <- subset(namru_biofire_summary_counts,
                                                  epiweek_recoleccion >= input$date_range_input_tab3[1] &
                                                    epiweek_recoleccion <= input$date_range_input_tab3[2])
  
  # Perform data transformation as needed
  namru_long_data <- namru_biofire_summary_counts_filtered %>%
    pivot_longer(cols = c(colnames_namru_counts_wtotals, count_Negativo_sangre, count_Negativo_hisnaso), 
                 names_to = "pathogen_code", 
                 values_to = "count",
                 values_drop_na = TRUE) %>%
    mutate(pathogen_code = recode(pathogen_code, !!!setNames(names(pathogen_names), paste0("count_", names(pathogen_names)))),
           `Patógeno` = pathogen_names[as.character(pathogen_code)],
           `Todas pruebas de sangre` = count_sangre_total,
           `Todas pruebas naso/orofaríngeo` = count_nasof_total,
           `Tipo de Muestra` = case_when(
             grepl("sangre", pathogen_code, ignore.case = TRUE) ~ "Sangre",
             grepl("hisnaso", pathogen_code, ignore.case = TRUE) ~ "Naso/orofaríngeo",
             TRUE ~ NA_character_)) %>%
    dplyr::select(-c("count_sangre_total", "count_nasof_total"))
  
  namru_long_data
})

# Create overall count ----------------------------
filtered_biofire_totals_plot_df <- reactive({
  
  # Filter data based on selected date range
  biofire_total_tested_filtered <- subset(biofire_total_tested,
                                          epiweek_recoleccion >= input$date_range_input_tab3[1] &
                                            epiweek_recoleccion <= input$date_range_input_tab3[2])
  
  biofire_total_tested_filtered
})

# Create plot -------------------------------------
# Render plot based on filtered data
output$combined_plot_tab3 <- renderPlot({
  
  # Plot total number of tests
  # Get filtered totals data
  biofire_total_tested_filtered <- filtered_biofire_totals_plot_df()
  
  # Plot for total tests
  total_biofire_test_plot <- biofire_total_tested_filtered %>%
    mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion))%>%
    ggplot() +
    geom_bar(aes(x = epiweek_recoleccion, y = count_nasof_total),
             stat = "identity",  fill = "royalblue", alpha=0.6, color="black", size = 0.3) +
    geom_bar(aes(x = epiweek_recoleccion, y = count_sangre_total),
             fill = "red", stat = "identity", alpha=0.6, color="black", size = 0.3) +
    theme_classic() +
    labs(
      title = "\n
      \n
      \n
      Número de personas probadas por semana",
      x = "",
      y = ""
    ) +
    scale_y_continuous(breaks = seq(0, max(
                                    max(biofire_total_tested_filtered$count_nasof_total, na.rm = TRUE),
                                    max(biofire_total_tested_filtered$count_sangre_total, na.rm = TRUE)
                                    ),
                                    by = 1)) +
    theme(axis.line = element_blank(), 
          plot.title = element_text(size = 12, face = "bold", hjust=0.5),  # Adjust title size and style
          axis.title.y = element_text(angle = 0, vjust = 0.5,face = "bold", margin = margin(r = -30)),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12, angle = 0, hjust = 1)  # Adjusted y-axis text size
          #axis.text.x = element_blank(),
          #axis.ticks.x = element_blank()
          )+
    scale_x_date(
      labels = format_date_spanish,
      limits = as.Date(c(input$date_range_input_tab3[1], input$date_range_input_tab3[2])),
      expand = c(0, 0)  # Remove extra space at ends
    )  
  
  # Get filtered data for both Sangre and Naso/orofaríngeo
  filtered_data <- filtered_biofire_plot_df() %>%
    mutate(epiweek_recoleccion = as.Date(epiweek_recoleccion))
  
  # Define colors for each Tipo de Muestra
  color_palette <- c("Naso/orofaríngeo (7)" = "#1e214e",
                     "Naso/orofaríngeo (5)" = "#271c8a",
                     "Naso/orofaríngeo (4)" = "#3706c0",
                     "Naso/orofaríngeo (3)" = "#5339d1",
                     "Naso/orofaríngeo (2)" = "#7599eb",
                     "Naso/orofaríngeo (1)" = "lightblue",
                     "Naso/orofaríngeo (0)" = "white",
                     "Sangre (0)" = "white",
                     "Sangre (1)" = "#fe8181",
                     "Sangre (2)" = "#fe5757",
                     "Sangre (3)" = "#fe2e2e",
                     "Sangre (4)" = "#cb2424",
                     "Sangre (5)" = "#b62020")
  
  # Convert color_palette to a dataframe
  color_palette_df <- data.frame(
    category = names(color_palette),
    color = as.character(color_palette),
    stringsAsFactors = FALSE
  )
  
  filtered_data$forColor <-
    factor(paste0(filtered_data$`Tipo de Muestra`, " (", filtered_data$count , ")"))

    # Create a single ggplot object
    tile_plot <- filtered_data %>%
      arrange(`Tipo de Muestra`) %>%
      ggplot(aes(x = epiweek_recoleccion, y = forcats::fct_relevel(`Patógeno`, "Negativo"))) +
      geom_tile(aes(fill = forColor), color='black') +
      facet_wrap(~ `Tipo de Muestra`, ncol = 1, scales = "free_y") +  # Facet by Tipo de Muestra
      labs(
        title="Número de personas con un dado resultado",
        x = "Semana",
        y = "",
        fill= ""
      ) +
      theme_classic() +
      scale_x_date(
        labels = format_date_spanish,
        limits = as.Date(c(input$date_range_input_tab3[1], input$date_range_input_tab3[2])),
        expand = c(0, 0)  # Remove extra space at ends
      ) +
      #scale_fill_gradient(low = "#FFFFFF", high = "#FF0000")+
      theme(
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold", hjust=0.5),  # Adjust title size and style
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(fill = 'white'),
        axis.line = element_blank(), 
        axis.text.x = element_text(size = 12),
        plot.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, face = "bold", margin = margin(r = -10)),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 1)  # Adjusted y-axis text size and color
      )+
      scale_fill_manual(values = color_palette_df$color, breaks = color_palette_df$category)
  
    # Add the total count to the top of the chart
    combined_plot <- total_biofire_test_plot / tile_plot + plot_layout(heights = c(1, 3), widths = c(1, 1))
    
    # Display the combined plot with specified width and height
    combined_plot
}, height = 900, width = 875) 


}

shinyApp(ui, server)

# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc