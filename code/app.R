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
                          "VSR" = "resul_rsv_all",
                          "SARS-CoV-2 confirmado por PCR" = "resul_sars_all",
                          "SARS-CoV-2 confirmado por prueba rápida de antígenos" = "resul_covid_19_all",
                          "SARS-CoV-2 (confirmado por PCR o prueba rápida)" = "resul_sars_covid_all"))
    ),
    column(6,
           # Date range input
           dateRangeInput("date_range_input_tab1", "Select Date Range:",
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
    sidebarLayout(
      sidebarPanel(
        selectInput("tab3_time_filter", "Ver cuantas enfermedades diferentes fueron detectadas:",
                    choices = c("En el mes pasado" = "month",
                                "En los 6 meses pasados" = "6months",
                                "En 1 año pasado" = "year",
                                "En la historia de nuestro estudio" = "all"),
                    selected = "all")
      ),
      mainPanel(
        reactableOutput("table_tab3")
      )
    )
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
    tabPanel("Estudio NAMRU-Biofire", ui_tab3())
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
                      "resul_rsv_all" = "VSR",
                      "resul_sars_all" = "SARS-CoV-2 confirmado por PCR",
                      "resul_covid_19_all" = "SARS-CoV-2 confirmado por prueba rápida de antígenos", 
                      "resul_sars_covid_all" = "SARS-CoV-2 (confirmado por PCR o prueba rápida)" )
    selected_virus_label <- virus_labels[[input$virus]]
    
    filtered%>%
      dplyr::mutate(epiweek_recolec_date = as.Date(epiweek_recolec),
                    count_all_column_name = ifelse(is.na(count_all_column_name), 0, count_all_column_name)
      )%>%
      ggplot(aes(x = epiweek_recolec_date)) +
      geom_bar(aes(y = .data[[count_all_column_name]], fill = "Total"), stat = "identity") +
      geom_bar(aes(y = .data[[count_pos_column_name]], fill = "Prueba Positiva"), stat = "identity") +
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
  

  # Biofire--------------------------------------------------------------------------
  
  filtered_biofire_data <- reactive({
    time_period <- switch(input$tab3_time_filter,
                          month = 30,
                          `6months` = 180,
                          year = 365,
                          all = 99999)
    
    cutoff_date <- Sys.Date() - time_period
    
    namru_biofire_filtered <- namru_biofire_summary %>%
      filter(Semana.más.reciente.con.resultado.positivo >= cutoff_date)
    
    as.data.frame(namru_biofire_filtered)%>%
      select(Patógeno, `# Personas` = Numero.de.personas,
             `Tipo de Muestra` = Tipo.de.Muestra,
             `Semana más reciente \n con resultado \n positivo` = Semana.más.reciente.con.resultado.positivo)
  })
  
  output$table_tab3 <- renderReactable({
    reactable(filtered_biofire_data())
  })
  
  
}

shinyApp(ui, server)

# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc