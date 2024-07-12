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
        selectInput(
          "columns_selected",
          "Elige sintomas:",
          choices = c("Tos" = "tos_count",
                      "Fiebre o Sensación de Fiebre" = "fiebre_count",
                      "Dificultad de Respirar" = "falta_aire_count",
                      "Dolor de garganta" = "garganta_count",
                      "Dolor de cabeza" = "cabeza_count",
                      "Congestión nasal" = "congest_nasal_count",
                      "Dolor de cuerpo" = "dlr_cuerp_count",
                      "Fatiga" = "fatiga_count",
                      # "Dolor de cuello" = "cuello_count",
                      # "Interrupción del sueño" = "interrup_sue_count",
                      "Silbancias" = "silbilancias_count",
                      # "Perdida de apetito" = "perd_apet_count",
                      "Nausea" = "nausea_count",
                      "Diarrea" = "diarrea_count",
                      "Vómito" = "vomito_count",
                      "Perdida del olfato o del gusto" = "perd_olf_gust_count",
                      "Disminución de audición o equilibrio" = "perd_aud_bal_count",
                      "Mala alimentación" = "mala_alim_count",
                      "Létargo" = "letargo_alim_count"),
          selected = NULL,
          multiple = TRUE
        ),
      ),
      mainPanel(
        fluidRow(
          column(12, plotOutput("incidence_plot_tab2")),  # Existing plot for symptoms
          column(12, plotOutput("symptoms_plot_tab2"))  # New plot similar to Tab 1
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
    filtered_agri <- filtered_agri_incidence()
    count_pos_column_name_agri <- paste0(input$virus_agri)

    virus_labels_agri <- c("sars_cov2_all" = "SARS-CoV-2",
                           "influenza_all" = "Influenza",
                           "vsr_all" = "VSR",
                           "virus_all" = "Todos")
    selected_virus_label_agri <- virus_labels_agri[[input$virus_agri]]
    
    filtered_agri %>%
      dplyr::mutate(epiweek_muestra_funsalud = as.Date(epiweek_muestra_funsalud)) %>%
      ggplot(aes(x = epiweek_muestra_funsalud)) +
      geom_bar(aes(y = denominator, fill = "Total"), stat = "identity") +
      geom_bar(aes(y = total_ili_count, fill = "Sintomas detectados de tos, \n fiebre, \n o falta de aire"), stat = "identity") +
      geom_bar(aes(y = .data[[count_pos_column_name_agri]], fill = "Primera Prueba Positiva"), stat = "identity") +
      scale_fill_manual(values = c("Total" = "grey", "Prueba Positiva" = "red", "Sintomas detectados de tos, \n fiebre, \n o falta de aire" = "orange")) +
      theme_classic() +
      theme(
        legend.position = "top",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      labs(title = paste("Resultados de Pruebas de", selected_virus_label_agri),
           x = "Epiweek (Semana cuando se detectó la infección por primera vez)",
           y = "Número de individuos",
           fill = "Resultado") +
      scale_y_continuous(breaks = seq(0, max(filtered_agri$denominator, na.rm=TRUE), by = 25)) +
      scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
  }, width = 500, height = 400)
  
  
  # Agri-Casa Symptoms-----------------------------------------------------------------------
  
  
  
  # Mapping of symptoms to their corresponding columns
  symptom_map <- list(
    "tos_count" = c("tos_visit_ints >= 2", "tos_flm_visit_ints >= 2", "tos_vig_rut == 1"),
    "fiebre_count" = c("sensa_fiebre_visit_ints >= 2", "fiebre_vig_rut == 1"),
    "falta_aire_count" = c("falta_aire_visit_ints >= 2", "dif_resp_vig_rut == 1"),
    "garganta_count" = c("gargt_irrit_visit_ints >= 2", "dol_gargan_vig_rut == 1"),
    "cabeza_count" = c("dlr_cabeza_visit_ints >= 2", "dol_cabeza_vig_rut == 1"),
    "congest_nasal_count" = c("congest_nasal_visit_ints >= 2", "cong_nasal_vig_rut == 1"),
    "dlr_cuerp_count" = c("dlr_cuerp_dgnl_visit_ints >= 2", "dol_cuerp_musc_vig_rut == 1"),
    "fatiga_count" = c("fatiga_visit_ints >= 2", "fatica_vig_rut == 1"),
    "silbilancias_count" = c("silbd_resp_visit_ints >= 2", "sibilancias_vig_rut == 1"),
    "nausea_count" = c("nausea_visit_ints >= 2", "nausea_vig_rut == 1"),
    "diarrea_count" = c("diarrea_visit_ints >= 2", "diarrea_vig_rut == 1"),
    "vomito_count" = c("vomito_visit_ints >= 2", "vomitos_vig_rut == 1"),
    "perd_olf_gust_count" = c("dism_gust_visit_ints >= 2", "dism_olf_visit_ints >= 2", "perd_olf_gust_vig_rut == 1"),
    "perd_aud_bal_count" = c("dism_aud_visit_ints >= 2", "dism_bal_visit_ints >= 2"),
    "mala_alim_count" = c("mala_alim_visit_ints >= 2", "mala_alim_vig_rut == 1"),
    "letargo_alim_count" = c("letargo_visit_ints >= 2", "letargo_vig_rut == 1")
  )
  
  # Reactive expression for Agri-Casa data filtering
  
  filtered_data_tab2 <- reactive({
    
    agri_casa_symptom_summary %>%
      filter(epiweek_symptoms >= input$date_range_input_tab2[1] & epiweek_symptoms <= input$date_range_input_tab2[2]) %>%
      filter(!is.na(epiweek_symptoms))%>%
      group_by(epiweek_symptoms) %>%
      summarise(count = n_distinct(anonymized_id[rowSums(sapply(input$symptoms, function(symptom) {
        eval(parse(text = paste(symptom_map[[symptom]], collapse = " | ")))
      })) > 0]),
      epiweek_denominator = epiweek_denominator)
  })
  
  output$symptoms_plot_tab2 <- renderPlot({
    req(input$columns_selected)  # Ensure columns are selected
    
    agri_casa_simptomas <- filtered_data_tab2()
    
    agri_casa_simptomas%>%
      dplyr::mutate(epiweek_denominator = as.Date(epiweek_denominator))%>%
      ggplot(aes(x = epiweek_muestra_funsalud)) +
      geom_bar(aes(y = count, fill = "Experimentan Síntomas"), stat = "identity") +
      geom_bar(aes(y = epiweek_denominator, fill = "Total"), stat = "identity") +
        labs(
        title = "Individuos con los síntomas especificados \n por semana en la Vigilancia de Rutina \n o Vigilancia Intensa",
        x = "Semana",
        y = "Número de individuos"
      ) +
      scale_fill_manual(values = c("Total" = "grey", "Experimentan Síntomas" = "gold")) +
      theme_classic()+
      scale_x_date(labels = format_date_spanish, limits = as.Date(c(input$date_range_input_tab2[1], input$date_range_input_tab2[2])))
  }, width = 500, height = 300)

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