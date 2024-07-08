#library(shiny) #keep commented for app to be deployed on R Shiny
library(bslib)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(shinythemes)

# Load dataframes ---------
influenza_summary <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/docs/influenza_summary_updated.csv")
agri_casa_summary <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/docs/agri_casa_summary_updated.csv")

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
           radioButtons("virus", "Elige el virus:",
                        c("Influenza A" = "resul_inf_a_all",
                          "Influenza B" = "resul_inf_b_all",
                          "RSV" = "resul_rsv_all",
                          "SARS-CoV-2 confirmado por PCR" = "resul_sars_all",
                          "SARS-CoV-2 confirmado por prueba rápida de antígenos" = "resul_covid_19_all",
                          "SARS-CoV-2 (confirmado por PCR o prueba rápida)" = "resul_sars_covid_all")),
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
    titlePanel("Síntomas nuevas por semana"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "columns_selected",
          "Elige sintomas:",
          choices = c("Tos" = "sintomas_nuevos_nuevos___1",
                      "Dolor de garganta" = "sintomas_nuevos_nuevos___2",
                      "Dolor de cabeza" = "sintomas_nuevos_nuevos___3",
                      "Congestion nasal" = "sintomas_nuevos_nuevos___4",
                      "Fiebre" = "sintomas_nuevos_nuevos___5",
                      "Dolor de cuerpo/músculos" = "sintomas_nuevos_nuevos___6",
                      "Fatiga" = "sintomas_nuevos_nuevos___7",
                      "Vómitos" = "sintomas_nuevos_nuevos___8",
                      "Diarrea" = "sintomas_nuevos_nuevos___9",
                      "Dificultad para respirar" = "sintomas_nuevos_nuevos___10",
                      "Pérdida de olfato o del gusto" = "sintomas_nuevos_nuevos___11",
                      "Nausea" = "sintomas_nuevos_nuevos___12",
                      "Sibilancias" = "sintomas_nuevos_nuevos___13",
                      "Mala alimentación" = "sintomas_nuevos_nuevos___14",
                      "Letargo" = "sintomas_nuevos_nuevos___15"),
          selected = NULL,
          multiple = TRUE
        ),
        dateRangeInput(
          "date_range_input_tab2",
          "Filtra período del tiempo:",
          start = "2023-10-02",
          end = Sys.Date()
        )
      ),
      mainPanel(
        plotOutput("disease_plot_tab2")
      )
    )
  )
}

# Define UI for Tab 3
ui_tab3 <- function() {
  fluidRow(
    column(12,
           # Customize the UI for Tab 3 here
           tags$h2("Custom UI for Tab 3"),
           # Add UI elements specific to Tab 3
           dateInput("date_input_tab3", "Date Input", value = Sys.Date()),
           actionButton("action_button_tab3", "Action Button")
    )
  )
}

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Infectious Disease Incidence in Guatemala"),
  
  # Theme
  theme = shinytheme("united"),    

  # Main panel content goes here
  tabsetPanel(
    # Define the three tabs
    tabPanel("Estudio de Influenza", ui_tab1()),
    tabPanel("Estudio de Agri-Casa", ui_tab2()),
    tabPanel("Estudio de NAMRU-Biofire", ui_tab3())
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
                      "resul_rsv_all" = "RSV",
                      "resul_sars_all" = "SARS-CoV-2 confirmado por PCR",
                      "resul_covid_19_all" = "SARS-CoV-2 confirmado por prueba rápida de antígenos", 
                      "resul_sars_covid_all" = "SARS-CoV-2 (confirmado por PCR o prueba rápida)" )
    selected_virus_label <- virus_labels[[input$virus]]
    
    # Plot
    filtered%>%
      dplyr::mutate(epiweek_recolec_date = as.Date(epiweek_recolec))%>%
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
      scale_y_continuous(breaks = seq(0, max(filtered[[count_all_column_name]]), by = 1))+
      scale_x_date(labels = format_date_spanish)
    
  })
  
  # Agri-Casa -----------------------------------------------------------------------

  # Reactive expression for Agri-Casa data filtering
  filtered_data_tab2 <- reactive({
    agri_casa_summary %>%
      dplyr::filter(epiweek_v_rutina >= input$date_range_input_tab2[1] & epiweek_v_rutina <= input$date_range_input_tab2[2]) %>%
      dplyr::filter(realizado_vig_rut == 1 & sintoma_nuevo == 1) %>%
      dplyr::filter(!is.na(epiweek_v_rutina))
  })
  
  output$disease_plot_tab2 <- renderPlot({
    req(input$columns_selected)  # Ensure columns are selected
    
    agri_casa_simptomas <- filtered_data_tab2() %>%
      rowwise() %>%
      dplyr::mutate(has_one = any(c_across(all_of(input$columns_selected)) == 1)) %>%
      ungroup() %>%
      group_by(epiweek_v_rutina) %>%
      dplyr::summarise(
        total_individuos = n(),
        individuos_con_simptomas = sum(has_one),
        percentaje_simptomas = (individuos_con_simptomas / total_individuos) * 100
      )
    
    agri_casa_simptomas%>%
      dplyr::mutate(epiweek_v_rutina_date = as.Date(epiweek_v_rutina))%>%
      ggplot(aes(x = epiweek_v_rutina_date, y = individuos_con_simptomas)) +
      geom_bar(stat="identity") +
      labs(
        title = "Individuos con los síntomas especificados \n por semana durante Vigilancia Rutina",
        x = "Semana",
        y = "Número de individuos experimentando estos síntomas"
      ) +
      theme_minimal()+
      scale_x_date(labels = format_date_spanish)
  })
}

shinyApp(ui, server)

# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc