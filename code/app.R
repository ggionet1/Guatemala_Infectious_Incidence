#library(shiny) #keep commented for app to be deployed on R Shiny
library(bslib)
library(ggplot2)
library(ggrepel)

# Define UI for random distribution app ----
# Sidebar layout with input and output definitions ----

# Define UI for Tab 1
ui_tab1 <- function() {
  fluidRow(
    column(6,
           # Dropdown menu for selecting disease
           radioButtons("virus", "Virus Incidence:",
                        c("Influenza A" = "resul_inf_a_all",
                          "Influenza B" = "resul_inf_b_all",
                          "RSV" = "resul_rsv_all",
                          "PCR-Confirmed SARS-CoV-2" = "resul_sars_all",
                          "Rapid Antigen Test-Confirmed SARS-CoV-2" = "resul_covid_19_all")),
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
          start = min(agri_casa_summary$epiweek_v_rutina, na.rm = TRUE),
          end = max(agri_casa_summary$epiweek_v_rutina, na.rm = TRUE)
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
  
  # Main panel content goes here
  tabsetPanel(
    # Define the three tabs
    tabPanel("Influenza Study", ui_tab1()),
    tabPanel("Agri-Casa Study", ui_tab2()),
    tabPanel("NAMRU-Biofire Study", ui_tab3())
  )
)


# Define server logic ----
server <- function(input, output) {
  
# Influenza -----------------------------------------------------------------------

    # Reactive expression to filter data based on selected disease and date range
  filtered_data <- reactive({
    
    # Load dataframe
    data <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/docs/influenza_summary.csv")
    
    # Filter data based on selected date range
    subset(data, epiweek_recolec >= input$date_range_input_tab1[1] & 
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
                      "resul_sars_all" = "PCR-Confirmed SARS-CoV-2",
                      "resul_covid_19_all" = "Rapid Antigen Test-Confirmed SARS-CoV-2")
    selected_virus_label <- virus_labels[[input$virus]]
    
    ggplot(filtered, aes(x = epiweek_recolec)) +
      geom_bar(aes(y = .data[[count_all_column_name]], fill = "Total"), stat = "identity") +
      geom_bar(aes(y = .data[[count_pos_column_name]], fill = "Positive"), stat = "identity") +
      scale_fill_manual(values = c("Total" = "grey", "Positive" = "red")) +
      geom_text_repel(aes(y = .data[[count_pos_column_name]], 
                          label = ifelse(filtered[[pct_pos_column_name]] > 0, paste0(round(filtered[[pct_pos_column_name]]), "%"), "")),
                      vjust = -0.5, color = "black", size = 3) +
      theme_classic() +
      labs(title = paste("Counts of", selected_virus_label, "over Time"),
           x = "Epiweek",
           y = "Number of Tests",
           fill = "Test Positivity") +
      scale_y_continuous(breaks = seq(0, max(filtered[[count_all_column_name]]), by = 1))+
      theme(axis.text.x = element_blank())
    
  })
  
# Agri-Casa -----------------------------------------------------------------------

  # Reactive expression for Agri-Casa data filtering
  filtered_data_tab2 <- reactive({
      
    # Load dataframe
    data <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/docs/agri_casa_summary.csv")
      
      # Filter dataframe
    agri_casa_summary %>%
      filter(epiweek_v_rutina >= input$date_range_input_tab2[1] & epiweek_v_rutina <= input$date_range_input_tab2[2]) %>%
      filter(realizado_vig_rut == 1 & sintoma_nuevo == 1) %>%
      filter(!is.na(epiweek_v_rutina))
  })
  
  output$disease_plot_tab2 <- renderPlot({
    req(input$columns_selected)  # Ensure columns are selected
    
    agri_casa_simptomas <- filtered_data_tab2() %>%
      rowwise() %>%
      mutate(has_one = any(c_across(all_of(input$columns_selected)) == 1)) %>%
      ungroup() %>%
      group_by(epiweek_v_rutina) %>%
      summarise(
        total_individuos = n(),
        individuos_con_simptomas = sum(has_one),
        percentaje_simptomas = (individuos_con_simptomas / total_individuos) * 100
      )
    
    ggplot(agri_casa_simptomas, aes(x = epiweek_v_rutina, y = individuos_con_simptomas, group = 1)) +
      geom_line() +
      labs(
        title = "Individuos con los síntomas especificados \n por semana durante Vigilancia Rutina",
        x = "Semana",
        y = "Individuos experimentando estos síntomas"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc
