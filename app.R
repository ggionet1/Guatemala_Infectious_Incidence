library(shiny)
library(bslib)

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
  fluidRow(
    column(12,
           # Customize the UI for Tab 2 here
           tags$h2("Custom UI for Tab 2"),
           # Add UI elements specific to Tab 2
           sliderInput("slider_input_tab2", "Slider Input", min = 0, max = 100, value = 50),
           selectInput("select_input_tab2", "Select Input", choices = c("Option 1", "Option 2", "Option 3"))
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
  
  # Reactive expression to filter data based on selected disease and date range
  filtered_data <- reactive({
    # Load dataframe
    data <- read.csv("https://raw.githubusercontent.com/ggionet1/Guatemala_Infectious_Incidence/main/influenza_summary.csv")

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
}

shinyApp(ui, server)

# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc

