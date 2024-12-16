# Load required libraries
library(shiny)
library(DT)          # For rendering interactive tables
library(ggplot2)      # For creating 2D plots
library(plotly)       # For creating 3D scatterplots
library(dplyr)        # For data manipulation
library(rmarkdown)
library(knitr)
library(jsonlite)
library(htmltools)
library(rsconnect)


# Ensure all dependencies are explicitly included
if (FALSE) {
  library(shiny)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(rmarkdown)
  library(knitr)
  library(jsonlite)
  library(htmltools)
  library(rsconnect)
}

# Define the User Interface (UI)
ui <- fluidPage(
  # App title
  titlePanel("A Wine Data Viewer and Analysis"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Add instructional text
      helpText("Please add wine Train Set.csv"),
      # File input to upload CSV file
      fileInput("file", "Choose a CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Checkbox to indicate if the file has a header
      checkboxInput("header", "Header", TRUE)
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        # Tab for Data Table
        tabPanel("Data Table", DTOutput("table")),
        
        # Tab for Quality Analysis
        tabPanel("Quality", 
                 selectInput("y_var_quality", "Select Y Variable:", choices = NULL), # Dropdown for y-variable
                 plotOutput("qualityPlot")), # Boxplot output for Quality
        
        # Tab for Type Analysis
        tabPanel("Type", 
                 selectInput("y_var_type", "Select Y Variable:", choices = NULL), # Dropdown for y-variable
                 plotOutput("typePlot"),      # Boxplot output for Type
                 plotOutput("typeSummaryPlot") # Summary bar chart for Quality by Type
        ),
        
        # Tab for Location Analysis
        tabPanel("Location", 
                 selectInput("y_var_location", "Select Y Variable:", choices = NULL), # Dropdown for y-variable
                 plotOutput("locationPlot"),  # Boxplot output for Location
                 plotOutput("locationSummaryPlot") # Summary bar chart for Quality by Location
        ),
        
        # Tab for Other Analysis
        tabPanel("Other", 
                 h4("3D Scatterplot: Type of Wine by Density, Total SO2, and Volatile Acidity"),
                 plotlyOutput("scatter3d_type"),
                 
                 
                 h4("3D Scatterplot: Location of Wine by Alcohol, Density, and Volatile Acidity"),
                 plotlyOutput("scatter3d_location"),
                 
                 h4("3D Scatterplot: Wine Quality by Volatile Acidity, Density, and Alcohol"),
                 plotlyOutput("scatter3d_quality"))
      )
    )
  )
)

# Define the Server logic
server <- function(input, output, session) {
  
  # Reactive expression to read and preprocess the uploaded file
  data <- reactive({
    # Ensure a file is uploaded
    req(input$file)
    
    # Read the uploaded CSV file
    df <- read.csv(input$file$datapath, header = input$header)
    
    # Preprocess the 'location' column to fix misspellings
    if ("location" %in% names(df)) {
      df$location <- gsub("Califormia", "California", df$location, ignore.case = TRUE)
    }
    
    # Convert 'quality', 'type', and 'location' to factors if they exist
    if ("quality" %in% names(df)) {
      df$quality <- as.factor(df$quality)
    }
    if ("type" %in% names(df)) {
      df$type <- as.factor(df$type)
    }
    if ("location" %in% names(df)) {
      df$location <- as.factor(df$location)
    }
    
    return(df)
  })
  
  # Populate the dropdown menus for all tabs
  observeEvent(data(), {
    column_choices <- names(data())
    updateSelectInput(session, "y_var_quality", choices = column_choices)
    updateSelectInput(session, "y_var_type", choices = column_choices)
    updateSelectInput(session, "y_var_location", choices = column_choices)
  })
  
  # Render the data table in the "Data Table" tab
  output$table <- renderDT({
    data()  # Display the dataset as an interactive table
  }, options = list(pageLength = 10, autoWidth = TRUE))
  
  # Render the boxplot in the "Quality" tab
  output$qualityPlot <- renderPlot({
    req(data(), input$y_var_quality)
    data() %>% 
      ggplot(aes(x = quality, y = .data[[input$y_var_quality]], color = quality)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", input$y_var_quality, "by Quality"),
           x = "Quality",
           y = input$y_var_quality) +
      theme_minimal()
  })
  
  # Render the boxplot in the "Type" tab
  output$typePlot <- renderPlot({
    req(data(), input$y_var_type)
    data() %>% 
      ggplot(aes(x = type, y = .data[[input$y_var_type]], fill = type)) +
      geom_boxplot() +
      ggtitle(paste("Boxplot of", input$y_var_type, "by Type")) +
      xlab("Type") +
      ylab(input$y_var_type) +
      theme_bw()
  })
  
  # Render the summary bar chart in the "Type" tab
  output$typeSummaryPlot <- renderPlot({
    req(data())
    wines_summary <- data() %>%
      group_by(quality, type) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = count / sum(count)) # Optional: Calculate proportions
    
    wines_summary %>%
      ggplot(aes(x = quality, y = count, fill = type)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(aes(label = count), 
                position = position_fill(vjust = 0.5), # Position the labels inside the bars
                color = "white") +
      ggtitle("A Barchart of Quality by Type") +
      xlab("Quality") +
      ylab("Proportion") +
      theme_bw()
  })
  
  # Render the boxplot in the "Location" tab
  output$locationPlot <- renderPlot({
    req(data(), input$y_var_location)
    data() %>% 
      ggplot(aes(x = location, y = .data[[input$y_var_location]], fill = location)) +
      geom_boxplot() +
      ggtitle(paste("Boxplot of", input$y_var_location, "by Location")) +
      xlab("Location") +
      ylab(input$y_var_location) +
      theme_bw()
  })
  
  # Render the summary bar chart in the "Location" tab
  output$locationSummaryPlot <- renderPlot({
    req(data())
    wines_summary <- data() %>%
      group_by(quality, location) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = count / sum(count)) # Optional: Calculate proportions
    
    wines_summary %>%
      ggplot(aes(x = quality, y = count, fill = location)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(aes(label = count), 
                position = position_fill(vjust = 0.5), # Position the labels inside the bars
                color = "white") +
      ggtitle("A Barchart of Quality by Location") +
      xlab("Quality") +
      ylab("Proportion") +
      theme_bw()
  })
  
  # Render the 3D scatterplot for Type
  output$scatter3d_type <- renderPlotly({
    req(data())
    plot_ly(data = data(),
            x = ~density,
            y = ~total.sulfur.dioxide,
            z = ~volatile.acidity,
            type = "scatter3d",
            mode = "markers",
            color = ~type,
            colors = c("#C8102E", "#0033A0"),
            marker = list(size = 4, opacity = 0.7)) %>%
      layout(
        scene = list(xaxis = list(title = "Density"),
                     yaxis = list(title = "Total SO2"),
                     zaxis = list(title = "Volatile Acidity")),
        legend = list(title = list(text = "Legend")))
  })
  
  # Render the 3D scatterplot for Location
  output$scatter3d_location <- renderPlotly({
    req(data())
    plot_ly(data = data(), 
            x = ~alcohol, 
            y = ~density,
            z = ~volatile.acidity, 
            type = "scatter3d", 
            mode = "markers",
            color = ~location, 
            colors = c("#0033A0", "#C8102E"),
            marker = list(size = 4, opacity = 0.5)) %>%
      layout(
        scene = list(xaxis = list(title = "Alcohol"),
                     yaxis = list(title = "Density"),
                     zaxis = list(title = "Volatile Acidity")),
        legend = list(title = list(text = "Legend")))
  })
  
  # Render the 3D scatterplot for Quality
  output$scatter3d_quality <- renderPlotly({
    req(data())
    plot_ly(data = data(), 
            x = ~volatile.acidity, 
            y = ~density,
            z = ~alcohol, 
            type = "scatter3d", 
            mode = "markers",
            color = ~as.factor(quality), 
            colors = c("#FF0000",  "#FF5733", "#FFFF33","#33FF57","#33FFF6","#0099ff", "#9900ff"),
            marker = list(size = 4, opacity = 0.8)) %>%
      layout(
        scene = list(xaxis = list(title = "Volatile Acidity"),
                     yaxis = list(title = "Density"),
                     zaxis = list(title = "Alcohol")),
        legend = list(title = list(text = "Legend")))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
