# app.R - Healthcare Data Dashboard
# Author: Senior Full-Stack Developer (15+ years in Healthcare)
# Description: A professional Shiny dashboard for uploading, viewing, summarizing, and plotting a single dataset.
# Features: Dynamic UI, error handling, downloads, and healthcare-themed styling.

#install.packages("colourpicker")

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shinyjs)
library(colourpicker)  # For potential color customization, though we use fixed professional palettes

# Define professional color palettes for plots (healthcare-inspired: blues for calm, greens for growth, etc.)
plot_colors <- list(
  scatter = c("#1E90FF", "#32CD32"),  # Blue and green for scatter
  histogram = "#4682B4",  # Steel blue for histogram
  bar = "#228B22",  # Forest green for bar
  line = c("#FF6347", "#20B2AA")  # Tomato and light sea green for line
)

# UI Definition
ui <- dashboardPage(
  skin = "blue",  # Base skin for consistency
  dashboardHeader(title = "Project", titleWidth = 150),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Dataset", tabName = "view", icon = icon("table")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-bar")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    useShinyjs(),  # For dynamic UI elements
    tags$head(
      tags$style(HTML("
      
        .main-header {
           min-height: 70px;
           width: 150px;
        }
        .main-sidebar{
           width: 150px;
        }
        .content-wrapper,
        .main-footer{
        margin-left: 150px;
        }
        .main-header.navbar{
        margin-left:150px;
        }
        .skin-blue .main-header .logo { 
           background-color: #36454F; 
           height: 80px;
           line-height: 80px;
           font-size: 22px;
           font-weight: 600;
           vertical-align: middle;
           display: flex;
           align-items: center;
        }
        .skin-blue .main-header .navbar {
           background-color: #36454F;
           min-height: 80px;
        }  
        .main-sidebar {
           top: 40px;  /* Position sidebar below the 80px header */
        }
        .sidebar-toggle {
           height: 80px;
           line-height: 80px;
           vertical-align: middle;
           display: flex;
           align-items: center;
           justify-content: center;
        }
        .skin-blue .main-sidebar { 
           background-color: #425561; 
           color: #FFF; 
        }  
        .skin-blue .main-sidebar .sidebar-menu > li > a { 
           color: #FFF; 
        }
        .skin-blue .main-sidebar .sidebar-menu > li.active > a { 
           background-color: #6082B6; 
           color: white; 
        }
        body { 
           font-family: 'Arial', 
           sans-serif; 
           background-color: #FFFFFF; 
        }
        .box { 
           border-radius: 5px; 
           box-shadow: 0 2px 4px rgba(0,0,0,0.1); 
           border: 1px solid #D3D3D3; 
        } 
        .btn-primary { 
           background-color: #003366; 
           border-color: #003366; 
        } 
        .btn-primary:hover { 
           background-color: #002244; 
        } 
      "))
    ),
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload Dataset", width = 12, solidHeader = TRUE, status = "primary",
                    fileInput("file", "Select CSV File", accept = ".csv"),
                    textOutput("upload_status")
                )
              )
      ),
      # View dataset
      tabItem(
        tabName = "view",
        fluidRow(
          box(
            title = "Dataset",
              div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              numericInput(
                "rows_per_page",
                label = NULL,
                value = 10,
                min = 5,
                max = 500,
                step = 5,
                width = "120px"
              )
            ),
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            DTOutput("dataset_table")
          )
        )
      ),
      # Summary Tab
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Dataset Summary", width = 12, solidHeader = TRUE, status = "primary",
                    verbatimTextOutput("dataset_summary")
                )
              )
      ),
      # Plots Tab
      tabItem(tabName = "plots",
              fluidRow(
                box(title = "Generate Plots", width = 12, solidHeader = TRUE, status = "primary",
                    selectInput("plot_type", "Plot Type", choices = c("Scatter" = "scatter", "Histogram" = "histogram", "Bar" = "bar", "Line" = "line")),
                    uiOutput("axis_selectors"),
                    actionButton("plot_btn", "Generate Plot", icon = icon("chart-bar")),
                    downloadButton("download_plot", "Download Plot"),
                    plotOutput("plot_output")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive values for dataset
  datasets <- reactiveValues(uploaded = NULL)
  
  # Upload Handler
  observeEvent(input$file, {
    req(input$file)
    datasets$uploaded <- read.csv(input$file$datapath)
    output$upload_status <- renderText("Dataset uploaded successfully.")
  })
  
  # View Handler
  output$dataset_table <- renderDT({
    req(datasets$uploaded, input$rows_per_page)
    df <- datasets$uploaded
    # Use cleaned names for display
    cleaned_names <- gsub("[_,-]", " ", colnames(df))
    colnames(df) <- cleaned_names
    datatable(
      df,
      filter = "top",
      options = list(pageLength = as.integer(input$rows_per_page),
                     lengthChange = FALSE,
                     filter = 'top', 
                     scrollX = TRUE,  # Horizontal scroll for wide tables
                     scrollY = "400px",  # Vertical scroll to contain height
                     autoWidth = TRUE,
                     searching = TRUE,
                     dom= "tip"
                     ),  # Auto-adjust column widths
              rownames = FALSE
      )
  })
  
  # Summary Handler
  output$dataset_summary <- renderPrint({
    req(datasets$uploaded)
    summary(datasets$uploaded)
  })
  
  # Plot Handler
  output$axis_selectors <- renderUI({
    req(input$plot_type, datasets$uploaded)
    df <- datasets$uploaded
    vars <- colnames(df)
    if (input$plot_type == "scatter" || input$plot_type == "line") {
      fluidRow(
        column(6, selectInput("x_var", "X-Axis Variable", choices = vars)),
        column(6, uiOutput("y_var_selector"))
      )
    } else if (input$plot_type == "histogram") {
      selectInput("x_var", "X-Axis Variable", choices = vars)
    } else if (input$plot_type == "bar") {
      selectInput("x_var", "Categorical Variable", choices = vars)
    }
  })
  
  output$y_var_selector <- renderUI({
    req(input$x_var, datasets$uploaded)
    df <- datasets$uploaded
    vars <- setdiff(colnames(df), input$x_var)
    selectInput("y_var", "Y-Axis Variable", choices = vars)
  })
  
  observeEvent(input$plot_btn, {
    req(datasets$uploaded, input$plot_type)
    df <- datasets$uploaded
    p <- NULL
    if (input$plot_type == "scatter") {
      req(input$x_var, input$y_var)
      p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = plot_colors$scatter[1], size = 3) +
        theme_minimal() + labs(title = "Scatter Plot")
    } 
    else if (input$plot_type == "histogram") {
      req(input$x_var)
      p <- ggplot(df, aes_string(x = input$x_var)) +
        geom_histogram(fill = plot_colors$histogram, color = "black", bins = 30) +
        theme_minimal() + labs(title = "Histogram")
    } 
    else if (input$plot_type == "bar") {
      req(input$x_var)
      p <- ggplot(df, aes_string(x = input$x_var)) +
        geom_bar(fill = plot_colors$bar, color = "black") +
        theme_minimal() + labs(title = "Bar Plot")
    } 
    else if (input$plot_type == "line") {
      req(input$x_var, input$y_var)
      p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
        geom_line(color = plot_colors$line[1], size = 1) +
        geom_point(color = plot_colors$line[2], size = 2) +
        theme_minimal() + labs(title = "Line Plot")
    }
    output$plot_output <- renderPlot(p)
  })
  
  # Download Plot
  output$download_plot <- downloadHandler(
    filename = function() paste("plot_", input$plot_type, ".png", sep = ""),
    content = function(file) {
      req(datasets$uploaded, input$plot_type)
      df <- datasets$uploaded
      p <- NULL
      # Replicate plot logic for download
      if (input$plot_type == "scatter") {
        p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(color = plot_colors$scatter[1], size = 3) + theme_minimal()
      } else if (input$plot_type == "histogram") {
        p <- ggplot(df, aes_string(x = input$x_var)) +
          geom_histogram(fill = plot_colors$histogram, color = "black", bins = 30) + theme_minimal()
      } else if (input$plot_type == "bar") {
        p <- ggplot(df, aes_string(x = input$x_var)) +
          geom_bar(fill = plot_colors$bar, color = "black") + theme_minimal()
      } else if (input$plot_type == "line") {
        p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
          geom_line(color = plot_colors$line[1], size = 1) +
          geom_point(color = plot_colors$line[2], size = 2) + theme_minimal()
      }
      ggsave(file, plot = p, device = "png", width = 8, height = 6)
    }
  )
}

# Run the app
shinyApp(ui, server)
