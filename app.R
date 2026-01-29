library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(readxl)
library(dplyr)
library(rsconnect)


ui <- fluidPage(
  
  titlePanel("Data Analysis"),
  theme = shinytheme("yeti"),
  
  sidebarLayout(
    sidebarPanel(
      hr(),
      fileInput("upload", "Upload CSV or Excel file",
                accept = c(".csv", ".xlsx")),
      width = 3,
      hr()
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel("Dataset", br(),
                 DTOutput("data_tbl")),
        
        tabPanel("Summary", br(), 
                 verbatimTextOutput("summary")),
        
        tabPanel("Plots", br(),
                 
                 fluidRow(
                   column(width =4, selectInput("ptype", "Plot type", choices =c("None" = "", 
                                                                                 "Scatter plot" = "scatter",
                                                                                 "Histogram" = "hist",
                                                                                 "Boxplot" = "box",
                                                                                 "Barplot" = "bar"))),
                   column(width =4, selectInput("xvar", "X-axis", choices = c("None" = ""))),
                   column(width =4, selectInput("yvar", "Y-axis (optional)", choices = c("None" = "")))
                 ),
                 br(),
                 plotOutput("plot_out", height = "350px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Read uploaded data
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    
    if (ext == "csv") {
      df <- read.csv(input$upload$datapath, stringsAsFactors = FALSE)
    } else {
      df <- read_excel(input$upload$datapath)
    }
    
    df <- df %>% mutate(across(where(is.character), ~ {
      cleaned <- gsub("[^0-9\\.\\-]", "", .)      
      converted <- suppressWarnings(as.numeric(cleaned))
      
      # If >90% successfully convert, make it numeric
      if (mean(!is.na(converted)) > 0.9) converted else .
    }))
    
  })
  
  # Show data-set
  output$data_tbl <- DT::renderDataTable({
    data()
  },
  options = list(pageLength = 5),
  rownames = FALSE
  )
  
  # Display summary
  output$summary <- renderPrint(summary(data()))
  
  # Update X and Y drop-downs after upload
  observeEvent(data(), {
    cols <- names(data())
    
    updateSelectInput(session, "xvar",
                      choices = c("None" = "", cols))
    
    
    updateSelectInput(session, "yvar",
                      choices = c("None" = "", cols))
    
  })
  
  
  # Draw plot ONLY when everything is selected
  output$plot_out <- renderPlot({
    
    req(input$xvar != "", input$ptype != "")
    
    
    df <- data()  
    
    if (input$ptype == "hist") 
    {
      ggplot(df, aes_string(x = input$xvar)) +
        geom_histogram(
          fill = "darkseagreen",
          color = "black",
          bins = 10
        ) + scale_x_log10() +
        labs(
          title = paste("Histogram of", input$xvar),
          x = input$xvar,
          y = "Count"
        )
    }
    
    else if (input$ptype == "bar") 
    {
      req(input$yvar) 
      if(is.numeric(df[[input$yvar]]))
      {
        ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
          geom_bar(fill = "saddlebrown", position = "dodge") +
          labs(
            title = paste("Bar Plot of", input$xvar, "vs", input$yvar),
            x = input$xvar,
            y = input$yvar
          ) + theme_minimal()
      }
      else
      {
        ggplot(df, aes_string(x = input$xvar, fill = input$yvar)) +
          geom_bar(fill = "saddlebrown", position = "dodge") +
          labs(
            title = paste("Bar Plot of", input$xvar, "vs", input$yvar),
            x = input$xvar,
            y = input$yvar
          ) + theme_minimal()
      }        
      
    }
    
    else if (input$ptype == "scatter") 
    {
      req(input$yvar)
      ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point(size = 3, alpha = 0.7, color = "cyan4") +
        labs(
          title = paste("Scatter Plot:", input$xvar, "vs", input$yvar),
          x = input$xvar,
          y = input$yvar
        )
    }
    
    else if (input$ptype == "box") 
    {
      req(input$yvar)
      # Two-variable boxplot: numeric X vs discrete Y
      if (is.numeric(df[[input$yvar]])) 
      {
        
        ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
          geom_boxplot(fill = "plum4") +
          scale_y_continuous(labels = scales::label_number()) +
          scale_y_log10() +     # <-- add this line
          coord_cartesian(clip = "off") +
          labs(
            title = paste("Box Plot of", input$yvar, "by", input$xvar),
            x = input$xvar,
            y = input$yvar
          ) + theme_minimal()
      } 
      else if (is.numeric(df[[input$xvar]]))
      {
        ggplot(df, aes_string(x = input$yvar, y = input$xvar)) +
          geom_boxplot(fill = "plum4") + 
          scale_y_continuous(labels = scales::label_number()) +
          scale_y_log10() +     # <-- add this line
          coord_cartesian(clip = "off") +
          labs(
            title = paste("Box Plot of", input$xvar, "by", input$yvar),
            x = input$yvar,
            y = input$xvar
          ) + theme_minimal()
      }
      
    }
  })
}

shinyApp(ui, server)
