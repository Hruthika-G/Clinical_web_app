library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(shinyjs)
library(colourpicker)

# Color palettes
plot_colors <- list(
  scatter = c("#20B2AA", "#32CD32"),
  histogram = "#4682B4",
  bar = "#228B22",
  line = c("#36454F", "#20B2AA")
)

# Valid credentials
valid_credentials <- data.frame(
  user = c("Hruthika", "user1"),
  password = c("admin123", "user123"),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  useShinyjs(),
  
  #  Login Page
  div(
    id = "login_page",
    tags$head(
      tags$style(HTML("
      
      /* Top-right tabs */
      .login-top-tabs {
        position: absolute;
        top: 20px;
        right: 30px;
        display: flex;
        gap: 25px;
        font-size: 15px;
        font-weight: 600;
        color: ##43627D;
        z-index: 1000;
      }
      
      .login-top-tabs span {
        cursor: pointer;
        opacity: 0.80;
      }
      
      .login-top-tabs span:hover {
        color: #6F8FAF
        opacity: 1;
        text-decoration: underline;
      }
      
      .active-tab {
        opacity: 1;
        border-bottom: 2px solid #FFFFFF;
        padding-bottom: 3px;
      }
      
      /* Wrapper */
      .login-wrapper {
        height: 100vh;
        width: 100vw;
        display: flex;
        align-items: center;
        justify-content: center;
        position: relative;
        background: #FFFFFF;
        font-family: 'Arial', sans-serif;
      }
      
      /* LEFT background area */
      .login-bg {
        flex: 1;
        height: 100%;
        background-image: url('login_bkrd.png');
        background-size: cover;
        background-position: center;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      /* Centered logo in background */
      .bg-logo {
        max-width: 280px;
        opacity: 0.9;
      }
      
      /* Login card */
      .login-card {
        width: 500px;
        background: #FFFFFF;
        padding: 30px;
        
        display: flex;
        flex-direction: column;
        #justify-content: center; /* vertical center */
        
        
      }
      
      /* Title */
      .login-title {
        font-size: 22px;
        font-weight: 600;
        margin-bottom: 20px;
        color: #36454F;
      }


      /* Inputs */
      .form-control {
        height: 42px;
        font-size: 14px;
      }

      /* Login button */
      .login-btn {
        background-color: #43627D !important;
        border-color: #43627D !important;
        color: #FFFFFF !important;
        width: 120px;
        font-size: 16px;
        height: 44px;
        border-radius: 5px;
      }

      .login-btn:hover {
        background-color: #5E7F9F !important;
      }

      /* Links */
      .login-links {
        display: flex;
        justify-content: space-between;
        font-size: 13px;
        margin-top: 10px;
      }

      .login-links a {
        color: #6F8FAF;
        cursor: pointer;
        text-decoration: none;
      }

      .login-links a:hover {
        text-decoration: underline;
      }

      .signup-text {
        text-align: center;
        margin-top: 18px;
        font-size: 14px;
      }

      .signup-text a {
        color: #6F8FAF;
        font-weight: 600;
        cursor: pointer;
        text-decoration: none;
      }
      
      .login-error {
        color: #C62828;
        font-size: 14px;
        text-align: center;
        margin-top: 8px;
      }

    "))
    ),
    
    div(class = "login-wrapper",
        
        # LEFT BACKGROUND PANEL
        div(class = "login-bg",
            tags$img(
              src = "Act_ver_logo.png",
              class = "bg-logo"
            )
        ),
        
        # LOGIN CARD (UNCHANGED SIZE)
        div(class = "login-card",
            div(class = "login-title", "Login"),
            textInput("username", "Username", placeholder = "Enter username"),
            passwordInput("password", "Password", placeholder = "Enter password"),
            div(class = "login-links",
                tags$a("Forgot password?")
            ),
            br(),
            actionButton(
              "login_btn",
              label = tagList(icon("sign-in-alt"), " Login"),
              class = "login-btn"
            ),
            br(),
            uiOutput("login_status"),
            div(class = "signup-text",
                "Don't have an account? ",
                tags$a("Sign up")
            )
        )
    )
    
  ),
  
  
  #  Dashboard
  hidden(
    div(id = "dashboard_page",
        
        dashboardPage(
          skin = "blue",
          
          dashboardHeader(
            title = "Project",
            titleWidth = 150
          ),
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
 
        /* Fix long variable names in selectInput/selectizeInput */
        .selectize-control.single .selectize-input {
           padding-right: 2.2em !important; # Reserve space for dropdown arrow
           overflow-x: auto !important;     # Allow horizontal scroll 
           white-space: nowrap !important;  # Keep text on one line 
        }
                              
        .selectize-control.single .selectize-input > div {
           max-width: calc(100% - 2.2em) !important;
           overflow: hidden !important;
           white-space: nowrap !important;
        }
                              
        .selectize-control.single .selectize-input:after {
           right: 0.8em !important;
        }
                              
        /* Optional: nicer scrollbar for long text */
        .selectize-control.single .selectize-input::-webkit-scrollbar {
           height: 6px;
        }
        .selectize-control.single .selectize-input::-webkit-scrollbar-thumb {
           background: #cfcfcf;
           border-radius: 10px;
        }
        
      "))
      ),
            
            
            tabItems(
              # Upload Tab
              tabItem(
                tabName = "upload",
                fluidRow(
                  column(width = 6, offset = 3,
                         box(width = 12, solidHeader = FALSE, status = NULL,
                             
                             h4("Upload new file / folder"),
                             p("Upload a CSV or xlsx file to start analysis."),
                             
                             fileInput(
                               "file",
                               label = NULL,
                               accept = ".csv"
                             ),
                             
                             textOutput("upload_status")
                         )
                  )
                )
              ),
              # View dataset
              tabItem(
                tabName = "view",
                fluidRow(
                  box(width = 12, solidHeader = FALSE, status = NULL,
                      div(
                        style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                        numericInput(
                          "rows_per_page",
                          label = "Rows per page",
                          value = 10,
                          min = 5,
                          max = 500,
                          step = 5,
                          width = "160px"
                        )
                      ),
                      
                      DTOutput("dataset_table")
                  )
                )
              ),
              # Summary Tab
              tabItem(tabName = "summary",
                      fluidRow(
                        box(width = 12, solidHeader = FALSE, status = NULL,
                            verbatimTextOutput("dataset_summary")
                        )
                      )
              ),
              
              # Plots Tab
              tabItem(
                tabName = "plots",
                fluidRow(
                  # Histogram (only X)
                  column(
                    width = 6,
                    box(width = NULL, solidHeader = FALSE, status = NULL,
                        h4("Histogram"),
                        fluidRow(
                          column(width = 3, 
                                 selectizeInput("hist_x", 
                                             "X Variable", 
                                             choices = NULL, 
                                             options = list(placeholder = "Select X Variable"),
                                             ))
                        ),
                        plotOutput("plot1", height = "300px")
                    )
                  ),
                  
                  # Bar Plot (X and Y)
                  column(
                    width = 6,
                    box(width = NULL, solidHeader = FALSE, status = NULL,
                        h4("Bar Plot"),
                        fluidRow(
                          column(width = 3, selectizeInput("bar_x", 
                                                           "X Variable", 
                                                           choices = NULL,
                                                           options = list(placeholder = "Select X Variable"),
                                                          )),
                          column(width = 3, selectizeInput("bar_y", 
                                                           "Y Variable", 
                                                           choices = NULL,
                                                           options = list(placeholder = "Select Y Variable"),
                                                          ))
                        ),
                        plotOutput("plot2", height = "300px")
                    )
                  )
                ),
                
                fluidRow(
                  # Box Plot (X and Y)
                  column(
                    width = 6,
                    box(width = NULL, solidHeader = FALSE, status = NULL,
                        h4("Box Plot"),
                        fluidRow(
                          column(width = 3, selectizeInput("box_x", 
                                                           "X Variable", 
                                                           choices = NULL,
                                                           options = list(placeholder = "Select X Variable"),
                                                          )),
                          column(width = 3, selectizeInput("box_y", 
                                                           "Y Variable", 
                                                           choices = NULL,
                                                           options = list(placeholder = "Select Y Variable"),
                                                          ))
                        ),
                        plotOutput("plot3", height = "300px")
                    )
                  ),
                  
                  # Scatter Plot (X and Y)
                  column(
                    width = 6,
                    box(width = NULL, solidHeader = FALSE, status = NULL,
                        h4("Scatter Plot"),
                        fluidRow(
                          column(width = 3, selectizeInput("scatter_x", 
                                                           "X Variable", 
                                                            choices = NULL,
                                                            options = list(placeholder = "Select X Variable"),
                                                          )),
                          column(width = 3, selectizeInput("scatter_y", 
                                                           "Y Variable", 
                                                           choices = NULL,
                                                           options = list(placeholder = "Select Y Variable"),
                                                          ))
                        ),
                        plotOutput("plot4", height = "300px")
                    )
                  )
                )
              )
              
      
            )
          )
        )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  datasets <- reactiveValues(uploaded = NULL)
  
  #  Login Logic
  observeEvent(input$login_btn, {
    req(input$username, input$password)
    
    user_match <- valid_credentials$user == input$username
    pass_match <- valid_credentials$password == input$password
    
    if (any(user_match & pass_match)) {
      shinyjs::hide("login_page")
      shinyjs::show("dashboard_page")
      output$login_status <- renderUI({
        "Login successful"})
    } 
    else {
      output$login_status <- renderUI({
        div(class = "login-error", "Invalid username or password!")
      })
    }
  })
  
  #  Logout Logic
  observeEvent(input$logout_btn, {
    
    # Hide dashboard, show login
    shinyjs::hide("dashboard_page")
    shinyjs::show("login_page")
    
    # Clear inputs
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    
    # Clear login message
    output$login_status <- renderUI({ NULL })
    
  })
  
  #  Upload Handler
  observeEvent(input$file, {
    req(input$file)
    datasets$uploaded <- read.csv(input$file$datapath)
    output$upload_status <- renderText("Dataset uploaded successfully.")
  })
  
  #  Dataset Table
  output$dataset_table <- renderDT({
    req(datasets$uploaded, input$rows_per_page)
    df <- datasets$uploaded
    colnames(df) <- gsub("[_,-]", " ", colnames(df))
    datatable(df,
              filter = "top",
              options = list(pageLength = as.integer(input$rows_per_page),
                             lengthChange = FALSE,
                             scrollX = TRUE,
                             scrollY = "400px",
                             autoWidth = TRUE,
                             searching = TRUE,
                             dom= "tip"),
              rownames = FALSE)
  })
  
  #  Summary 
  output$dataset_summary <- renderPrint({
    req(datasets$uploaded)
    summary(datasets$uploaded)
  })
  
  # Update choices for all selectors when dataset is uploaded
  observe({
    req(datasets$uploaded)
    cols <- colnames(datasets$uploaded)
    
    updateSelectInput(session, "hist_x", choices = cols)
    updateSelectInput(session, "bar_x", choices = cols)
    updateSelectInput(session, "bar_y", choices = cols)      
    updateSelectInput(session, "box_x", choices = cols)
    updateSelectInput(session, "box_y", choices = cols)
    updateSelectInput(session, "scatter_x", choices = cols)
    updateSelectInput(session, "scatter_y", choices = cols)
  })
  
  # Histogram
  output$plot1 <- renderPlot({
    req(datasets$uploaded, input$hist_x)
    ggplot(datasets$uploaded, aes_string(x = input$hist_x)) +
      geom_histogram(fill = plot_colors$histogram, color = "black", bins = 30) +
      theme_minimal()
  })
  
  # Bar Plot
  output$plot2 <- renderPlot({
    req(datasets$uploaded, input$bar_x, input$bar_y)
    ggplot(datasets$uploaded, aes_string(x = input$bar_x, fill = input$bar_y)) +
      geom_bar(color = "black", position = "dodge") + # geom_col = geom_bar(stat = "identity)
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Box Plot
  output$plot3 <- renderPlot({
    req(datasets$uploaded, input$box_x, input$box_y)
    ggplot(datasets$uploaded, aes_string(x = input$box_x, y = input$box_y)) +
      geom_boxplot(fill = plot_colors$line[1], color = plot_colors$line[2]) +
      theme_minimal()
  })
  
  # Scatter Plot
  output$plot4 <- renderPlot({
    req(datasets$uploaded, input$scatter_x, input$scatter_y)
    ggplot(datasets$uploaded, aes_string(x = input$scatter_x, y = input$scatter_y)) +
      geom_point(color = plot_colors$scatter[1], size = 3) +
      theme_minimal()
  })
  
  
}

# Run App
shinyApp(ui, server)

