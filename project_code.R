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
  email = c("hruthika@gmail.com", "user1@test.com"),
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
      
      /* Top-right tabs as buttons */
      .login-top-tabs {
        position: absolute;
        top: 20px;
        right: 30px;
        display: flex;
        gap: 15px;
        font-size: 15px;
        font-weight: 600;
        z-index: 1000;
        border-radius: 5px;
        overflow: hidden;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
      
      .login-top-tabs span {
        cursor: pointer;
        padding: 8px 20px;
        border-radius: 5px;
        background-color: #FFFFFF;
        color: #000000;
        opacity: 0.8;
        transition: all 0.2s;
        border-right: 1px solid #ccc;
      }
      
      .login-top-tabs span:last-child{
        border-right: none;
      }
      
      .login-top-tabs span:hover {
        opacity: 1;
      }
      
      .login-top-tabs .active-tab {
        opacity: 1;
        background-color: #43627D;
        box-shadow: 0 2px 6px rgba(0,0,0,0.3);
        color: #FFFFFF
      }
      
      .login-top-tabs span:hover:not(.active-tab){
        background-color: #f0f0f0;
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
        margin-top: 80px; /* space from top tabs */
      }
      
      /* LOGIN PAGE CODE DONE */
      
       /* Title */
       .login-title {
         font-size: 22px;
         font-weight: 600;
         margin-bottom: 20px;
         color: #36454F;
        #text-align: center;
       }

       /* Inputs */
       .form-control {
         height: 42px;
         font-size: 14px;
         margin-bottom: 15px;
       }

       /* Login/Signup button */
       .login-btn {
         background-color: #43627D !important;
         border-color: #43627D !important;
         color: #FFFFFF !important;
         width: 120px;
         font-size: 16px;
         height: 44px;
         border-radius: 5px;
         cursor: pointer;
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
        # text-align: center;
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
    
    # Top-right login/signup tabs
    # Top-right header buttons
    div(
      class = "login-top-tabs",
      
      tags$span(
        "Login",
        id = "tab_login",
        class = "top-tab active-tab",
        onclick = "Shiny.setInputValue('tab_login', Math.random())"
      ),
      
      span(
        "Sign Up",
        id = "tab_signup",
        class = "top-tab",
        onclick = "Shiny.setInputValue('tab_signup', Math.random())"
      )
    ),
    
    
    div(class = "login-wrapper",
        
        # LEFT BACKGROUND PANEL
        div(class = "login-bg",
            tags$img(
              src = "Act_ver_logo.png",
              class = "bg-logo"
            )
        ),
        
        # LOGIN FORM
        div(id = "login_form", class = "login-card",
            
            div(class = "login-title", "Login"),
            
            # Email
            tags$label("Email address"),
            textInput(
              "username",
              NULL,
              placeholder = "Enter email address"
            ),
            
            # Password
            tags$label("Password"),
            passwordInput(
              "password",
              NULL,
              placeholder = "Enter password"
            ),
            
            # Forgot password
            div(class = "login-links",
                tags$a("Forgot password?")
            ),
            
            br(),
            
            # Login button with icon
            actionButton(
              "login_btn",
              label = tagList(icon("sign-in-alt"), " Login"),
              class = "login-btn"
            ),
            
            br(),
            
            # Login status
            uiOutput("login_status"),
            
            # Don't have account text
            div(
              class = "signup-text",
              tagList(
                "Don’t have an account? ",
                tags$a(
                  "Sign up",
                  href = "#",
                  onclick = "Shiny.setInputValue('tab_signup', Math.random())"
                )
              )
            )
        )
        ,
        
        # SIGN UP FORM (hidden by default)
        div(
          id = "signup_form",
          class = "login-card",
          style = "display:none;",
          
          # Title
          div(class = "login-title", "Sign up"),
          tags$p(
            "Sign up for free to access to any of our products",
            style = "font-size:13px; color:#6c757d; margin-bottom:20px;"
          ),
          
          # Email
          tags$label("Email address"),
          textInput(
            "signup_email",
            NULL,
            placeholder = "Enter email address"
          ),
          
          # Password
          tags$label("Password"),
          passwordInput(
            "signup_password",
            NULL,
            placeholder = "Choose password"
          ),
          tags$p(
            "Use 8 or more characters with a mix of letters, numbers & symbols",
            style = "font-size:12px; color:#6c757d; margin-top:-10px;"
          ),
          
          # Terms checkbox
          checkboxInput(
            "signup_terms",
            label = tagList(
              "Agree to our ",
              tags$a("Terms of use", href = "#"),
              " and ",
              tags$a("Privacy Policy", href = "#")
            ),
            value = FALSE
          ),
          
          # Fake reCAPTCHA (visual only)
          div(
            style = "
      border:1px solid #d3d3d3;
      border-radius:4px;
      padding:12px;
      display:flex;
      align-items:center;
      gap:10px;
      width:260px;
      margin-bottom:20px;
    ",
            tags$input(type = "checkbox"),
            tags$span("I'm not a robot", style = "font-size:13px;")
          ),
          
          # Sign Up button
          actionButton(
            "signup_btn",
            "Sign up",
            class = "login-btn"
          ),
          
          br(),
          
          # Status message
          uiOutput("signup_status"),
          
          # Already have account
          tags$p(
            tagList(
              "Already have an account? ",
              tags$a(
                "Log in",
                href = "#",
                onclick = "Shiny.setInputValue('tab_login', Math.random())"
              )
            ),
            style = "font-size:13px; margin-top:15px;"
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
)

# Server
server <- function(input, output, session) {
  
  datasets <- reactiveValues(uploaded = NULL)
  
  # Toggle Login / Sign Up Tabs
  observeEvent(input$tab_login, {
    shinyjs::show("login_form")
    shinyjs::hide("signup_form")
    
    # Highlight active tab
    runjs("
      document.getElementById('tab_login').classList.add('active-tab');
      document.getElementById('tab_signup').classList.remove('active-tab');
    ")
    
    # Clear login messages
    output$login_status <- renderUI({ NULL })
    output$signup_status <- renderUI({ NULL })
  })
  
  observeEvent(input$tab_signup, {
    shinyjs::show("signup_form")
    shinyjs::hide("login_form")
    
    # Highlight active tab
    runjs("
      document.getElementById('tab_signup').classList.add('active-tab');
      document.getElementById('tab_login').classList.remove('active-tab');
    ")
    
    # Clear login messages
    output$login_status <- renderUI({ NULL })
    output$signup_status <- renderUI({ NULL })
  })
  
  # --------------------------
  # Login Logic
  # --------------------------
  observeEvent(input$login_btn, {
    req(input$username, input$password)
    
    user_match <- valid_credentials$email == input$username
    pass_match <- valid_credentials$password == input$password
    
    if (any(user_match & pass_match)) {
      shinyjs::hide("login_page")
      shinyjs::show("dashboard_page")
      
      output$login_status <- renderUI({
        div(style="color:green;", "Login successful")
      })
      
    } else {
      output$login_status <- renderUI({
        div(class = "login-error", "Invalid username or password!")
      })
    }
  })
  
  
  # Sign Up Logic

  observeEvent(input$signup_btn, {
    req(input$signup_email, input$signup_password, input$signup_terms)
    
    if (!input$signup_terms) {
      output$signup_status <- renderUI({
        div(class = "login-error", "You must agree to the terms.")
      })
      return()
    }
    
    # Check if username already exists
    if (input$signup_email %in% valid_credentials$user) {
      output$signup_status <- renderUI({
        div(class = "login-error", "Username already exists!")
      })
      return()
    }
    
    # Add new user to valid_credentials (for demo purposes only; not persistent)
    valid_credentials <<- rbind(valid_credentials, 
                                data.frame(user = input$signup_email, 
                                           password = input$signup_password,
                                           stringsAsFactors = FALSE))
    
    output$signup_status <- renderUI({
      div(style="color:green;", "Sign Up successful! Please login.")
    })
    
    # Switch back to login form
    shinyjs::show("login_form")
    shinyjs::hide("signup_form")
    
    runjs("
      document.getElementById('tab_login').classList.add('active-tab');
      document.getElementById('tab_signup').classList.remove('active-tab');
    ")
    
    # Clear sign-up inputs
    
    updateTextInput(session, "signup_email", value = "")
    updateTextInput(session, "signup_password", value = "")
    updateTextInput(session, "signup_password", value = "")
    
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

