if(!require(shiny)) install.packages("shiny"); library(shiny)
if(!require(shinydashboard)) install.packages("shinydashboard"); library(shinydashboard)
if(!require(shinydashboardPlus)) install.packages("shinydashboardPlus"); library(shinydashboardPlus)
if(!require(shinyWidgets)) install.packages("shinyWidgets"); library(shinyWidgets)
if(!require(DatabaseConnector)) install.packages("DatabaseConnector"); library(DatabaseConnector)
if(!require(sqldf)) install.packages("sqldf"); library(sqldf)
if(!require(rio)) install.packages("rio"); library(rio)
if(!require(readxl)) install.packages("readxl"); library(readxl)
if(!require(RPostgreSQL)) install.packages("RPostgreSQL"); library(RPostgreSQL)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(reshape2)) install.packages("reshape2"); library(reshape2)
if(!require(networkD3)) install.packages("networkD3"); library(networkD3)
if(!require(DBI)) install.packages("DBI"); library(DBI)
if(!require(SqlRender)) install.packages("SqlRender"); library(SqlRender)
if(!require(stringr)) install.packages("stringr"); library(stringr)
if(!require(DT)) install.packages("DT"); library(DT)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(sunburstR)) install.packages("sunburstR"); library(sunburstR)
if(!require(shinycssloaders)) install.packages("shinycssloaders"); library(shinycssloaders)
if(!require(lubridate)) install.packages("lubridate"); library(lubridate)

css <- "
     #reverseSlider .irs-bar {
        border-top: 1px solid #ddd;
        border-bottom: 1px solid #ddd;
        background: linear-gradient(to bottom, #DDD -50%, #FFF 150%); 
      }
    
     #reverseSlider .irs-bar-edge {
      border-top: 1px solid #ddd;
      border-right: 0;
      background: linear-gradient(to bottom, #DDD -50%, #FFF 150%); 
    }
    
     #reverseSlider .irs-line {
      border: 1px solid #428bca;
      background: #428bca; 
    }
    "

#UI----
ui <- dashboardPage(
  header = dashboardHeader(
    title = "Automated Treatment Pathway Analyzer", titleWidth = 400
  ),
  
  sidebar = dashboardSidebar(
    width = 0,
    sidebarMenu(
      menuItem("CDM Data Search", tabName = "Search", icon = icon("search"))
    )
  ),    
  
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "Search",
              navbarPage(title=div(icon("jsfiddle"),""), 
                         tabPanel("SETUP", icon = icon("database"),
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  box(
                                                    title = "Initial Setup : Text Input", width = 12, status = "info",
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(
                                                               textInput('text_dbname', "DB name", placeholder = 'db name'),
                                                               textInput('text_host', "Host", placeholder = 'host IP address'),
                                                               textInput('text_port', "Port", placeholder = 'port number'),
                                                               textInput('text_user', "User", placeholder = 'user name'),
                                                               passwordInput('text_password', "Password", placeholder = 'password number'), 
                                                               title = "DataBase Connect Information Input", solidHeader = TRUE, width = 6, status = "info"),
                                                             
                                                             box(
                                                               textInput('text_cdmschema', "CDM Schema", placeholder = "e.g., cdm (including 'cdm' Table)"),
                                                               textInput('text_cohortschema', "Cohort Schema", placeholder = "e.g., results (including 'cohort' Table)"),
                                                               textInput('text_cohortdefinitionschema', "Cohort Definition Schema", placeholder = "e.g., webapi (including 'cohort_definition' Table)"),
                                                               title = "DataBase Schema Information Input", solidHeader = TRUE, width = 6, status = "info")
                                                             
                                                      ),
                                                      
                                                      column(width = 12,
                                                             column(width = 12,
                                                                    actionButton("button_DB_cohort", "Show Result (refresh)", icon = icon("arrow-alt-circle-down", "fa-fw"),
                                                                                 style="color: #fff; background-color: #17BDFF; border-color: #17BDFF; font-size:15px;"), br(), br())
                                                             
                                                      )),
                                                    
                                                    fluidRow(
                                                      column(width = 12,
                                                             box(
                                                               verbatimTextOutput("check_dbConnect", placeholder = T),
                                                               title = "Check DataBase Connect : Success OR Failure",
                                                               solidHeader = TRUE, width = 6, status = "info"),
                                                             
                                                             
                                                             box(
                                                               verbatimTextOutput("check_cdmschema", placeholder = T),
                                                               verbatimTextOutput("check_cohortschema", placeholder = T),
                                                               verbatimTextOutput("check_cohortdefinitionschema", placeholder = T),
                                                               title = "Check DataBase Schema : Success OR Failure",
                                                               solidHeader = TRUE, width = 6, status = "info")
                                                      )
                                                    )
                                                  )
                                           )
                                    )
                                  )
                                  
                         ),
                         
                         tabPanel('DEMO', icon = icon("project-diagram"),
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  box(
                                                    title = "STEP 1 : Cohort Selection", width = 12, status = "primary",
                                                    box(
                                                      fluidRow(
                                                        column(width = 12,
                                                               div(style = 'overflow-x: scroll;', DT::dataTableOutput("demo_cohort_difinition_DT")), br(), br()),
                                                        
                                                        column(width = 12,
                                                               column(width = 6,
                                                                      tags$style(type = 'text/css', css),
                                                                      div(id = 'reverseSlider',
                                                                          sliderInput("demo_slider_cohortStartDate_sqlRange_min", 
                                                                                      "Drug Exposure Window [약물 사용 기간]", 
                                                                                      min = -365, max = 0, value = -365))),
                                                               column(width = 6,
                                                                      sliderInput("demo_slider_cohortStartDate_sqlRange_max", 
                                                                                  "index date(0 day)", 
                                                                                  min = 0, max = 365*5, value = 365*5)))),
                                                      title = "ATLAS cohort list", solidHeader = TRUE, width = 12, status = "primary")
                                                  )
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  box(
                                                    title = "STEP 2 : Filter - ATC Drug Level (Scenario)", width = 12, status = "primary",
                                                    awesomeRadio(inputId = 'demo_radio_scenario', label = "",
                                                                 choices = list("Scenario 1 : Atrial fibrillation (ATC filter = [B01A])" = 1,
                                                                                "Scenario 2 : Ankylosing spondylitis (ATC filter = [L04, M01, A07EC])" = 2),
                                                                 selected = 1,
                                                                 status = "primary")
                                                  )
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  column(width = 6,
                                                         actionButton("demo_button_drugname_pathway", "Show Result (refresh)", icon = icon("arrow-alt-circle-down", "fa-fw"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:15px;"), br(), br())
                                                  
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  box(
                                                    title = "STEP 3 : Treatment Pathway", width = 12, status = "primary",
                                                    box(
                                                      fluidRow(
                                                        column(width = 12,
                                                               div(style = 'overflow-x: scroll;', DT::dataTableOutput("demo_pathway_DT")))), br(), br(), 
                                                      
                                                      box(
                                                        fluidRow(
                                                          column(width = 12,
                                                                 column(width = 12,
                                                                        awesomeRadio(inputId = 'demo_radio_pathway', label = "",
                                                                                     choices = list("ATC 1st" = 1, "ATC 2nd" = 2, 
                                                                                                    "ATC 3rd" = 3, "ATC 4th" = 4, "ATC 5th" = 5),
                                                                                     selected = 5, status = "primary", inline = TRUE)))),
                                                        fluidRow(
                                                          column(width = 12,
                                                                 column(width = 4,
                                                                        sliderInput("demo_slider_usedDrug_min", 
                                                                                    "Min Frequency of Drug(%) [최소 약물 사용 빈도: 전체 환자 중 OO% 이상 사용된 약물만 포함]", 
                                                                                    min = 0, max = 10, value = 0.5)),
                                                                 column(width = 4,
                                                                        sliderInput("demo_slider_step_max", 
                                                                                    "Max Number of Steps [최대 스텝 개수]", 
                                                                                    min = 1, max = 10, value = 5)),
                                                                 column(width = 4,
                                                                        sliderInput("demo_slider_link_min", 
                                                                                    "Min Number of Links [스텝 연결 빈도: 최소 OO개 이상인 경우만 포함]", 
                                                                                    min = 1, max = 10, value = 5)))), br(),
                                                        fluidRow(
                                                          column(width = 12,
                                                                 column(width = 6,
                                                                        tags$style(type = 'text/css', css),
                                                                        div(id = 'reverseSlider',
                                                                            sliderInput("demo_slider_cohortStartDate_quryRange_min", 
                                                                                        "Drug Exposure Window [약물 사용 기간]", 
                                                                                        min = -365, max = 0, value = -365))),
                                                                 column(width = 6,
                                                                        sliderInput("demo_slider_cohortStartDate_quryRange_max", 
                                                                                    "index date(0 day)", 
                                                                                    min = 0, max = 365*5, value = 365*5)))),
                                                        title = "Display Option", width = 12, status = "primary"),
                                                      
                                                      title = "", width = 12, status = "primary", solidHeader = TRUE
                                                    )
                                                  ))
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  column(width = 6,
                                                         actionButton("demo_button_pathway_visual", "Show Result (refresh)", icon = icon("arrow-alt-circle-down", "fa-fw"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:15px;"), br(), br())
                                                  
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  tabBox(
                                                    id = 'dataset', width = 12,
                                                    tabPanel("SankeyNetwork", 
                                                             downloadButton("demo_sankeyNetwork_html", "Export"), br(), br(),
                                                             sankeyNetworkOutput("demo_sankeyNetwork", height = '1000px')),
                                                    
                                                    tabPanel("Sunburst",
                                                             sund2bOutput('demo_sunburst', height = '1000px'))
                                                  )))
                                  )),
                         
                         tabPanel('Pathway Analysis', icon = icon("project-diagram"),
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  box(
                                                    title = "STEP 1 : Cohort Selection", width = 12, status = "primary",
                                                    box(
                                                      fluidRow(
                                                        column(width = 12,
                                                               div(style = 'overflow-x: scroll;', DT::dataTableOutput("cohort_difinition_DT")), br(), br()),
                                                        
                                                        column(width = 12,
                                                               column(width = 6,
                                                                      tags$style(type = 'text/css', css),
                                                                      div(id = 'reverseSlider',
                                                                          sliderInput("slider_cohortStartDate_sqlRange_min", 
                                                                                      "Drug Exposure Window [약물 사용 기간]", 
                                                                                      min = -365, max = 0, value = -365))),
                                                               column(width = 6,
                                                                      sliderInput("slider_cohortStartDate_sqlRange_max", 
                                                                                  "index date(0 day)", 
                                                                                  min = 0, max = 365*5, value = 365*5)))),
                                                      title = "ATLAS cohort list", solidHeader = TRUE, width = 12, status = "primary")
                                                  )
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  column(width = 6,
                                                         actionButton("button_cohort_conceptClass", "Show Result (refresh)", icon = icon("arrow-alt-circle-down", "fa-fw"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:15px;"), br(), br())
                                                  
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  box(
                                                    title = "STEP 2 : Filter - ATC Drug Level", width = 12, status = "primary",
                                                    box(
                                                      div(style = 'overflow-x: scroll;', DT::dataTableOutput("conceptClass_DT")),
                                                      title = "Concept Class Name", width = 2, status = "primary", solidHeader = TRUE
                                                    ),
                                                    box(
                                                      actionButton("button_conceptClass_conceptCode", "Show Result (refresh)", icon = icon("arrow-alt-circle-down", "fa-fw"),
                                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:15px;"), br(), br(),
                                                      div(style = 'overflow-x: scroll;', DT::dataTableOutput("conceptCode_DT")),
                                                      title = "Concept Code Name", width = 6, status = "primary", solidHeader = TRUE
                                                    ),
                                                    box(
                                                      actionButton("button_conceptCode_drugName", "Show Result (refresh)", icon = icon("arrow-alt-circle-down", "fa-fw"),
                                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:15px;"), br(), br(),
                                                      div(style = 'overflow-x: scroll;', DT::dataTableOutput("drugName_DT")),
                                                      title = "Drug Name", width = 4, status = "primary", solidHeader = TRUE
                                                    )
                                                  )
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  column(width = 6,
                                                         actionButton("button_drugname_pathway", "Show Result (refresh)", icon = icon("arrow-alt-circle-down", "fa-fw"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:15px;"), br(), br())
                                                  
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  box(
                                                    title = "STEP 3 : Treatment Pathway", width = 12, status = "primary",
                                                    box(
                                                      fluidRow(
                                                        column(width = 12,
                                                               div(style = 'overflow-x: scroll;', DT::dataTableOutput("pathway_DT")))), br(), br(), 
                                                      
                                                      box(
                                                        fluidRow(
                                                          column(width = 12,
                                                                 column(width = 12,
                                                                        awesomeRadio(inputId = 'radio_pathway', label = "",
                                                                                     choices = list("ATC 1st" = 1, "ATC 2nd" = 2, 
                                                                                                    "ATC 3rd" = 3, "ATC 4th" = 4, "ATC 5th" = 5),
                                                                                     selected = 5, status = "primary", inline = TRUE)))),
                                                        fluidRow(
                                                          column(width = 12,
                                                                 column(width = 4,
                                                                        sliderInput("slider_usedDrug_min", 
                                                                                    "Min Frequency of Drug(%) [최소 약물 사용 빈도: 전체 환자 중 OO% 이상 사용된 약물만 포함]", 
                                                                                    min = 0, max = 10, value = 0.5)),
                                                                 column(width = 4,
                                                                        sliderInput("slider_step_max", 
                                                                                    "Max Number of Steps [최대 스텝 개수]", 
                                                                                    min = 1, max = 10, value = 5)),
                                                                 column(width = 4,
                                                                        sliderInput("slider_link_min", 
                                                                                    "Min Number of Links [스텝 연결 빈도: 최소 OO개 이상인 경우만 포함]", 
                                                                                    min = 1, max = 10, value = 5)))), br(),
                                                        fluidRow(
                                                          column(width = 12,
                                                                 column(width = 6,
                                                                        tags$style(type = 'text/css', css),
                                                                        div(id = 'reverseSlider',
                                                                            sliderInput("slider_cohortStartDate_quryRange_min", 
                                                                                        "Drug Exposure Window [약물 사용 기간]", 
                                                                                        min = -365, max = 0, value = -365))),
                                                                 column(width = 6,
                                                                        sliderInput("slider_cohortStartDate_quryRange_max", 
                                                                                    "index date(0 day)", 
                                                                                    min = 0, max = 365*5, value = 365*5)))),
                                                        title = "Display Option", width = 12, status = "primary"),
                                                      
                                                      title = "", width = 12, status = "primary", solidHeader = TRUE
                                                    )
                                                  ))
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  column(width = 6,
                                                         actionButton("button_pathway_visual", "Show Result (refresh)", icon = icon("arrow-alt-circle-down", "fa-fw"),
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:15px;"), br(), br())
                                                  
                                           )
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           column(width = 12,
                                                  tabBox(
                                                    id = 'dataset', width = 12,
                                                    tabPanel("SankeyNetwork", 
                                                             downloadButton("sankeyNetwork_html", "Export"), br(), br(),
                                                             sankeyNetworkOutput("sankeyNetwork", height = '1000px')),
                                                    
                                                    tabPanel("Sunburst", 
                                                             sund2bOutput('sunburst', height = '1000px'))
                                                  )))
                                  )))
      )
    ),
    
    ## UI CSS code ##
    
    #' tags$head(
    #'   tags$script(' $(document).on("keydown", function (e) {
    #'               Shiny.onInputChange("lastkeypresscode", e.keyCode);
    #'               });
    #'               '),
    #'   
    #'   tags$style(
    #'     HTML('
    #'          @import url("https://fonts.googleapis.com/css?family = Acme");
    #'          @import url("https://fonts.googleapis.com/css?family = Roboto");
    #'          
    #'          .skin-blue .main-header .logo {
    #'          font-family: "Acme", sans-serif;
    #'          background-color: rgb(7, 125, 150);
    #'          font-size:26px;
    #'          }
    #'          
    #'          /* logo when hovered */
    #'          .skin-blue .main-header .logo:hover {
    #'          background-color: rgb(7, 125, 150);
    #'          }
    #'          
    #'          /* navbar (rest of the header) */
    #'          .skin-blue .main-header .navbar {
    #'          background-color: rgb(7, 125, 150);
    #'          }
    #'          
    #'          /* main sidebar */
    #'          .skin-blue .main-sidebar {
    #'          font-family: "Acme", sans-serif;
    #'          background-color:  #088da5;
    #'          }
    #'          
    #'          /* active selected tab in the sidebarmenu */
    #'          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    #'          font-family: "Acme", sans-serif;
    #'          background-color: #39a3b7;
    #'          color: #ffffff;
    #'          }
    #'          
    #'          /* other links in the sidebarmenu */
    #'          .skin-blue .main-sidebar .sidebar .sidebar-menu a{
    #'          background-color: #088da5;
    #'          color: #000000;
    #'          border: 0.9px solid #088da5;
    #'          }
    #'          
    #'          /* other links in the sidebarmenu when hovered */
    #'          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
    #'          background-color: RGB(8,141,168);
    #'          color: #ffffff;
    #'          }
    #'          
    #'          /* toggle button when hovered  */
    #'          .skin-blue .main-header .navbar .sidebar-toggle:hover{
    #'          background-color: #9cd1db;
    #'          color: #ffffff;
    #'          }
    #'          
    #'          /* content color */
    #'          .skin-blue .content{
    #'          height: 2000px;
    #'          background-color: #39a3b7;
    #'          font-family: "Roboto", sans-serif;
    #'          }
    #'          
    #'          /* content tab color */
    #'          .skin-blue .content .tab-content{
    #'          background-color: #ffffff;
    #'          border-radius: 1rem;
    #'          }
    #'          
    #'          /* navbar color */
    #'          .skin-blue .content .tab-content .navbar {
    #'          background-color: RGB(8,141,168);
    #'          border: 0.9px solid RGB(8,141,168);
    #'          border-radius: 0.9rem;
    #'          }
    #'          
    #'          /* navbar header text color */
    #'          .skin-blue .content .tab-content .navbar .container-fluid .navbar-header .navbar-brand{
    #'          font-family: "Acme", sans-serif;
    #'          color: #ffffff;
    #'          font-size:21px;
    #'          }
    #'          
    #'          /* navbar unactive text color */
    #'          .skin-blue .content .tab-content .navbar .container-fluid .nav li a{
    #'          background-color: RGB(8,141,168);
    #'          font-family: "Acme", sans-serif;
    #'          color: #ffffff;
    #'          font-size:17px;
    #'          }
    #'          
    #'          /* navbar sub tab color */
    #'          .skin-blue .content .tab-content .navbar .container-fluid .nav .active a{
    #'          font-family: "Acme", sans-serif;
    #'          color: #ffffff;
    #'          background-color: rgb(7, 125, 150);
    #'          border-radius: 0.9rem;
    #'          }
    #'          ')
    #'   )
    #' )
  )
)

server <- function(input, output, session) { 
  DB_connect <- function() {
    driver = dbDriver("PostgreSQL")
    conn <- dbConnect(driver,
                      dbname = input$text_dbname,
                      host = input$text_host,
                      user = input$text_user,
                      port = input$text_port,
                      password = input$text_password)
    
    options(sqldf.RPostgreSQL.dbname = input$text_dbname,
            sqldf.RPostgreSQL.host = input$text_host,
            sqldf.RPostgreSQL.user = input$text_user,
            sqldf.RPostgreSQL.port = input$text_port,
            sqldf.RPostgreSQL.password = input$text_password)
    
    return(conn)
  }
  
  observeEvent(input$button_DB_cohort,{
    try({
      conn <- DB_connect()
      
      output$check_dbConnect <- renderPrint({
        if(isPostgresqlIdCurrent(conn))
          print('Success! : DataBase Connect')
      })
      return(conn)
    })
    
    dbDisconnect(conn)
    output$check_dbConnect <- renderPrint({
      print('Failure : Rewrite DataBase Connect Information')
    })
  })
  
  observeEvent(input$button_DB_cohort,{
    try({
      conn <- DB_connect()
      
      sql <- "SELECT *
              FROM @cdmschema.concept
              LIMIT 10"
      
      sql <- render(sql, cdmschema = input$text_cdmschema)
      concept <- dbGetQuery(conn, sql)
      dbDisconnect(conn)
      
      output$check_cdmschema <- renderPrint({
        if(length(concept) > 0)
          print('Success! : CDM Schema')
        else
          print('Failure : Rewrite CDM Schema')
      })
      return(concept)
    })
    
    output$check_cdmschema <- renderPrint({
      print('Failure : Rewrite CDM Schema')
    })
  })
  
  observeEvent(input$button_DB_cohort,{
    try({
      conn <- DB_connect()
      
      sql <- "SELECT *
              FROM @cohortschema.cohort
              LIMIT 10"
      
      sql <- render(sql, cohortschema = input$text_cohortschema)
      cohort <- dbGetQuery(conn, sql)
      dbDisconnect(conn)
      
      output$check_cohortschema <- renderPrint({
        if(length(cohort) > 0)
          print('Success! : Cohort Schema')
        else
          print('Failure : Rewrite Cohort Schema')
      })
      return(cohort)
    })
    
    output$check_cohortschema <- renderPrint({
      print('Failure : Rewrite Cohort Schema')
    })
  })
  
  observeEvent(input$button_DB_cohort,{
    try({
      conn <- DB_connect()
      
      sql <- "SELECT *
              FROM @cohortdefinitionschema.cohort_definition
              LIMIT 10"
      
      sql <- render(sql, cohortdefinitionschema = input$text_cohortdefinitionschema)
      cohort_definition <- dbGetQuery(conn, sql)
      dbDisconnect(conn)
      
      output$check_cohortdefinitionschema <- renderPrint({
        if(length(cohort_definition) > 0)
          print('Success! : Cohort Definition Schema')
        else
          print('Failure : Rewrite Cohort Definition Schema')
      })
      
      return(cohort_definition)
    })
    
    output$check_cohortdefinitionschema <- renderPrint({
      print('Failure : Rewrite Cohort Definition Schema')
    })
  })
  
  output$demo_cohort_difinition_DT <- DT::renderDataTable(
    DT::datatable(
      button_cohort_difinition_DB(), selection = 'single',
      filter = list(position = 'top', clear = FALSE), options = list(pageLength = 5, order = list(3, 'desc'))
    )
  )
  
  demo_drug_DB <- function() {
    cohort_difinition_selected <- cohort_difinition_DB()[input$demo_cohort_difinition_DT_rows_selected,]
    cohort_difinition_selected <- subset(cohort_difinition_selected, select="cohort_definition_id")
    
    conn <- DB_connect()
    
    sql_drug <- "SELECT subject_id, vocabulary_id, concept_class_id, concept_code, ancestor_concept_id, concept_name as drug_name,  
                        (min(drug_exposure_start_date) - cohort_start_date) as drug_first_day
                 FROM @cdmschema.drug_exposure de, @cdmschema.concept c, @cohortschema.cohort ch, @cdmschema.concept_ancestor ca
                 WHERE de.person_id = ch.subject_id
                 AND ch.cohort_definition_id = @cohort_definition_id
                 AND ca.descendant_concept_id=de.drug_concept_id
                 AND c.concept_id=ca.ancestor_concept_id
                 AND c.vocabulary_id in ('ATC')
                 AND de.drug_exposure_start_date between ch.cohort_start_date + @before AND ch.cohort_start_date + @after
                 Group by subject_id, cohort_start_date, vocabulary_id, concept_class_id, concept_code, ancestor_concept_id, concept_name"
    
    sql_drug <- render(sql_drug, cdmschema = input$text_cdmschema, cohortschema = input$text_cohortschema, 
                       cohort_definition_id = cohort_difinition_selected, 
                       before = input$demo_slider_cohortStartDate_sqlRange_min, 
                       after = input$demo_slider_cohortStartDate_sqlRange_max)
    drug <- dbGetQuery(conn, sql_drug) 
    dbDisconnect(conn)
    
    saveRDS(drug,'./demo_drug.rds')
  }
  
  output$demo_pathway_DT <- DT::renderDataTable({
    DT::datatable(
      demo_button_pathway_DB(),
      filter = list(position = 'top', clear = FALSE),
      options = list(search = list(regex = TRUE), pageLength = 5)
    )
  })
  
  demo_button_pathway_DB <- eventReactive(input$demo_button_drugname_pathway, {
    if(length(input$demo_cohort_difinition_DT_rows_all) > 0)
      demo_pathway_DB() 
    
    else
      data.frame(count="", perc="", first="", second="")
  })
  
  demo_drug_step1 <- function(drugName) {
    if(input$demo_radio_pathway == '1')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 1st",])
    else if(input$demo_radio_pathway == '2')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 2nd",])
    else if(input$demo_radio_pathway == '3')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 3rd",])
    else if(input$demo_radio_pathway == '4')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 4th",])
    else if(input$demo_radio_pathway == '5')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 5th",])
    
    no.person <- length(unique(drugName$subject_id))
    
    list.freq.dr <- drugName %>%
      filter(drug_first_day >= 0) %>%
      group_by(drug_name) %>%
      summarise(count.person=length(unique(subject_id)), perc=round(count.person/no.person*100,1)) %>%
      filter(perc >= input$demo_slider_usedDrug_min)
    
    drug_step1 <- drugName %>%
      filter(drug_name %in% list.freq.dr$drug_name) %>%
      filter(drug_first_day >= input$demo_slider_cohortStartDate_quryRange_min & drug_first_day <= input$demo_slider_cohortStartDate_quryRange_max) %>%
      group_by(subject_id) %>%
      mutate(drug_rank=rank(drug_first_day))
    
    drugRank_order <- unique(subset(drug_step1, select=c('subject_id','drug_rank'))) %>%
      arrange(drug_rank) %>%
      mutate(drug_order = seq(1,n()))
    
    drug_step1 <- drug_step1 %>%
      merge(drugRank_order, by=c('subject_id','drug_rank'), all.x=T) %>%
      filter(drug_order <= input$demo_slider_step_max) %>%
      mutate(step = paste0('Step_', sprintf('%02d', drug_order))) %>%
      mutate(drug_name = gsub(';.*','',drug_name))
    
    return(drug_step1)
  }
  
  demo_pathway_DB <- function() {
    if(length(input$demo_cohort_difinition_DT_rows_selected) > 0){
      demo_drug_DB()
      drug <- readRDS("./demo_drug.rds")
      
      if(input$demo_radio_scenario == 1)
        conceptCode_paste <- '^B01A'
      
      else if(input$demo_radio_scenario == 2)
        conceptCode_paste <- '^L04|^M01|^A07EC'
      
      conceptCode <- drug %>% filter(grepl(conceptCode_paste, concept_code, ignore.case = F))
      drugName <- unique(subset(conceptCode, select='drug_name'))
      drugName <- drug[drug$drug_name %in% drugName$drug_name,]
      
      drug_step1 <- demo_drug_step1(drugName)
      drug_step2 <- drug_step2(drug_step1)
      
      drug.pathway <- reshape2::dcast(drug_step2, subject_id~step, value.var='node_name')
      coln <- colnames(drug.pathway)[-1]
      
      drug.pathway <- drug.pathway %>%
        group_by(across(all_of(coln))) %>%
        summarise(count=n(), perc=paste(round(count/nrow(drug_step1)*100,1),'%',sep='')) %>%
        arrange(-count) %>%
        relocate(count, perc)
      
      return(drug.pathway)
    }
  }
  
  demo_drug_step2_DB <- function() {
    if(length(input$demo_cohort_difinition_DT_rows_selected) > 0){
      demo_drug_DB()
      drug <- readRDS("./demo_drug.rds")
      
      if(input$demo_radio_scenario == 1)
        conceptCode_paste <- '^B01A'
      
      else if(input$demo_radio_scenario == 2)
        conceptCode_paste <- '^L04|^M01|^A07EC'
      
      conceptCode <- drug %>% filter(grepl(conceptCode_paste, concept_code, ignore.case = F))
      drugName <- unique(subset(conceptCode, select='drug_name'))
      drugName <- drug[drug$drug_name %in% drugName$drug_name,]
      
      drug_step1 <- demo_drug_step1(drugName)
      drug_step2 <- drug_step2(drug_step1)
      
      return(drug_step2)
    }
  }
  
  output$demo_sunburst <- renderSund2b({
    sund2b(demo_sunburst_DB())
  })
  
  demo_sunburst_DB <- eventReactive(input$demo_button_pathway_visual, {
    if(length(input$demo_pathway_DT_rows_all) >= 1){
      if(length(input$demo_pathway_DT_rows_selected) >= 1){
        dataOfConverter <- demo_pathway_DB()[input$demo_pathway_DT_rows_selected, ]
      }
      else{
        dataOfConverter <- demo_pathway_DB()[input$demo_pathway_DT_rows_all, ]
      }
      dataOfConverter$column <- apply(dataOfConverter[,-c(1,2)], 1, paste, collapse='-')
      dataOfConverter <- dataOfConverter[c("column","count")]
      dataOfConverter$column <- gsub("-NA", "", dataOfConverter$column)
      
      return(dataOfConverter)
    }
  })
  
  output$demo_sankeyNetwork <- renderSankeyNetwork({
    demo_sankeyNetwork_DB()
  })
  
  demo_sankeyNetwork_DB <- eventReactive(input$demo_button_pathway_visual, {
    if(length(input$demo_pathway_DT_rows_all) >= 1){
      drug <- readRDS("./demo_drug.rds")
      
      if(input$demo_radio_scenario == 1){
        conceptCode_paste <- '^B01A'
        title <- 'Scenario 1 : Atrial fibrillation (ATC filter = [B01A])'
      }
      
      else if(input$demo_radio_scenario == 2){
        conceptCode_paste <- '^L04|^M01|^A07EC'
        title <- 'Scenario 2 : Ankylosing spondylitis (ATC filter = [L04, M01, A07EC])'
      }
      
      conceptCode <- drug %>% filter(grepl(conceptCode_paste, concept_code, ignore.case = F))
      drugName <- unique(subset(conceptCode, select='drug_name'))
      drugName <- drug[drug$drug_name %in% drugName$drug_name,]
      
      drug_step3 <- drug_step3(drug_step2(demo_drug_step1(drugName)))
      
      pwdata.drug <- conv_link(drug_step3, input$demo_slider_link_min)
      
      sankey <- sankeyNetwork(Links = pwdata.drug$link, Nodes = pwdata.drug$node, Source = "source",
                              Target = "target", Value = "value", NodeID = "name",
                              units = "건", fontSize = 15, nodeWidth = 30)
      
      cohort_difinition_selected <- cohort_difinition_DB()[input$demo_cohort_difinition_DT_rows_selected,]
      cohort_difinition_selected <- subset(cohort_difinition_selected, select="name")
      
      title <- paste(title, '-', cohort_difinition_selected)
      sankey <- htmlwidgets::prependContent(sankey, htmltools::tags$h1(title))
      
      return(sankey)
    }
  })
  
  output$cohort_difinition_DT <- DT::renderDataTable(
    DT::datatable(
      button_cohort_difinition_DB(), selection = 'single',
      filter = list(position = 'top', clear = FALSE), options = list(pageLength = 5, order = list(3, 'desc')) 
    )
  )
  
  button_cohort_difinition_DB <- eventReactive(input$button_DB_cohort, {
    if(length(input$text_cohortdefinitionschema) > 0){
      cohort_difinition_DB() 
    }
    else
      data.frame(id="", name="", description='', created_date='', modified_date='')
  })
  
  cohort_difinition_DB <- function(){
    try({
      conn <- DB_connect()
      
      cohortQry <- "select cohort_definition_id, name, created_date, modified_date, count(subject_id) 
                    from @cohortdefinitionschema.cohort_definition cd, @cohortschema.cohort ch 
                    where cd.id = ch.cohort_definition_id 
                    group by ch.cohort_definition_id, name, created_date, modified_date;"
      
      cohortQry <- render(cohortQry, cohortdefinitionschema = input$text_cohortdefinitionschema,
                          cohortschema = input$text_cohortschema)
      cohort_difinition <- unique(dbGetQuery(conn, cohortQry))
      dbDisconnect(conn)
      cohort_difinition <- cohort_difinition[,c('cohort_definition_id', 'name', 'created_date', 'modified_date', 'count')]
      colnames(cohort_difinition) <- c('cohort_definition_id', 'name', 'created_date', 'modified_date', 'subject_count')
      
      return(cohort_difinition)
    })
  }
  
  drug_DB <- function() {
    cohort_difinition_selected <- cohort_difinition_DB()[input$cohort_difinition_DT_rows_selected,]
    cohort_difinition_selected <- subset(cohort_difinition_selected, select="cohort_definition_id")
    
    conn <- DB_connect()
    
    sql_drug<-"SELECT subject_id, vocabulary_id, concept_class_id, concept_code, ancestor_concept_id, concept_name as drug_name,  
            (min(drug_exposure_start_date) - cohort_start_date) as drug_first_day
            FROM @cdmschema.drug_exposure de, @cdmschema.concept c, @cohortschema.cohort ch, @cdmschema.concept_ancestor ca
            WHERE de.person_id = ch.subject_id
              AND ch.cohort_definition_id = @cohort_definition_id
              AND ca.descendant_concept_id=de.drug_concept_id
              AND c.concept_id=ca.ancestor_concept_id
              AND c.vocabulary_id in ('ATC')
              AND de.drug_exposure_start_date between ch.cohort_start_date + @before AND ch.cohort_start_date + @after
            Group by subject_id, cohort_start_date, vocabulary_id, concept_class_id, concept_code, ancestor_concept_id, concept_name"
    
    sql_drug <- render(sql_drug, cdmschema = input$text_cdmschema, cohortschema = input$text_cohortschema, 
                       cohort_definition_id = cohort_difinition_selected, 
                       before=input$slider_cohortStartDate_sqlRange_min, 
                       after=input$slider_cohortStartDate_sqlRange_max)
    drug <- dbGetQuery(conn, sql_drug) 
    dbDisconnect(conn)
    
    saveRDS(drug,'./drug.rds')
  }
  
  output$conceptClass_DT <- DT::renderDataTable({
    DT::datatable(
      button_conceptClass_DB(),
      filter = list(position = 'top', clear = FALSE),
      options = list(search = list(regex = TRUE), pageLength = 5)
    )
  })
  
  button_conceptClass_DB <- eventReactive(input$button_cohort_conceptClass, {
    if(length(input$cohort_difinition_DT_rows_all) > 0){
      drug_DB()
      conceptClass_DB() 
    }
    else
      data.frame(concept_class_id="")
  })
  
  conceptClass_DB <- function(){
    if(length(input$cohort_difinition_DT_rows_selected) >= 1){
      drug <- readRDS("./drug.rds")
      conceptClass <- unique(subset(drug, select='concept_class_id'))
      
      return(conceptClass)
    }
  }
  
  output$conceptCode_DT <- DT::renderDataTable({
    DT::datatable(
      button_conceptCode_DB(),
      filter = list(position = 'top', clear = FALSE),
      options = list(search = list(regex = TRUE), pageLength = 5)
    )
  })
  
  button_conceptCode_DB <- eventReactive(input$button_conceptClass_conceptCode, {
    if(length(input$conceptClass_DT_rows_all) > 0)
      conceptCode_DB() 
    
    else
      data.frame(concept_code="", drug_name='')
  })
  
  conceptCode_DB <- function(){
    if(length(input$conceptClass_DT_rows_all) > 0){
      drug <- readRDS("./drug.rds")
      
      if(length(input$conceptClass_DT_rows_selected) >= 1){
        conceptClass <- drug[drug$concept_class_id %in% conceptClass_DB()[input$conceptClass_DT_rows_selected, "concept_class_id"],]
        conceptCode <- unique(subset(conceptClass, select = c('concept_code', 'drug_name')))
      }
      else{
        conceptClass <- drug[drug$concept_class_id %in% conceptClass_DB()[input$conceptClass_DT_rows_all, "concept_class_id"],]
        conceptCode <- unique(subset(conceptClass, select = c('concept_code', 'drug_name')))
      }
      return(conceptCode)
    }
  }
  
  output$drugName_DT <- DT::renderDataTable({
    DT::datatable(
      button_drugName_DB(),
      filter = list(position = 'top', clear = FALSE),
      options = list(search = list(regex = TRUE), pageLength = 5)
    )
  })
  
  button_drugName_DB <- eventReactive(input$button_conceptCode_drugName, {
    if(length(input$conceptCode_DT_rows_all) > 0)
      drugName_DB() 
    
    else
      data.frame(drug_name="")
  })
  
  drugName_DB <- function(){
    if(length(input$conceptCode_DT_rows_all) > 0){
      drug <- readRDS("./drug.rds")
      
      if(length(input$conceptCode_DT_rows_selected) >= 1){                                                
        conceptClass <- drug[drug$vocabulary_id %in% 'ATC',]
        conceptCode_paste <- paste0("^", conceptCode_DB()[input$conceptCode_DT_rows_selected, "concept_code"], collapse = "|")
        conceptCode <- conceptClass %>% filter(grepl(conceptCode_paste, concept_code, ignore.case = F))
        conceptCode <- unique(subset(conceptCode, select='drug_name'))
        
        return(conceptCode)
      }
    }
  }
  
  output$pathway_DT <- DT::renderDataTable({
    DT::datatable(
      button_pathway_DB(),
      filter = list(position = 'top', clear = FALSE),
      options = list(search = list(regex = TRUE), pageLength = 5)
    )
  })
  
  button_pathway_DB <- eventReactive(input$button_drugname_pathway, {
    if(length(input$drugName_DT_rows_all) > 0)
      pathway_DB() 
    
    else
      data.frame(count="", perc="", first="", second="")
  })
  
  drug_step1 <- function(drugName) {
    if(input$radio_pathway == '1')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 1st",])
    else if(input$radio_pathway == '2')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 2nd",])
    else if(input$radio_pathway == '3')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 3rd",])
    else if(input$radio_pathway == '4')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 4th",])
    else if(input$radio_pathway == '5')
      drugName <- unique(drugName[drugName$concept_class_id == "ATC 5th",])
    
    no.person <- length(unique(drugName$subject_id))
    
    list.freq.dr <- drugName %>%
      filter(drug_first_day >= 0) %>%
      group_by(drug_name) %>%
      summarise(count.person=length(unique(subject_id)), perc=round(count.person/no.person*100,1)) %>%
      filter(perc >= input$slider_usedDrug_min)
    
    drug_step1 <- drugName %>%
      filter(drug_name %in% list.freq.dr$drug_name) %>%
      filter(drug_first_day >= input$slider_cohortStartDate_quryRange_min & drug_first_day <= input$slider_cohortStartDate_quryRange_max) %>%
      group_by(subject_id) %>%
      mutate(drug_rank=rank(drug_first_day))
    
    drugRank_order <- unique(subset(drug_step1,select=c('subject_id','drug_rank'))) %>%
      arrange(drug_rank) %>%
      mutate(drug_order=seq(1,n()))
    
    drug_step1 <- drug_step1 %>%
      merge(drugRank_order, by=c('subject_id','drug_rank'), all.x=T) %>%
      filter(drug_order <= input$slider_step_max) %>%
      mutate(step = paste0('Step_', sprintf('%02d', drug_order))) %>%
      mutate(drug_name = gsub(';.*','',drug_name))
    
    return(drug_step1)
  }
  
  drug_step2 <- function(drug_step1){
    drug_step2 <- drug_step1 %>%
      group_by(subject_id, drug_order) %>%
      arrange(drug_name) %>%
      mutate(node_name=paste(drug_name,collapse=' + ')) %>%
      select(subject_id, node_name, drug_order, step) %>%
      unique() %>%
      as.data.frame() 
    
    return(drug_step2)
  }
  
  drug_step3 <- function(drug_step2) {
    drug_step3 <- drug_step2 %>%
      mutate(node_name=paste0(node_name, '_', drug_order)) %>%
      reshape2::dcast(subject_id~step, value.var='node_name')
    drug_step3$Step_02 <- apply(drug_step3[,c('Step_01','Step_02')],1,function(x)ifelse(is.na(x[2]),'NA',x[2]))
    
    return(drug_step3)
  }
  
  pathway_DB <- function() {
    if(length(input$drugName_DT_rows_all) > 0){
      drug <- readRDS("./drug.rds")
      
      if(length(input$drugName_DT_rows_selected) >= 1){
        drugName <- drug[drug$drug_name %in% drugName_DB()[input$drugName_DT_rows_selected, "drug_name"],]
      }
      else{
        drugName <- drug[drug$drug_name %in% drugName_DB()[input$drugName_DT_rows_all, "drug_name"],]
      }
      
      drug_step1 <- drug_step1(drugName)
      drug_step2 <- drug_step2(drug_step1)
      
      drug.pathway <- reshape2::dcast(drug_step2, subject_id~step, value.var='node_name')
      coln <- colnames(drug.pathway)[-1]
      
      drug.pathway <- drug.pathway %>%
        group_by(across(all_of(coln))) %>%
        summarise(count=n(), perc=paste(round(count/nrow(drug_step1)*100,1),'%',sep='')) %>%
        arrange(-count) %>%
        relocate(count, perc)
      
      return(drug.pathway)
    }
  }
  
  drug_step2_DB <- function() {
    if(length(input$drugName_DT_rows_all) > 0){
      drug <- readRDS("./drug.rds")
      
      if(length(input$drugName_DT_rows_selected) >= 1){
        drugName <- drug[drug$drug_name %in% drugName_DB()[input$drugName_DT_rows_selected, "drug_name"],]
      }
      else{
        drugName <- drug[drug$drug_name %in% drugName_DB()[input$drugName_DT_rows_all, "drug_name"],]
      }
      
      drug_step1 <- drug_step1(drugName)
      drug_step2 <- drug_step2(drug_step1)
      
      return(drug_step2)
    }
  }
  
  output$sunburst <- renderSund2b({
    sund2b(sunburst_DB())
  })
  
  sunburst_DB <- eventReactive(input$button_pathway_visual, {
    if(length(input$pathway_DT_rows_all) >= 1){
      if(length(input$pathway_DT_rows_selected) >= 1){
        dataOfConverter <- pathway_DB()[input$pathway_DT_rows_selected, ]
      }
      else{
        dataOfConverter <- pathway_DB()[input$pathway_DT_rows_all, ]
      }
      dataOfConverter$column <- apply(dataOfConverter[,-c(1,2)], 1, paste, collapse='-')
      dataOfConverter <- dataOfConverter[c("column","count")]
      dataOfConverter$column <- gsub("-NA", "", dataOfConverter$column)
      
      return(dataOfConverter)
    }
  })
  
  conv_link <- function(df,value.threshold){
    l <- data.frame()
    coln <- colnames(df)
    coln.step <- sort(coln[grepl('step_', coln, ignore.case=T)])
    step.n <- length(coln.step)
    for(i in 1:(step.n-1)){
      ab <- data.frame(s=df[,coln.step[i]], t=df[,coln.step[i+1]], stringsAsFactors = F)
      if(is.null(l)|nrow(l)==0|length(l)==0){
        l <- ab
      }
      else{
        l <- rbind(l,ab)
      }
    }
    
    link <- l %>%
      group_by(s,t) %>%
      summarise(value= n()) %>%
      na.omit(link) %>%
      filter(value >= value.threshold)
    
    node <- data.frame(node_name=unique(c(link$s,link$t))) %>%
      mutate(node=seq(1,length(node_name))-1, name=gsub('_.*','',node_name))
    node[node$node_name=='NA', 'name'] <- '▷(continue)'
    
    link <- link %>% 
      merge(node, by.x='s',by.y='node_name',all.x=T) %>%
      merge(node, by.x='t',by.y='node_name',all.x=T) %>%
      select(node.x, node.y, value)
    
    colnames(link) <- c('source', 'target', 'value')
    
    return(list(link=link, node=node))
  }
  
  output$sankeyNetwork <- renderSankeyNetwork({
    sankeyNetwork_DB()
  })
  
  sankeyNetwork_DB <- eventReactive(input$button_pathway_visual, {
    if(length(input$pathway_DT_rows_all) >= 1){
      drug <- readRDS("./drug.rds")
      
      if(length(input$drugName_DT_rows_selected) >= 1){
        drugName <- drug[drug$drug_name %in% drugName_DB()[input$drugName_DT_rows_selected, "drug_name"],]
      }
      else{
        drugName <- drug[drug$drug_name %in% drugName_DB()[input$drugName_DT_rows_all, "drug_name"],]
      }
      
      drug_step3 <- drug_step3(drug_step2(drug_step1(drugName)))
      
      pwdata.drug <- conv_link(drug_step3, input$slider_link_min)
      
      sankey <- sankeyNetwork(Links = pwdata.drug$link, Nodes = pwdata.drug$node, Source = "source",
                              Target = "target", Value = "value", NodeID = "name",
                              units = "건", fontSize = 15, nodeWidth = 30)
      
      cohort_difinition_selected <- cohort_difinition_DB()[input$cohort_difinition_DT_rows_selected,]
      cohort_difinition_selected <- subset(cohort_difinition_selected, select="name")
      
      sankey <- htmlwidgets::prependContent(sankey, htmltools::tags$h1(cohort_difinition_selected))
      
      return(sankey)
    }
  })
  
  output$demo_sankeyNetwork_html <- downloadHandler(
    filename = function() {
      paste("demo_sankeyNetwork_", Sys.Date(), ".html", sep = "")
    },
    
    content = function(file) {
      saveNetwork(demo_sankeyNetwork_DB(), file)
    }
  )
  
  output$sankeyNetwork_html <- downloadHandler(
    filename = function() {
      paste("sankeyNetwork_", Sys.Date(), ".html", sep = "")
    },
    
    content = function(file) {
      saveNetwork(sankeyNetwork_DB(), file)
    }
  )
  
}

shinyApp(ui, server)
