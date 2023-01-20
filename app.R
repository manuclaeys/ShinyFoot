library(shinydashboard)
library(shiny)
library("bupaR")
library("eventdataR")
library(lubridate)
library(htmlwidgets)
library(sigmajs)
library(shiny)
library(shinyjqui)
library(DiagrammeR)

ui <- dashboardPage(
  dashboardHeader(
    title = "Extraction de la process map",
    titleWidth = 450
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Présentation", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Données", tabName = "widgets", icon = icon("th")),
      menuItem("La Map", tabName = "map", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
    # Boxes need to be put in a row (or column)

  tabItems(
    # First tab content
    tabItem(tabName = "dashboard"  ,
            tabItem(tabName = "dashboard",
                    h2("Application d'exportation de process map"),
                    h3("En cours de développement. Uploader le dataset dans l'onglet Dataset et exporter la Map sur l'onglet 'Map"),
                    h3("Le fichier est sauvgardé à l'emplacement où l'application est lancée. "),
                    h4("Author : Emmanuelle Claeys, Sébastien Déjan")
            ),
            
        #   fluidRow(
        #      box(plotOutput("plot1", height = 250)),
        #      
        #     box(
        #        title = "Controls",
        #        sliderInput("slider", "Number of observations:", 1, 100, 50)
        #      )
        #    )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Chargez vos données et cliquer sur Traitement"),
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Main panel for displaying outputs ----
            

            fluidRow(
              tabsetPanel(
                type = "tabs",
                # summary tab
                tabPanel(
                  "  Select Dates and Location",
                 uiOutput("loc")
         #         uiOutput("dt"),
         #         
                )
              )
            ),
            
            
            
            mainPanel(
              # Output: Data file ----
              tableOutput("contents"),
              shiny::dataTableOutput("merged"),
        #      actionButton("go", "Go"),
              actionButton("traitement", "Traitement"),

              hr(),
              #plotOutput("plot"),
              #shinyWidgetOutput("plot","foo",width = "100%", height = "400px")

              
              column(width = 6,
                     verbatimTextOutput("plot_clickinfo")
              )
              
            )
    ),
    tabItem(tabName = "map",
            h2("Chargement de la map"),
            mainPanel(
              column(
                width = 8,
                jqui_resizable(grVizOutput("grviz"))
              ),
              actionButton("export", "export")
              
            ),
            column(width = 2,
                   verbatimTextOutput("info_expo")
            )
    )
  )
  )
)



map_function <- function(){
  library("bupaR")
  library("eventdataR")
  library(lubridate)
  
  example_log_1 = new
  example_log_1$timestamp = gsub('.{4}$', '', example_log_1$timestamp)
  example_log_1$timestamp <- paste("2017-01-01",example_log_1$timestamp, sep = ' ')
  example_log_1$timestamp  <- as.POSIXct(example_log_1$timestamp , "%m/%d/%Y %H:%M:%S")    
  example_log_1$case = 1
  j=1
  k=1
  for(i in 2:(nrow(example_log_1)-1)){
    if(example_log_1$play_pattern.name[i]!=example_log_1$play_pattern.name[i+1]){
      example_log_1$case[j:i+1]=k
      k=k+1
      j=i+1
    }
  }
  example_log_1$case[j:nrow(example_log_1)]=k+1
  
  example_log_1$activity_instance = c(1:nrow(example_log_1))
  example_log_1$status = "complete"
  
  myeventlog = example_log_1 %>% #a data.frame with the information in the table above
    eventlog(
      case_id = "case",
      activity_id = "type.name",
      activity_instance_id = "activity_instance",
      lifecycle_id = "status",
      timestamp = "timestamp",
      resource_id = "player.name"
    )
  
  myeventlog %>% mapping
  assign("new", myeventlog, envir = .GlobalEnv)
}



server <- function(input, output) {
  set.seed(122)
  #histdata <- rnorm(500)
  v<-reactiveValues(data=NULL)
  export<-reactiveValues(data=NULL)
  
  
# output$plot1 <- renderPlot({
#    data <- histdata[seq_len(input$slider)]
  #    hist(data)
  #  })
  
  #  output$contents <- renderTable({
  #    req(input$file1)
  #    df  <-  read.csv(input$file1$datapath, sep=',')
  #   assign("df", df, envir = .GlobalEnv)
  #   assign("new", df, envir = .GlobalEnv)
  #   cat("Données uploadées")

  # })
  

  
  output$merged <- renderDataTable({
    if(input$loc=="All"){
    assign("team","All", envir = .GlobalEnv)
    new = df
    map_function()
    return(head(new))
    } 
    if(is.null(new)) return()
    assign("team",input$loc, envir = .GlobalEnv)
    new = df
    map_function()
    new = new[new$team.name == input$loc,]
    assign("new", new, envir = .GlobalEnv)
    return(head(new))
  })
  
  
  observeEvent(
    input$traitement,
    {
      v$data<-df
    }
  )
  
  observeEvent(
    input$export,{export$data = 1}
  )
  
  output$info_expo <- renderPrint({
    if (is.null(export$data)){
      cat("Cliquez sur \"exporter\" pour exporter vos données\n")
      return()
    } 
    
    saveWidget(process_map(new), file=paste(getwd(),"/file.html", sep=""))
    cat("exporté !!!!! \n")
  })
  
  
    output$plot <- renderPlot(
      {
        if (is.null(v$data)) return()
     })
    
    
    output$plot_clickinfo <- renderPrint({
      if (is.null( v$data)){
        cat("Cliquez sur \"traitement\" pour charger vos données.\n")
        return()
      } 
      
      cat("Données chargées, allez sur l'onglet MAP\n")
    })
    
    
    output[["grviz"]] <- renderGrViz({
      if (is.null(v$data)) return()
#      map_function()
#      new %>%
#        process_map()
      
      if(team=="All"){
       # new = df
        map_function()
        return(process_map(new))
        
      } 
      map_function()
      new = new[new$team.name == team,]
      process_map(new)
      
    })
   
    output$loc<-renderUI({
      if (is.null(v$data)) return()
      selectInput("loc", label = h4("Choose team player"),
                  choices = c("All",unique(new$team.name)) ,selected = 1
      )
    })
    
    
    
}

shinyApp(ui, server)