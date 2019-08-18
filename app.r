library(shiny)
library(shinydashboard)
library(shinyWidgets)

source("clustercore.r")

dashboardHeader<-dashboardHeader(title = "Clustering App",titleWidth="300",tags$li(a(href = 'https://github.com/',
                                                                    img(src = "https://image.flaticon.com/icons/png/128/1051/1051326.png",
                                                                        title = "GitHub Source Code", height = "30px"),
                                                                    style = "padding-top:10px; padding-bottom:10px;"),
                                                                    class = "dropdown")
                                                         ,tags$li(a(href = 'https://it.linkedin.com/in/barone-andrea',
                                                                    img(src = "https://image.flaticon.com/icons/svg/61/61109.svg",
                                                                        title = "Developer Linkedin Contact", height = "30px"),
                                                                    style = "padding-top:10px; padding-bottom:10px;"),
                                                                  class = "dropdown")
                                                                        )



dashboardSidebar<-  dashboardSidebar( width = 300,
                                      sidebarMenu(
                                        

                                        # Input: Select a file ----
                                        fileInput("file1", "Choose CSV File",
                                                  multiple = FALSE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv")),
                                        
                                        # Input: Checkbox if file has header ----
                                        checkboxInput("header", "Header", TRUE),
                                        
                                        # Input: Select separator ----
                                        radioButtons("sep", "Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = "\t"),
                                                     selected = ","),
                                        
                                        # Input: Select decimal ----
                                        radioButtons("decimal", "Decimal",
                                                     choices = c(Point = ".",
                                                                 Comma = ","
                                                                 ),
                                                     selected = "."),
                                        
                                        
                                        # Horizontal line ----
                                        tags$hr(),
                                        
                                            menuItem("KMEANS", icon = icon("bar-chart-o"),
                                                     textInput("clusternumber", "Number Of Cluster", value = "2"),
                                                     actionButton("kmeans", "Cluster with KMEANS"),
                                                     p("Click here to execute KMEANS clustering algorithm.")
                                                     ),
             
                                        
                                        menuItem("AGNES",icon=icon("bar-chart-o"),
                                        selectInput("linkageFunction", "Select a Linkage Criterion",
                                                    c("ward.D","ward.D2","single","complete","average","mcquitty", "median", "centroid")
                                                    ,width = "100%"),
                                        selectInput("distance", "Select a Distance Metric",
                                                    c("euclidean", "maximum", "manhattan", "canberra", "binary","minkowski")
                                                    ,width = "100%"),
                                        actionButton("hclust", "Cluster with AGNES"),
                                        p("Click to execute AGNES clustering algorithm.")
                                        
                                      )
                                      )
)

                                      




dashboardBody<- dashboardBody(
                              box(
                                title = "Dataset Top 5", solidHeader = TRUE,
                                collapsible = TRUE,
                                tableOutput("contents"),
                                width=12,
                                background = "purple"
                              ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              # Horizontal line ----
                              tags$hr(),
                              # Horizontal line ----
                              tags$hr(),
                              
                              box(
                                title = "Cluster Plot", solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput('plot',width="100%"),
                                width=12,
                                background = "black"
                              )
)




ui <- dashboardPage(skin = "purple",
                   dashboardHeader,
                   dashboardSidebar,
                   dashboardBody
)
    
  
 
  
  
server <- function(input, output) {
  
    
    output$contents <- renderTable({
      
      if(is.null(input$file1)){
        return(head(datasets::iris,5))
      }
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         dec=input$decimal
                         )
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      return(head(df,5))
      
    })
    
    kmeans <- eventReactive(input$kmeans, {
      
      if(is.null(input$file1)){
        return(kmeans.output(datasets::iris,input$clusternumber))
      }
      
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     dec = input$decimal,
                     stringsAsFactors = FALSE)
      return(kmeans.output(df,input$clusternumber))
    })
    
    
    
    hclust <- eventReactive(input$hclust, {
      
      if(is.null(input$file1)){
        return(hclust.output(datasets::iris,input$linkageFunction,input$distance,input$clusternumber))
      }

      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     dec = input$decimal,
                     stringsAsFactors = FALSE)
      return(hclust.output(df,input$linkageFunction,input$distance,input$clusternumber))
    })
    
    observeEvent(input$hclust,output$plot <- renderPlot({
      hclust.plot(hclust())
    }))
    
    observeEvent(input$kmeans,output$plot <- renderPlot({
    kmeans.plot(kmeans())
   }))
 
 
} 
    
  
shinyApp(ui, server)
  