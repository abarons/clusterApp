

# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(mclust)
library(dbscan)


# Support Functions -------------------------------------------------------
source("clustercore.r")



# UI ----------------------------------------------------------------------

dashboardHeader<-dashboardHeader(title = "Cluster App",titleWidth="300",tags$li(a(href = 'https://github.com/abarons/clusterApp',
                                                                    img(src = "https://image.flaticon.com/icons/png/128/1051/1051326.png",
                                                                        title = "GitHub Source Code", height = "30px"),
                                                                    style = "padding-top:10px; padding-bottom:10px;"),
                                                                    class = "dropdown")
                                                         ,tags$li(a(href = 'https://it.linkedin.com/in/barone-andrea',
                                                                    img(src = "https://image.flaticon.com/icons/svg/61/61109.svg",
                                                                        title = "Contact Developer", height = "30px"),
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

                                        
                                        # KMEANS Menu Item
                                        menuItem("KMEANS", icon = icon("wrench"),
                                                     textInput("clusternumber", "Number Of Clusters", value = "2"),
                                                     checkboxInput("normalizeKMEANS", "Min - Max Normalization", FALSE),
                                                     actionButton("kmeans", "Cluster with KMEANS"),
                                                     p("Click to execute KMEANS clustering algorithm.")
                                                     ),
             
                                        
                                        # AGNES Menu Items
                                        menuItem("AGNES",icon=icon("wrench"),
                                                  selectInput("linkageFunction", "Select a Linkage Criterion",
                                                    c("ward.D","ward.D2","single","complete","average","mcquitty", "median", "centroid")
                                                    ,width = "100%"),
                                                  selectInput("distance", "Select a Distance Metric",
                                                    c("euclidean", "maximum", "manhattan", "canberra", "binary","minkowski")
                                                    ,width = "100%"),
                                                 textInput("clustertree", "Number Of Clusters", value = "2"),
                                                 checkboxInput("normalizeAGNES", "Min - Max Normalization", FALSE),
                                                  actionButton("hclust", "Cluster with AGNES"),
                                                  p("Click to execute AGNES clustering algorithm.")
                                        
                                                ),
                                        
                                        
                                        # DBSCAN Menu Items
                                        menuItem("DBSCAN",icon=icon("wrench"),
                                                 textInput("eps", "Epsilon", value = "0.1"),
                                                 textInput("minpts", "Min Points", value = "2"),
                                                 checkboxInput("normalizeDBSCAN", "Min - Max Normalization", FALSE),
                                                 actionButton("dbscan", "Cluster with DBSCAN"),
                                                 p("Click to execute DBSCAN clustering algorithm.")
                                                 
                                        ),
                                        
                                        
                                        # EMMG Menu Items
                                        menuItem("EMMG",icon=icon("wrench"),
                                                 textInput("n", "Maximum Simulated Mixtures", value = "4"),
                                                 checkboxInput("normalizeEMMG", "Min - Max Normalization", FALSE),
                                                 actionButton("emmg", "Cluster with EMMG"),
                                                 p("Click to execute EMMG clustering algorithm.")
                                                 
                                        ),
                                        
                                        
                                        # Download Menu Items
                                        menuItem("Download Results",icon=icon("fas fa-download"),
                                                 # Horizontal line ----
                                                 tags$hr(),
                                                 p("A Download link will appear as soon as"),
                                                 p("one of the algorithms is activated."),
                                                 uiOutput("download")
                                        
                                        )
                                        )
)

                                      




dashboardBody<- dashboardBody(tags$head(tags$style("section.content { overflow-y: hidden; }
                                                   .content-wrapper, .right-side {background-color:  #f8f9f9;}")),
                              
                              #First Text Box
                              box(
                                title = "How To Use Cluster App", solidHeader = TRUE,
                                collapsible = TRUE,
                                verbatimTextOutput("urlText"),
                                width=12,
                                background = "purple"
                              ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              # Horizontal line ----
                              tags$hr(),
                              # Horizontal line ----
                              tags$hr(),
                              
                              
                              #Dataset Data Table
                              box(
                                title = "Dataset", solidHeader = TRUE,
                                collapsible = TRUE,
                                DT::dataTableOutput("contents"),style = "height:300px; overflow-y: scroll;overflow-x: scroll;",
                                width=6,
                                status = "primary"
                              ),
                              
                              
                              #Summary Text Box
                              box(
                                title = "Summary Dataset", solidHeader = TRUE,
                                collapsible = TRUE,
                                verbatimTextOutput("summary"),style = "height:300px; overflow-y: scroll;overflow-x: scroll;",
                                width=6,
                                status = "primary"
                              ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              # Horizontal line ----
                              tags$hr(),
                              # Horizontal line ----
                              tags$hr(),
                              
                              
                              #Cluster Plot
                              box(
                                title = "Cluster Plot", solidHeader = TRUE,
                                collapsible = TRUE,
                                plotOutput('plot',width="100%") %>% withSpinner(color="#0dc5c1") ,
                                width=12,
                                status = "primary"
                              ),
                              
                              # Horizontal line ----
                              tags$hr(),
                              # Horizontal line ----
                              tags$hr(),
                              # Horizontal line ----
                              tags$hr(),
                              
                              
                              #Summary Text Box
                              box(
                                title = "Cluster Summary", solidHeader = TRUE,
                                collapsible = TRUE,
                                verbatimTextOutput("summarycluster"),style = "height:300px; overflow-y: scroll;overflow-x: scroll;",
                                width=12,
                                status = "primary"
                              )
                             
)





ui <- dashboardPage(skin = "purple",
                   dashboardHeader,
                   dashboardSidebar,
                   dashboardBody
)
    
  
 

# Server ------------------------------------------------------------------

server <- function(input, output,session) {
  
  output$download <- renderUI({
    if(input$kmeans==1 || input$hclust==1 || input$dbscan==1 || input$emmg==1) {
      downloadButton('downloadData', 'Download Clustering Results')
    }
  })
  
  
  output$urlText <- renderText({
    return("Try starting one of the algorithms positioned on the left sidebar, using Iris Dataset.\nOr, upload a csv file. Cluster App will automatically find the features to perform clustering\nThen,go to download panel to save results.")
  })
    
  
  dataset <- reactive({

    if(is.null(input$file1)){
      return(datasets::iris)
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
    return(df)
    
  })
  
    output$contents <- renderDataTable({
      return(dataset())
    })
    
    
    
    output$summary  <- renderPrint({ 
      return(summary(dataset()))
    })
    
    
    kmeans <- eventReactive(input$kmeans, {
      return(kmeans.output(dataset(),input$clusternumber,input$normalizeKMEANS))
    })
    
  
    hclust <- eventReactive(input$hclust, {
      return(hclust.output(dataset(),input$linkageFunction,input$distance,input$clustertree,input$normalizeAGNES))
    })
    
    
    
    dbscan <- eventReactive(input$dbscan, {
      return(dbscan.output(dataset(),input$eps,input$minpts,input$normalizeDBSCAN))
    })
    
    emmg <- eventReactive(input$emmg, {
      return(emmg.output(dataset(),input$n,input$normalizeEMMG))
    })
    
    observeEvent(input$hclust,{
      output$plot <- renderPlot({
      hclust.plot(hclust())
      })
      output$summarycluster <- renderPrint({
      hclust.summary(hclust())
    })
     output$downloadData <- downloadHandler(
        filename = function() {
          paste('HCLUST_ClusteringResults.csv', sep='')
        },
        content = function(con) {
          write.csv(hclust()$output, con,row.names = FALSE,quote=FALSE)
        }
      )
  })
    
    observeEvent(input$kmeans,{
      output$plot <- renderPlot({
        kmeans.plot(kmeans())
      })
      output$summarycluster <- renderPrint({
        kmeans.summary(kmeans())
      })
      output$downloadData <- downloadHandler(
        filename = function() {
          paste('KMEANS_ClusteringResults.csv', sep='')
        },
        content = function(con) {
          write.csv(kmeans()$output, con,row.names = FALSE,quote=FALSE)
        }
      )
    })
    
    observeEvent(input$dbscan,{
      output$plot <- renderPlot({
        dbscan.plot(dbscan())
      })
      output$summarycluster <- renderPrint({
        dbscan.summary(dbscan())
      })
      output$downloadData <- downloadHandler(
        filename = function() {
          paste('DBSCAN_ClusteringResults.csv', sep='')
        },
        content = function(con) {
          write.csv(dbscan()$output, con,row.names = FALSE,quote=FALSE)
        }
      )
    })
    
    
    observeEvent(input$emmg,{
      output$plot <- renderPlot({
        emmg.plot(emmg())
      })
      output$summarycluster <- renderPrint({
        emmg.summary(emmg())
      })
      output$downloadData <- downloadHandler(
        filename = function() {
          paste('EMMG_ClusteringResults.csv', sep='')
        },
        content = function(con) {
          write.csv(emmg()$output, con,row.names = FALSE,quote=FALSE)
        }
      )
    })
    
   
 
} 



# Run App -----------------------------------------------------------------

shinyApp(ui, server)
  