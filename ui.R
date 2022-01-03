

library(shiny)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)
library(umap)
library(Rtsne)
library(DT)

library(plotly)


rdata <- c('iris','biopsy','Melanoma','Pima.te','Boston','diamonds','muscle','mtcars')

fun <- c("None","abs(x)","ceiling(x)","cos(x)","exp(x)","floor(x)","log(x)","log10(x)","sin(x)","sqrt(x)","trunc(x)","tan(x)")

dataview <- c("Pre-processed Dataset","Processed Dataset","Summary")

impute <- c("None","Remove all NA rows","Impute with Mean","Impute with Median","Impute with 0")

scale <- c("None","center = TRUE & scale = TRUE","center = FALSE & scale = TRUE","center = TRUE & scale = FALSE")

umapmetric <- c("euclidean","manhattan")

kmeansalg <- c("Hartigan-Wong","Lloyd","Forgy","MacQueen")


dbHeader <- dashboardHeader(title = "Dimension Reduction",
                            titleWidth = 250,
                            tags$li(a(href = 'https://dikw.shinyapps.io/DimensionReduction/',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.dikw.com',
                                      img(src = 'dikw-book-logo.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

ui <- dashboardPage(skin="blue",
  dbHeader,
  dashboardSidebar(width=225,
                   sidebarMenu(
                     fileInput('fileUpload', 'Select File'),
                     br(),
                     menuItem("Data Processing", tabName = "process", icon = icon("table")),
                     br(),
                     menuItem("Benchmark-animation", tabName = "benchmark-animation", icon = icon("chart-bar")),
                     br(),
                     menuItem("UMAP", tabName = "umap", icon = icon("chart-bar")),
                     br(),
                     menuItem("t-SNE", tabName = "tsne", icon = icon("chart-bar"))
                   )
  ),
  dashboardBody(
    # hk custom to have box height
    tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 100;

        $("#umap_container").height(boxHeight);
        $("#umap_plot").height(boxHeight - 20);
        
        $("#animate_container").height(boxHeight);
        $("#animate_plot").height(boxHeight - 20);
        
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
    tabItems(
      tabItem(tabName = "process",
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            
           fluidRow(   
              box(title="Data Selection",background = "light-blue",solidHeader = TRUE,width = 4,
                  hr(),
                  fluidRow(
                  column(12,
                         textOutput("nrowFiledata")
                         )),
                  fluidRow(
                  column(12,
                         h5("For datasets with a large amount of observations consider using a subset. Datasets with > 5000 observations may be slow to process, error or timeout")
                  )),
                  fluidRow(
                    column(12,
                           checkboxInput("limitcheck", "Limit observations", FALSE)
                           )),
                  fluidRow(
                    column(6,
                           numericInput("limitnum", "",2000, min = 1, max = 5000)
                  )),
                  hr(),
                  # filter
                  h5("Filter"),
                  fluidRow(
                    column(8,selectizeInput("filterVars", "Select Filter Variable", choices = NULL, multiple = FALSE))
                  ),
                  fluidRow(
                    column(8,selectizeInput("filterLevels", "Filter", choices = NULL, multiple = TRUE))
                  ),
                  fluidRow(
                    column(12,
                           textOutput("nrowDataset")
                    ))
                ),
              box(title="Variables to use",background = "light-blue",solidHeader = TRUE,width = 4,
                fluidRow(
                column(12,selectizeInput("samplexvars", "Select (numerical) variables for dimension reduction", choices = NULL, multiple = TRUE))
                ),
                fluidRow(
                column(12,selectizeInput("sampleyvars", "Select variables for additional decoration", choices = NULL, multiple = TRUE))
                ),
                fluidRow(
                column(6,checkboxInput("classnum", "Treat numeric class as factor", FALSE))
                ),
                hr(),
                fluidRow(
                  column(12,selectInput("dview", "Pre-view data set",dataview,selected="Pre-processed Dataset"))
                ),
                br(),
                fluidRow(
                  column(4,downloadButton('dataview.csv', 'Download Data View',style = "color: white;background-color: navy"))
                )
              ),
              box(title="Pre Processing",background = "light-blue",solidHeader = TRUE,width = 4,
                fluidRow(
                  column(6,selectInput("impute","How to handle Missing Values",impute,selected="None"))
                ),
                fluidRow(
                  column(12,tableOutput("renderNA"))
                ),
                fluidRow(
                  column(12,h5("After removing NA's or imputing values consider to scale the data."))
                ),
                hr(),
                fluidRow(
                  column(12,selectInput("scale", "Scale Data",scale,selected="None"))
                ),
                hr(),
                fluidRow(
                  column(4,checkboxInput("appround", "Apply rounding", FALSE)),
                  column(3,numericInput("round","",3,min = 0,max = 5))
                )
              )
                ),
           fluidRow(
             box(width = 12,
                DT::dataTableOutput('data')
             )
           )
      ),
      tabItem(tabName = "benchmark-animation",
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              fluidRow(
                column(2,
                       fluidRow(
                         box(id='anime', title = 'Animation', background = "purple",solidHeader = TRUE, width = 12,
                             fluidRow(
                               column(6,actionButton("animate", "Start animation",style = "color: white;background-color: #D35400"))
                             ),
                             fluidRow(
                               column(12,h5('Selecteer hieronder de woning corporaties die je wilt volgen in de animatie en start de animatie.'))
                             ),
                             fluidRow(
                               column(12,selectizeInput("tofollow", "Follow", choices = NULL, multiple = TRUE))
                             ),
                             hr()
                         )
                       )
                ) , # end column
                column(10,
                       box(id='animate_container',background = "purple",solidHeader = TRUE, title = 'plot', width=12,
                           fluidRow(
                             plotOutput("animate_plot", width="100%", height="100%") %>% withSpinner(type=4,color="#D35400")
                           )
                       )
                ) # end column
              )
      ),
                    
      tabItem(tabName = "umap",
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
            fluidRow(
              column(2,
                    fluidRow(
                         box(id='dim_reduction', title = 'Dimension Reduction', background = "purple",solidHeader = TRUE, width = 12,
                             fluidRow(
                               column(6,actionButton("umapseed", "Generate Plots",style = "color: white;background-color: #D35400"))
                               ),
                            fluidRow(
                             column(12,h5('Select "Generate Plots" again to run with varying parameters and either a random or set seed'))
                            ),
                            fluidRow(
                              column(12,checkboxInput("showlegend", "Show Legend", FALSE))
                            ),
                            fluidRow(
                              column(12,selectizeInput("umapsize", "Size", choices = NULL, multiple = FALSE))
                            ),
                            fluidRow(
                              column(12,selectizeInput("umapcolor", "Color", choices = NULL, multiple = FALSE))
                            ),
                            hr(),
                           h4("Downloads"),
                           br(),
                           fluidRow(
                           column(6,downloadButton('umapdata.csv', 'Data Grid',style = "color: black;background-color: #35e51d"))
                           )
                         )
                    ),
                    fluidRow(
                      box(id='parameters', title = 'UMAP parameters', background = "purple", solidHeader = FALSE, collapsible = TRUE, collapsed=T,  width = 12,
                          #h4("UMAP Parameters"),
                          #br(),
                          fluidRow(
                            column(6,numericInput("n_neighbors", "Nearest Neighbours", 15, min = 2, max = 100)),
                            column(6,numericInput("bandwidth", "Bandwidth", 1, min = 1, max = 10))
                          ),
                          fluidRow(
                            column(6,selectInput("umapmetric", "Metric",umapmetric,selected="euclidian")),
                            column(6,numericInput("alpha", "Alpha", 1, min = 1, max = 10))
                          ),
                          fluidRow(
                            column(6,numericInput("n_epochs", "Epochs", 200, min = 2, max = 1000)),
                            column(6,numericInput("gamma", "Gamma", 1, min = 1, max = 10))
                          ),
                          fluidRow(
                            column(6,numericInput("min_dist", "Min. Distance", 0.1, min = 0.01, max = 10)),
                            column(6,numericInput("negative_sample_rate", "Negative Sample Rate", 5, min = 1, max = 10))
                          ),
                          fluidRow(
                            column(6,numericInput("set_op_mix_ratio", "Set op mix ratio", 1, min = 0, max = 1)),
                            column(6,numericInput("spread", "Spread", 1, min = 1, max = 10))
                          ),
                          fluidRow(
                            column(6,numericInput("local_connectivity", "Local Connectivity", 1, min = 1, max = 10)),
                            column(6,numericInput("knn_repeats", "KNN Repeats", 1, min = 1, max = 10))
                          ),
                          hr(),
                          h4("Seeding"),
                          fluidRow(
                            column(8,textOutput("printuseed"))
                          ),
                          checkboxInput("seed", "Set Seed", FALSE),
                          fluidRow(
                            column(6,numericInput("useedset", "Seed",1234, min = 1000, max = 9999))
                          )
                      )
                    )
               ) , # end column
              column(10,
               box(id='umap_container',background = "purple",solidHeader = TRUE, title = 'plot', width=12,
                   fluidRow(
                         plotlyOutput("umap_plot", width="100%", height="100%") %>% withSpinner(type=4,color="#D35400")
                   )
               )
              ) # end column
               ),
            fluidRow(
             box(width = 12,
                 DT::dataTableOutput('umap')
             ))
      ),
      tabItem(tabName = "tsne",
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              fluidRow(
                 box(background = "green",solidHeader = TRUE,width = 3,
                    h4("t-SNE Parameters"),
                    br(),
                    fluidRow(
                      column(6,numericInput("perplexity", "Perplexity", 30, min = 10, max = 100)),
                      column(6,h5('From notes on the Rtnse function "Perplexity parameter (should not be bigger than 3 * perplexity < nrow(X) - 1"'))
                    ),
                    fluidRow(
                      column(6,numericInput("initial_dims", "Initial Dimensions", 50, min = 10, max = 100)),
                      column(6,numericInput("theta", "Theta", 0.5, min = 0, max = 1))
                    ),

                    fluidRow(
                      column(6,checkboxInput("pca", "PCA", TRUE)),
                      column(6,checkboxInput("partial_pca", "Partial PCA", FALSE))
                    ),
                    fluidRow(
                      column(6,numericInput("max_iter", "Max. Iterations", 1000, min = 500, max = 2000)),
                      column(6,checkboxInput("pca_center", "PCA Center", TRUE))#,
                    ),
                    fluidRow(
                      column(6,checkboxInput("pca_scale", "PCA Scale", FALSE)),
                      column(6,checkboxInput("normalise", "Normalise", TRUE))#,
                    ),
                    fluidRow(
                      column(6,numericInput("momentum", "Momentum",0.5, min = 0.1, max = 1)),
                      column(6,numericInput("final_momentum", "Final Momentum", 0.8, min = 0.1, max = 1))
                    ),
                    fluidRow(
                        column(6,numericInput("exaggeration_factor", "Exagg. Factor",12, min = 1, max = 20)),
                        column(6,numericInput("num_threads", "Number of Threads", 1, min = 1, max = 10))
                      ),
                    hr(),
                    h4("KMeans Parameters"),
                    br(),
                    fluidRow(
                      column(6,numericInput("tsnen_clusters", "Number of Clusters",2,min = 1, max = 100)),
                      column(6,numericInput("tsneiter_max", "Max. Iterations", 10, min = 1, max = 100))
                    ),
                    fluidRow(
                      column(6,numericInput("tsnenstart", "Random Sets", 1, min = 1, max = 10)),
                      column(6,selectInput("tsnekmeansalg","Algorithm",kmeansalg,selected="Hartigan-Wong"))
                    )
                    ),
                box(background = "green",solidHeader = TRUE,width = 2,
                    h4("Plotting"),
                    br(),
                    fluidRow(
                    column(6,actionButton("tsneseed", "Generate Plots",style = "color: white;background-color: #D35400"))
                    ),
                    fluidRow(
                    column(12,h5('Select "Generate Plots" again to run with varying parameters and either a random or set seed'))
                    ),
                    hr(),
                    h4("Seeding"),
                    fluidRow(
                      column(8,verbatimTextOutput("printtseed"))
                    ),
                      checkboxInput("seedtsne", "Set Seed", FALSE),
                    fluidRow(
                      column(6,numericInput("tseedset", "Seed",1234, min = 1000, max = 9999))
                    ),
                    hr(),
                    h4("Downloads"),
                    br(),
                    fluidRow(
                      column(6,downloadButton('tsnedata.csv', 'Data Grid',style = "color: black;background-color: #35e51d"))
                      ),
                    br(),
                    fluidRow(
                      column(3,downloadButton('tsneplot.png', 't-SNE Plot',style = "color: black;background-color: #35e51d"))
                      ),
                    br(),
                    fluidRow(
                      column(3,downloadButton('tsneclusplot.png', 'Cluster Plot',style = "color: black;background-color: #35e51d"))
                    )
                ),
                box(width = 5,
                    fluidRow(
                      box(background = "green",solidHeader = TRUE,width = 12,
                          plotOutput("tsneplot") %>% withSpinner(type=4,color="#D35400")
                      )),
                    fluidRow(
                      box(background = "green",solidHeader = TRUE,width = 12,
                          plotOutput("tsnecluster") %>% withSpinner(type=4,color="#D35400")
                      ))
                ),
                box(background = "green",solidHeader = TRUE,width = 2,
                    h4("Chart Parameters"),
                    br(),
                    checkboxInput("txaxisbounds", "Set X axis boundaries", FALSE),
                    fluidRow(
                      column(6,numericInput("txmin", "Min. X",0,min = -2000, max = 2000)),
                      column(6,numericInput("txmax", "Max. X",0, min = -2000, max = 2000))
                    ),
                    checkboxInput("tyaxisbounds", "Set Y axis boundaries", FALSE),
                    fluidRow(
                      column(6,numericInput("tymin", "Min. Y",0,min = -2000, max = 2000)),
                      column(6,numericInput("tymax", "Max. Y",0, min = -2000, max = 2000))
                    ),
                    hr(),
                    checkboxInput("tsnecluslabels", "Display Cluster Labels", TRUE),
                    hr(),
                    textInput("custmtsnetitle", "t-SNE Custom Title", "Add a custom title here"),
                    checkboxInput("custmtsnetitlecheck", "Display t-SNE Custom Title", FALSE),
                    br(),
                    textInput("custmtsneclustitle", "KMeans Cluster Custom Title", "Add a custom title here"),
                    checkboxInput("custmtsneclustitlecheck", "Display KMeans Custom Title", FALSE)
                    

                )
                ),
              fluidRow(
                box(width = 12,
                    DT::dataTableOutput('tsne')
                )
                )
      )
      )# Tab Items
      
    
  )
)