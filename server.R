

library(shiny)
library(shinydashboard)
library(dplyr)
library(lazyeval) # reference t0 mutate_()
library(ggplot2)
library(DT)
library(data.table)
library(MASS)

library(tidyverse)
library(umap)
library(Rtsne)
library(irlba)

library(gganimate)



server <- function(input,output,session) {

  filedata <- reactive({
    #req(input$fileUpload)
    inputData <- input$fileUpload
    if (is.null(inputData))
      return(mtcars)
    inputData <- read.csv(inputData$datapath,header = TRUE)

    if(input$limitcheck=="TRUE") {
      inputData <- head(inputData,input$limitnum)
    } else if(input$limitcheck=="FALSE") {
      inputData <- inputData
    }
    
    # if(input$impute=="Remove all NA rows") {
    #   inputData <- na.omit(inputData)
    # } else if (input$impute!="Remove all NA rows") {
    #   inputData <- inputData
    # }
      
  })
  

  colswithna <- reactive({
    message('check nas')
    df <- data_set()
    if (ncol(df)==0) {return(NULL)}
    ncc <- sum(complete.cases(df))
    #browser()
    if(ncc != nrow(df)) {
    df <- df %>% 
      dplyr::select_if(function(x) any(is.na(x))) %>% 
      summarise(across(everything(),~sum(is.na(.))))
    df = as_tibble(t(df), rownames = "dimensions") 
    df <- df %>% rename(countNA=V1) 
    return(df)
    } else {
    return(NULL)
    }
    
  })

  # select variables for dimension reduction
  observe({
    req(filedata())
    #df <- select_if(filedata(),is.numeric)
    df <- filedata()
    filterVars <- colnames(df)
    updateSelectizeInput(session, "filterVars", choices = c("None",filterVars))
  })
  
  # observe choices in filter vars
  observe({
    fvar <- input$filterVars
    # build filter levels
    isolate(
    df <- filedata()
    )
    filterLevels <- levels(as.factor(df[[as.character(fvar)]]))
    # select column and put levels in choices
    updateSelectizeInput(session, "filterLevels", "Filter", choices = filterLevels)
  })
    
  # select variables for dimension reduction
  observe({
    req(filedata())
    df <- select_if(filedata(),is.numeric)
    varX <- colnames(df)
    updateSelectizeInput(session, "samplexvars", "Select variables for dimension reduction", choices = varX)
  })

  # select variable for class or target  
  observe({
    varY <- colnames(filedata())
    varY <- varY[!(varY %in% input$samplexvars)]
    updateSelectizeInput(session, "sampleyvars", "Add Target Class", choices = varY) # selected = "None"
  })
  
  # select variable for umap plotsize  
  observe({
    class <- filtered_data_set() %>% dplyr::select(input$sampleyvars)
    vars <- colnames(class)
    #varY <- varY[!(varY %in% input$samplexvars)]
    updateSelectizeInput(session, "umapsize", "Size", choices = c('default',vars)) # selected = "None"
  })

  
  # select variable for umap plot color  
  observe({
    class <- filtered_data_set() %>% dplyr::select(input$sampleyvars)
    vars <- colnames(class)
    #varY <- varY[!(varY %in% input$samplexvars)]
    updateSelectizeInput(session, "umapcolor", "Color", choices = c('default',vars)) # selected = "None"
  })
  
  # select wocos to follow  
  observe({
    df <- filtered_data_set()
    if('corporatie' %in% names(df)) {
    #class <- filtered_data_set() %>% dplyr::select('corporatie')
    vars <- levels(as.factor(df[[as.character('corporatie')]]))
    updateSelectizeInput(session, "tofollow", "Follow", choices = vars) # selected = "None"
    } else {
      updateSelectizeInput(session, "tofollow", "Follow", choices = c('none')) 
    }
    
    message('follow')
    
  })
  
  # filtered_data_set
  filtered_data_set <- reactive({
    df <- filedata()
    
    message(paste("rows start filtered data : ",nrow(df)))
    
    # apply filter
    filterVar <- input$filterVars
    filterLevels <- input$filterLevels
    #message(paste("filterVar : ", class(filterVar)))
    #message(paste("filterLevels : ",paste(filterLevels)))
    
    # only filter if at least one filter level is selected
    if(!(filterVar == "")) {
      if(!(filterVar == "None")){
        df <- df %>% 
          dplyr::mutate_(filterCol = interp(~as.factor(x), x=as.name(filterVar))) %>%
          dplyr::filter(filterCol %in% filterLevels)
      }
    }
    return(df)
  })
  
  
  # dataset for feature reduction
  # numeric only
  data_set <- reactive({
    df <- filtered_data_set()
    # select columns for data reduction
    df <- df %>%
      dplyr::select(input$samplexvars)
    
    message(paste("rows in data_set : ",nrow(df)))
    
    # impute missings
    if(input$impute!="None" & input$impute!="Remove all NA rows") {
      for (k in colnames(df)) {
        if(input$impute=="Impute with Median") {
          i <- median(df[[k]],na.rm = T)
        } else if(input$impute=="Impute with Mean") {
          i <- mean(df[[k]],na.rm = T)
        } else if(input$impute=="Impute with 0") {
          i <- 0
        }
        set(x = df, which(is.na(df[[k]])), k, i)
      }
    }
    

    if(input$scale!="None") {
      if(input$scale=="center = TRUE & scale = TRUE") {
        df <- data.frame(scale(df,center = TRUE,scale = TRUE))
      } else if(input$scale=="center = FALSE & scale = TRUE") {
        df <- data.frame(scale(df,center = FALSE,scale = TRUE))
      } else if(input$scale=="center = TRUE & scale = FALSE") {
        df <- data.frame(scale(df,center = TRUE,scale = FALSE))
      } 
    }
    
    if(input$appround=="TRUE") {
      df <- round(df,input$round)
    }

    return(df)
  })
  
  # summary processed dataset
  summpro <- reactive({
    df <- data_set()
    df <- gather(df,
                 key = "attribute",
                  value = "value")
    df_sum <- df %>% group_by(attribute) %>% summarise_all(list(mean = mean,median = median ,min = min, max = max, sd = sd),na.rm = TRUE)

     if(input$appround=="TRUE") {
       df_sum[,2:6] <- round(df_sum[,2:6],input$round)
     }

    df_na <- df %>% group_by(attribute) %>% summarise_all(c("anyNA"))
    df <- cbind(data.frame(df_sum),data.frame(df_na)[,2])
    colnames(df)[7] <- "Has NAs"

    return(df)
  })
  
  # pre view dataset
  preview <- reactive({
    req(input$dview)
    if(input$dview == "Pre-processed Dataset") {
      dt_view = filedata()
    } else if(input$dview == "Processed Dataset" & is.null(input$sampleyvars)) {
      dt_view <- data_set()
    } else if(input$dview == "Processed Dataset" & input$sampleyvars!="") {
      dt_view = data_set()
      class <- filtered_data_set() %>% dplyr::select(input$sampleyvars)
      dt_view <- cbind(dt_view,class)
    } else if(input$dview=="Summary") {
      dt_view = summpro()
    }
    as.data.table(dt_view)
  })

  
  seedrandumap <- eventReactive(input$umapseed,{
    if(input$seed=="FALSE") {
      s <- as.integer(runif(1, min = 1000, max = 9999))
    } else if(input$seed=="TRUE") {
      s <- input$useedset
    }
    
  })

  
  seedrandtsne <- eventReactive(input$tsneseed,{
    if(input$seedtsne=="FALSE") {
      s <- as.integer(runif(1, min = 1000, max = 9999))
    } else if(input$seedtsne=="TRUE") {
      s <- input$tseedset
    }
  })
  
############### UMAP ##################################
  

 
  umap_class_df <- eventReactive({
    input$umapseed
    input$animate
    1
  },{
    message('start data processing')
    df_input <- filedata()
    df <- data_set()
    seedrand <- as.integer(seedrandumap())
    
    
    custom.settings = umap.defaults
    custom.settings$n_neighbors=input$n_neighbors
    custom.settings$n_components=2
    custom.settings$metric=input$umapmetric
    custom.settings$n_epochs=input$n_epochs
    custom.settings$input="data"
    custom.settings$init="spectral"
    custom.settings$min_dist=input$min_dist
    custom.settings$set_op_mix_ratio=input$set_op_mix_ratio
    custom.settings$local_connectivity=input$local_connectivity
    custom.settings$bandwidth=input$bandwidth
    custom.settings$alpha=input$alpha
    custom.settings$gamma=input$gamma
    custom.settings$negative_sample_rate=input$negative_sample_rate
    custom.settings$a=NA
    custom.settings$b=NA
    custom.settings$spread=input$spread
    custom.settings$random_state=seedrand 
    custom.settings$transform_state=NA
    custom.settings$knn_repeats=input$knn_repeats
    custom.settings$verbose=FALSE
    custom.settings$umap_learn_args=NA
    
    df_umap <- umap(df,config=custom.settings)
    
    if(is.null(input$sampleyvars)) {
      #class <- class %>% mutate(class="None") %>% dplyr::select(class)
      df_umap_class <- data.frame(df_umap$layout)
      names(df_umap_class) <- c("x", "y")
    } else {
      classvars <- filtered_data_set() %>% dplyr::select(input$sampleyvars)
      df_umap_class <- data.frame(cbind(df_umap$layout,classvars))
      names(df_umap_class) <- c("x", "y",input$sampleyvars)
    }
    
    return(df_umap_class)
  })
  
  
  ### umap plot
  output$umap_plot = renderPlotly({
    df_umap_class <- umap_class_df()
  
    n <- nrow(df_umap_class)
    
    # set color
    if(input$umapcolor=="default"){
      df_umap_class$color <- 'blue'
      umapcolor='color'
    } else {
      umapcolor=input$umapcolor
    }
    
    # set size
    if(input$umapsize=="default"){
      df_umap_class$size <- 1
      umapsize='size'
    } else {
      umapsize=input$umapsize
    }

    umap_plot <- NULL
    
    umap_plot <- ggplot(df_umap_class, aes(x, y)) + 
      geom_point(aes_string(colour = umapcolor,
                     size = umapsize, alpha=0.6)) +
      scale_size(name=umapsize) + #range = c(.1, 24),
      labs(title = "UMAP Dimension Reduction",
           x = 'X',
           y = 'Y',
           caption="powered by DIKW Academy")
    

    # add or remove legend
    if(input$showlegend=='FALSE') {
      umap_plot <- umap_plot + 
        theme(legend.position = "none")
    } else {
      umap_plot <- umap_plot +
        theme(legend.key = element_blank(),
              legend.text =element_text(size = 10),
              legend.position = "right",
              legend.direction = "vertical")
    }

    return(ggplotly(umap_plot))
  }
  )

  ### animate plot
  output$animate_plot = renderPlot({
    df <- umap_class_df()
    message('start making plot')
    n <- nrow(df)
    
    if ("jaar" %in% colnames(df)) {
    
    aplot <- ggplot(df, 
                aes(x=x, y=y, colour=corporatie)) +
      geom_text(data = df,aes(x=0,y=0, label=as.character(jaar)), size=50, colour='white',show.legend = FALSE) +
      geom_point(show.legend = FALSE, alpha= 0.7)  +
      geom_label(data = subset(df,corporatie %in% input$tofollow),
                 aes(label=corporatie),
                 show.legend = FALSE) + 
      labs(title = "AEDES Benchmark 2018 - 2021",
           subtitle = "UMAP embedding van geselecteerde Benchmark detail informatie in 2D",
           caption = "powered by DIKW Academy") +
      transition_states(jaar, transition_length = 4, state_length=1, wrap=FALSE) 

    
    return(aplot)
    } else {
      return()
    }
    
  })
    
    
#################### t-sne #######################################
  
  
  tsne_class_df <- eventReactive(input$tsneseed,{
    df_input <- filedata()
    df <- data_set()

    seedrand <- as.integer(seedrandtsne()) 
    
   
    set.seed(seedrand)
    
    df_tsne <- Rtsne(
                df, 
                dims = 2, 
                initial_dims = input$initial_dims,
                perplexity = input$perplexity, 
                theta = input$theta, 
                check_duplicates = FALSE,
                pca = input$pca, 
                partial_pca = FALSE, #input$partial_pca, 
                max_iter = input$max_iter,
                verbose = getOption("verbose", FALSE), 
                is_distance = FALSE,
                Y_init = NULL, 
                pca_center = input$pca_center, 
                pca_scale = input$pca_scale,
                normalize = input$normalise, 
                momentum = input$momentum, 
                final_momentum = input$final_momentum, 
                eta = 200,
                exaggeration_factor = input$exaggeration_factor, 
                num_threads = input$num_threads)
    
    #class <- filedata() 
    class <- filtered_data_set()
    
    if(input$sampleyvars=="None") {
      class <- class %>% mutate(class="None") %>% dplyr::select(class)
    } else if(input$sampleyvars!="None") {
      class <- class %>% dplyr::select(input$sampleyvars)
    }
    
    df_tsne_class <- data.frame(cbind(df_tsne$X,df_tsne$Y))
    
    df_tsne_class <- cbind(df_tsne_class,class)
    
    names(df_tsne_class) <- c("x", "y","class")
    return(df_tsne_class)
    
    
  })
  
  tsne_clus_df <- eventReactive(input$tsneseed,{
    df <- tsne_class_df()
    seedrand <- as.integer(seedrandtsne())
    
    set.seed(seedrand)
    
    clusters <- kmeans(df[,1:2],input$tsnen_clusters,iter.max = input$tsneiter_max, nstart = input$tsnenstart, algorithm = input$tsnekmeansalg)
    clusters <- clusters$cluster
    return(clusters)
  })
  

  tsne_class_clus_df <-eventReactive(input$tsneseed,{
    tsne_class_df <- tsne_class_df()
    tsne_clus_df <- tsne_clus_df()
    
    df_tsne_class_clus <- cbind(tsne_class_df,tsne_clus_df)
    return(df_tsne_class_clus)
  })
  
  
  tsne_clus_cent_df <- eventReactive(input$tsneseed,{
    tsne_class_df <- tsne_class_df()
    
    seedrand <- as.integer(seedrandtsne())
    
    set.seed(seedrand)
    
    clusters <- kmeans(tsne_class_df[,1:2],input$tsnen_clusters,iter.max = input$tsneiter_max, nstart = input$tsnenstart, algorithm = input$tsnekmeansalg)
    tsne_clus_cent <- data.frame(clusters$centers)
    tsne_clus_cent <- rowid_to_column(tsne_clus_cent,var = "cluster")
    return(tsne_clus_cent)
  })
  
  
  dataWithtSNEClusters <- eventReactive(input$tsneseed,{
    data <- data_set()
    class <- filedata()
    if(input$sampleyvars=="None") {
      class <- data.frame("None")
      colnames(class) <- c("Class")
    } else if(input$sampleyvars!="None") {
      class <- class %>% dplyr::select(input$sampleyvars)
    }
    
    clus <- tsne_class_clus_df()
    data <- cbind(data,class,clus$tsne_clus_df)
    
    names(data)[names(data) == "clus$tsne_clus_df"] <- "ClusterID"
    return(data)
  })
  
  ### tsne plot
  
  output$tsneplot = renderPlot({
    df_tsne_class_clus <- tsne_class_clus_df()
    
    df_tsne_class_clus$x <- as.numeric(as.character(df_tsne_class_clus$x))
    df_tsne_class_clus$y <- as.numeric(as.character(df_tsne_class_clus$y))
    
    n <- nrow(df_tsne_class_clus)
    
    tsne_clus_cent <- tsne_clus_cent_df()
    
    tsne_plot <- NULL
    
    tsne_plot <- ggplot(df_tsne_class_clus, aes(x, y))
    
    if((is.numeric(df_tsne_class_clus$class) & input$classnum=="TRUE") | !is.numeric(df_tsne_class_clus$class)) {
      tsne_plot <- tsne_plot + geom_point(aes(colour = factor(class)))
    } else if(is.numeric(df_tsne_class_clus$class) & input$classnum=="FALSE") {
      tsne_plot <- tsne_plot + geom_point(aes(colour = class))
    }
    
    tsne_plot <- tsne_plot + labs(x ="X", y = "Y",colour=input$sampleyvars)

    if(input$custmtsnetitlecheck=="FALSE") {
      tsne_plot <- tsne_plot + ggtitle("t-SNE Dimension Reduction")
    } else if(input$custmtsnetitlecheck=="TRUE") {
      tsne_plot <- tsne_plot + ggtitle(paste0(input$custmtsnetitle))
    }
    
    
    if(input$tsnecluslabels=="TRUE") {
      tsne_plot <- tsne_plot + 
        geom_point(data=tsne_clus_cent, aes(x,y)) +
        geom_text(data=tsne_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    
    if(input$txaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + xlim(input$txmin,input$txmax) 
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + ylim(input$tymin,input$tymax)
    }
    
    
    
    if(is.numeric(df_tsne_class_clus$class) & input$classnum=="FALSE") {
      tsne_plot <- tsne_plot + scale_colour_gradientn(colours = terrain.colors(12))
    }
    
    tsne_plot <- tsne_plot +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
    
    return(tsne_plot)
    

  }
  )
  
  #### tsne plot for printing
    
  tsneplotoutput = reactive({
    df_tsne_class_clus <- tsne_class_clus_df()
    
    df_tsne_class_clus$x <- as.numeric(as.character(df_tsne_class_clus$x))
    df_tsne_class_clus$y <- as.numeric(as.character(df_tsne_class_clus$y))
    
    n <- nrow(df_tsne_class_clus)
    
    tsne_clus_cent <- tsne_clus_cent_df()
    
    tsne_plot <- NULL
    
    tsne_plot <- ggplot(df_tsne_class_clus, aes(x, y))
    
    if((is.numeric(df_tsne_class_clus$class) & input$classnum=="TRUE") | !is.numeric(df_tsne_class_clus$class)) {
      tsne_plot <- tsne_plot + geom_point(aes(colour = factor(class)))
    } else if(is.numeric(df_tsne_class_clus$class) & input$classnum=="FALSE") {
      tsne_plot <- tsne_plot + geom_point(aes(colour = class))
    }
    
    tsne_plot <- tsne_plot + labs(x ="X", y = "Y",colour=input$sampleyvars)
    
    if(input$custmtsnetitlecheck=="FALSE") {
      tsne_plot <- tsne_plot + ggtitle("t-SNE Dimension Reduction")
    } else if(input$custmtsnetitlecheck=="TRUE") {
      tsne_plot <- tsne_plot + ggtitle(paste0(input$custmtsnetitle))
    }
    
    
    if(input$tsnecluslabels=="TRUE") {
      tsne_plot <- tsne_plot + 
        geom_point(data=tsne_clus_cent, aes(x,y)) +
        geom_text(data=tsne_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    
    if(input$txaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + xlim(input$txmin,input$txmax)
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + ylim(input$tymin,input$tymax)
    }
    
    if(input$txaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + xlim(input$txmin,input$txmax)
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + ylim(input$tymin,input$tymax)
    }
    
    
    
    if(is.numeric(df_tsne_class_clus$class) & input$classnum=="FALSE") {
      tsne_plot <- tsne_plot + scale_colour_gradientn(colours = terrain.colors(12))
    }
    
    tsne_plot <- tsne_plot +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
    
    return(tsne_plot)
    
  })
  
  ### tsne cluster
  
  output$tsnecluster = renderPlot({
    df_tsne_class_clus <- tsne_class_clus_df()

    df_tsne_class_clus$x <- as.numeric(as.character(df_tsne_class_clus$x))
    df_tsne_class_clus$y <- as.numeric(as.character(df_tsne_class_clus$y))

    tsne_clus_cent <- tsne_clus_cent_df()
    
    tsne_clus <- NULL

    tsne_clus <- ggplot(df_tsne_class_clus, aes(x, y)) +
      geom_point(aes(colour = factor(tsne_clus_df))) +
      labs(x ="X", y = "Y",colour="Cluster")
    
    
    if(input$txaxisbounds=="TRUE") {
      tsne_clus <- tsne_clus + xlim(input$txmin,input$txmax) #xlim(-20,15) #xlim(input$uxmin,input$uxmax)
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_clus <- tsne_clus + ylim(input$tymin,input$tymax)
    }
    
    if(input$custmtsneclustitlecheck=="FALSE") {
      tsne_clus <- tsne_clus + ggtitle("t-SNE Dimension Reduction with KMeans Clusters")
    } else if(input$custmtsneclustitlecheck=="TRUE") {
      tsne_clus <- tsne_clus + ggtitle(paste0(input$custmtsneclustitle))
    }

    if(input$tsnecluslabels=="TRUE") {
      tsne_clus <- tsne_clus +
        geom_point(data=tsne_clus_cent, aes(x,y)) +
        geom_text(data=tsne_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    

    tsne_clus <- tsne_clus +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))

    return(tsne_clus)
  }
  )
  
  #### tsne cluster for printing
  
  tsneclusplotoutput = reactive({
    
    df_tsne_class_clus <- tsne_class_clus_df()
    
    df_tsne_class_clus$x <- as.numeric(as.character(df_tsne_class_clus$x))
    df_tsne_class_clus$y <- as.numeric(as.character(df_tsne_class_clus$y))
    
    tsne_clus_cent <- tsne_clus_cent_df()
    
    tsne_clus <- NULL
    
    tsne_clus <- ggplot(df_tsne_class_clus, aes(x, y)) +
      geom_point(aes(colour = factor(tsne_clus_df))) +
      labs(x ="X", y = "Y",colour="Cluster")
    
    if(input$txaxisbounds=="TRUE") {
      tsne_clus <- tsne_clus + xlim(input$txmin,input$txmax)
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_clus <- tsne_clus + ylim(input$tymin,input$tymax)
    }
    
    if(input$custmtsneclustitlecheck=="FALSE") {
      tsne_clus <- tsne_clus + ggtitle("t-SNE Dimension Reduction with KMeans Clusters")
    } else if(input$custmtsneclustitlecheck=="TRUE") {
      tsne_clus <- tsne_clus + ggtitle(paste0(input$custmtsneclustitle))
    }
    
    if(input$tsnecluslabels=="TRUE") {
      tsne_clus <- tsne_clus +
        geom_point(data=tsne_clus_cent, aes(x,y)) +
        geom_text(data=tsne_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    
    
    tsne_clus <- tsne_clus +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
    
    return(tsne_clus)
    
  })
  
#################### data table ##################################    
  
  output$data <- DT::renderDataTable(
    DT::datatable({
      data = preview()
    }
      ,options = list(pageLength = 100,scrollX = TRUE),
                  rownames = FALSE)

  )
  
  output$nrowFiledata <- renderText({
    n <- nrow(filedata())
    paste('Observations in original data file : ',n)
  })

  output$nrowDataset <- renderText({
    n <- nrow(data_set())
    paste('Observations in dataset : ',n)
  })
  
  output$printuseed <- renderPrint({
    data <- data.frame(seedrandumap())
    colnames(data) <- c("Current Seed")
    print(data)
  })
  
  output$printtseed <- renderText({
    data <- data.frame(seedrandtsne())
    colnames(data) <- c("Current Seed")
    return(data)
  })
  
  output$renderNA <- renderTable({
    cna <- colswithna()
    if(!is.null(cna)) {
      cna
    }
    else {
      data.frame("Missing data" = "No missing data")
      
    }

  })
  
  output$dataview.csv <- downloadHandler(
    filename = function(){"dataview.csv"},
    content = function(fname){
      if(input$dview == "Pre-processed Dataset") {
      write.csv(filedata(), fname,row.names = FALSE)
      } else if(input$dview == "Processed Dataset" & input$sampleyvars!="None") {
        data <- data_set()
        class <- filedata() 
        class <- class %>% dplyr::select(input$sampleyvars)
        data <- cbind(data,class)
        write.csv(data, fname,row.names = FALSE)
      } else if(input$dview == "Processed Dataset" & input$sampleyvars=="None") {
        write.csv(data_set(), fname,row.names = FALSE)
      } else if(input$dview=="Summary") {
        write.csv(summpro(), fname,row.names = FALSE)
      }
    }
  )
  
  output$umap <- DT::renderDataTable(
    DT::datatable(
        data = umap_class_df()
        ,options = list(pageLength = 100,scrollX = TRUE),
      rownames = FALSE)
    
  )
  
  output$tsne <- DT::renderDataTable(
    DT::datatable(
      data = dataWithtSNEClusters()
      ,options = list(pageLength = 100,scrollX = TRUE),
      rownames = FALSE)
    
  )
  
  output$umapdata.csv <- downloadHandler(
    filename = function(){"umapdata.csv"},
    content = function(fname){
        write.csv(umap_class_df(), fname, row.names = FALSE)
      }
  )
  
  output$tsnedata.csv <- downloadHandler(
    filename = function(){"tsnedata.csv"},
    content = function(fname){
      write.csv(dataWithtSNEClusters(), fname,row.names = FALSE)
    }
  )
  
  
}
