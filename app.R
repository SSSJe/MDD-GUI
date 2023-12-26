## Import R package
library(RColorBrewer)
library(ggsci)
library(ggrepel)
library(spsComps)
library(shinyjs) 
library(shinyBS)
library(shinythemes)
library(DT)
library(bs4Dash)
library(ggplot2)

## Set color
mycolors <- unique(c(brewer.pal(name = "Dark2", n = 8), 
  brewer.pal(name = "Accent", n = 7), brewer.pal(name = "Set1", n = 9), 
  brewer.pal(name = "Set3", n = 12), brewer.pal(name = "Set2", n = 8), 
  brewer.pal(name = "Paired", n = 5), brewer.pal(name = "Paired", n = 10)[7:10], 
  brewer.pal(name = "Pastel1", n = 9), brewer.pal(name = "Pastel2", n = 7)))

## Set user interface
ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  dashboardHeader(title = "MDD_GUI"),
  dashboardSidebar(skin = "light", childIndent = TRUE, minified = FALSE,
    
    ## Set the input of cell majortype
    checkboxGroupInput(inputId = "check", label = "MajorType", 
              choices = list("Excitatory Neurons",
                "Inhibitory Neurons",
                "Oligodendrocytes",
                "Astrocytes",
                "Microglia/Macrophage",
                "Endothelial"),
              selected = list("Excitatory Neurons", "Inhibitory Neurons", "Oligodendrocytes", "Astrocytes", "Microglia/Macrophage", "Endothelial")),
    
    ## Set the input of gene
    textInput("gene",'Search for A Gene',value = 'PCP4'),
    column(12, submitButton('search',icon("search")), align = 'center', 
      style = "margin-bottom: 10px;", style = "margin-top: -10px;")
  ),

 ## Set the body of display content
  dashboardBody(
    tags$style(HTML(" 
        input:invalid { 
        background-color: #FFCCCC; 
        }")), 
    
    #### Set up shinyjs #### 
    useShinyjs(), 

    ### shinyBS ### 
    bsAlert("alert"),
    textOutput('text_error'),
    
    ## Boxes need to be put in a row (or column)
    fluidRow(

      ## Show the information of this website
      box(title = 'Info', id= "box_l",
        helpText('Website for Visualizing the snRNA-seq Profile of the Brodmann Area 9 (BA9) of the Post-Mortem Brain Tissue of 17 Control Subjects and 17 Major Depressive Disorder (MDD) Cases.',
          a('(See the paper more information. PMID: 32341540)', href = "https://doi.org/10.1038/s41593-020-0621-y",target="_blank")), 
        status = "info", width = 6),

      ## Show the flow of this study      
      box(title = 'The flow of the study', id= "box_r", img(src = "MDD_flow.png",style = 'width: 100%;'), status = "info", align='center'),
      spsComps::heightMatcher("box_l","box_r")
      ),

    fluidRow(
      
      ## Show the tSNE plot of cell majortypes 
      box(title = 'tSNE plot of MMD majortypes', id = 'box_m', status = "warning", 
        solidHeader = TRUE, collapsible = FALSE,
        img(src = "MDD_Majortype.png", style = 'width: 90%;'), align='center'),

      ## Show the tSNE plot of cell subtypes
      box(title = 'tSNE plot of MMD subtypes', id = 'box_s', status = "warning", 
        solidHeader = TRUE, collapsible = FALSE,
        img(src = "MDD_subtype.png", style = 'width: 100%;'), align='center'),
      spsComps::heightMatcher("box_m", "box_s"),
      
      ## Show the groups of cells
      box(title = 'tSNE plot of MMD groups', id = 'box_t', status = "lightblue", 
        solidHeader = TRUE, collapsible = FALSE,
        img(src = "MDD_group.png", style = 'width: 100%;'), align='center'),

      ## Location the tSNE plot of gene expression in cells
      box(title = 'tSNE plot of gene expression', id = 'box_v', status = "lightblue", 
        solidHeader = TRUE, collapsible = FALSE,
        plotOutput("tSNE_plot"),
        downloadButton('tSNE_download', 'Download'),width = 6),
      spsComps::heightMatcher("box_t", "box_v"),
      
      ## Location the violin plot of gene expression in cells
      box(title = 'Violin plot of gene expression', status = "primary", 
        solidHeader = TRUE, collapsible = FALSE,
        plotOutput("Violin_plot", height = 250), 
        downloadButton('Violin_download', 'Download'),width = 6),

      ## Location the box plot of gene expression in cells
      box(title = 'Violin plot of gene expression between case and control', status = "primary", 
        solidHeader = TRUE, collapsible = FALSE,
        plotOutput("Violin_plot_cc", height = 250),
        downloadButton('Violin_download_cc', 'Download'),width = 6),

      ## Show mean of gene expression in cell majortypes
      box(title = 'Summary of majortypes', status = 'success',
        solidHeader = TRUE, collapsible = FALSE,
        DTOutput("summary_m"), width = 6),
      
      ## Show mean of gene expression in cell subtypes
      box(title = 'Summary of subtypes', status = 'success',
        solidHeader = TRUE, collapsible = FALSE,
        DTOutput("summary_s"), width = 6),
     
      ## Show the dataset of gene expression in cells
      box(title = 'DataTable',
        DTOutput("dataset"),
        downloadButton('dataset_download', 'Download'), width = 12)
      )
    )

)

## Set server process
server <- function(session, input, output) {
  
  ## Input the expression data and meta data
  datasetInput <- reactive({
    mat.meta <- readRDS('scMDD.obj.meta.all.rds')
    mat.exp <- readRDS('scMDD.obj.exp.all.rds')
    if(input$gene %in% colnames(mat.exp))  {
      mat.sub.exp <- as.matrix(mat.exp[,input$gene])
      if(is.null(colnames(mat.sub.exp))) {
        gene_name <- input$gene
        colnames(mat.sub.exp) <- gene_name
        mat.all <- cbind(mat.meta,mat.sub.exp)
        return(mat.all)
      }
    } else{
      return(as.matrix(0,nrow = nrow(mat.exp),ncol = 1))}
  })

  ## data peocessing
  datasetplt <- reactive({
    dat.all <- datasetInput()
    index.cell <- which(datasetInput()$MajorType %in% input$check)
    dat.all$colours <- datasetInput()[,input$gene]
    dat.plot <- dat.all[index.cell,]
    return(dat.plot)
    })

  ## Display of error
  observeEvent(input$gene, { 
     if(!(input$gene %in% colnames(datasetInput()))) { 
      alert("ERROR: The input of 'Search for A Gene' may be incorrect or absent.") 
     } 
    })
  
  ## Display of error in bold
  output$text_error <- renderText({
    if(!(input$gene %in% colnames(datasetInput()))) {
    shinyBS::createAlert(session, "alert", "myValueAlert", title = "Search for A Gene: Invalid input", style = "danger", content = "ERROR: The input of 'Search for A Gene' may be incorrect or absent.")
    }
    else{ 
    shinyBS::closeAlert(session, "myValueAlert") 
    }
  })
  
  ## Show the tSNE plot of gene expression in cells
  output$tSNE_plot <- renderPlot({
    dat.all <- datasetInput()
    dat.all$colours <- datasetInput()[,input$gene]
    ggplot(dat.all, aes(tSNE_1, tSNE_2)) + geom_point(aes(color = colours)) + 
      scale_colour_gradientn(colours=viridis:: plasma(100)) + 
      labs(x = '', y = '', title = 'Scatter Plot',col = 'Gene Expression') +
      theme_classic() + 
      theme(plot.title = element_text(color = 'black',size = 15,face = 'bold',hjust = 0.5), axis.text =element_text(size = 12), legend.position = 'right')
    })

  ## Download the tSNE plot of gene expression in cells
  output$tSNE_download <- downloadHandler(
    filename = function() {
      paste0('tSNE plot of MMD.pdf')
    },
    content = function(file) {
      dat.all <- datasetInput()
      dat.all$colours <- datasetInput()[,input$gene]
      pdf(file, width = 6, height = 5)
      p1 <- ggplot(dat.all, aes(tSNE_1, tSNE_2)) + geom_point(aes(color = colours)) + 
        scale_colour_gradientn(colours=viridis:: plasma(100)) + 
        labs(x = '', y = '', title = 'Scatter Plot',col = 'Gene Expression') +
        theme_classic() + 
        theme(plot.title = element_text(color = 'black',size = 15,face = 'bold',hjust = 0.5), axis.text =element_text(size = 12), legend.position = 'right')
      print(p1)
      dev.off()
    }, contentType ="image/png"
  )

  ## Show the violin plot of gene expression in cells
  output$Violin_plot <- renderPlot({
    dat.all <- datasetplt()
    if (length(input$check) == 1) {
        dat.all$plot_type <- dat.all[,'subtype']
      }
      else{
        dat.all$plot_type <- dat.all[,'MajorType']
      }
    ggplot(dat.all) + geom_violin(aes(plot_type, colours, color = plot_type)) + 
      labs(x = '', y = '', title = 'Violin plot') +
      scale_fill_manual(values = mycolors) +
      theme_classic() + 
      theme(plot.title = element_text(color = 'black',size = 15,face = 'bold',hjust = 0.5), axis.text =element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),legend.position = 'none')
  })

  ## Download the violin plot of gene expression in cells
  output$Violin_download <- downloadHandler(
    filename = function() {
      paste0('cluster_Vioplot', '_', input$gene, '.pdf', sep = '')
    },
    content = function(file) {
      dat.all <- datasetplt()
      if (length(input$check) == 1) {
        dat.all$plot_type <- dat.all[,'subtype']
      }
      else{
        dat.all$plot_type <- dat.all[,'MajorType']
      }
      pdf(file, width = 6, height = 5)
      p2 <- ggplot(dat.all) + geom_violin(aes(plot_type, colours, color = plot_type)) + 
        labs(x = '', y = '', title = 'Violin plot') +
        scale_fill_manual(values = mycolors) +
        theme_classic() + 
        theme(plot.title = element_text(color = 'black',size = 15,face = 'bold',hjust = 0.5), axis.text =element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),legend.position = 'none')
      print(p2)
      dev.off()
    }
  )

  ## Show the box plot of gene expression in cells
  output$Violin_plot_cc <- renderPlot({
    dat.all <- datasetplt()
    if (length(input$check) == 1) {
      dat.all$plot_type <- dat.all[,'subtype']
    }
    else{
      dat.all$plot_type <- dat.all[,'MajorType']
    }
    ggplot(dat.all) + geom_violin(aes(plot_type, colours, color = Group)) + 
      labs(x = '', y = '', title = 'Violin plot') +
      scale_fill_manual(values = mycolors) +
      theme_classic() + 
      theme(plot.title = element_text(color = 'black',size = 15,face = 'bold',hjust = 0.5), axis.text =element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),legend.position = 'right')
  })

  ## Download the box plot of gene expression in cells
  output$Violin_download_cc <- downloadHandler(
    filename = function() {
      paste0('case_control_violinplot', '_', input$gene, '.pdf', sep = '')
    },
    content = function(file) {
      dat.all <- datasetplt()
      if (length(input$check) == 1) {
        dat.all$plot_type <- dat.all[,'subtype']
      }
      else{
        dat.all$plot_type <- dat.all[,'MajorType']
      }
      pdf(file, width = 6, height = 5)
      p3 <- ggplot(dat.all) + geom_violin(aes(plot_type, colours, color = Group)) + 
        labs(x = '', y = '', title = 'Violin plot') +
        scale_fill_manual(values = mycolors) +
        theme_classic() + 
        theme(plot.title = element_text(color = 'black',size = 15,face = 'bold',hjust = 0.5), axis.text =element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),legend.position = 'right')
      print(p3)
      dev.off()
    }
  )
  ## Calculate the expression of majortypes
  output$summary_m <- renderDT({
    index.m <- which(datasetInput()$MajorType %in% input$check)
    data.sub <- datasetInput()[index.m,]
    data.set <- aggregate(data.sub[,7], list(data.sub$MajorType), mean)
    datatable(data.set, colnames = c('MajorType', 'Mean'), options = list(pageLength = 6))
    })

  ## Calculate the expression of subtypes
     output$summary_s <- renderDT({
      index.m <- which(datasetInput()$MajorType %in% input$check)
      data.sub <- datasetInput()[index.m,]
      data.set <- aggregate(data.sub[,7] ~ subtype, data.sub, mean)
      datatable(data.set, colnames = c('Subtype', 'Mean'), options = list(pageLength = 6))
    })

  ## View the expression of gene in each cells
  output$dataset <- renderDT({
    index.m <- which(datasetInput()$MajorType %in% input$check)
    data.set <- datasetInput()[index.m, c('Clusters', 'MajorType', 'subtype', input$gene)]
    })

  ## Download the expression of gene in each cells
  output$dataset_download <- downloadHandler(
    filename = function() {
      paste0('Exp_MMD', input$gene, '_.csv', sep='')
    },
    content = function(file) {
      index.m <- which(datasetInput()$MajorType %in% input$check)
      data.set <- datasetInput()[index.m, c('Group', 'Clusters', 'MajorType', 'subtype', input$gene)]
      write.csv(data.set, file)
    }
  )
}

## Run shinyApp
shinyApp(ui, server,option = list(launch.browser = TRUE))
