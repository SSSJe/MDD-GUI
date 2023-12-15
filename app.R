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

mycolors <- unique(c(brewer.pal(name="Dark2", n = 8), 
  brewer.pal(name="Accent", n = 7), brewer.pal(name="Set1", n = 9), 
  brewer.pal(name="Set3", n = 12), brewer.pal(name="Set2", n = 8), 
  brewer.pal(name="Paired", n = 5), brewer.pal(name="Paired", n = 10)[7:10], 
  brewer.pal(name="Pastel1", n = 9), brewer.pal(name="Pastel2", n = 7)))

ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  dashboardHeader(title = "MDD_GUI"),
  dashboardSidebar(skin = "light", childIndent = TRUE, minified = FALSE, 
     
    checkboxGroupInput(inputId = "check", label = "MajorType", 
              choices = list("Excitatory Neurons",
                "Inhibitory Neurons",
                "Oligodendrocytes",
                "Astrocytes",
                "Microglia/Macrophage",
                "OPCs",
                "Endothelial"),selected = list("Excitatory Neurons","Inhibitory Neurons","Oligodendrocytes","Astrocytes","Microglia/Macrophage","OPCs","Endothelial")), 
    textInput("gene",'Search for A Gene',value = 'PCP4'),
    column(12, submitButton('search',icon("search")), align = 'center', style = "margin-bottom: 10px;"
        , style = "margin-top: -10px;")
  ),

  dashboardBody(

    tags$style(HTML(" 
        input:invalid { 
        background-color: #FFCCCC; 
        }")), 
    useShinyjs(), 
    bsAlert("alert"),
    textOutput('text_error'),

    fluidRow(
      box(title = 'Info', id= "box_l",
        helpText('Website for Visualizing the snRNA-seq Profile of the Brodmann Area 9 (BA9) of the Post-Mortem Brain Tissue of 17 Control Subjects and 17 Major Depressive Disorder (MDD) Cases.',
          a('(See the paper more information. PMID: 32341540)', href = "https://doi.org/10.1038/s41593-020-0621-y",target="_blank")), 
        status = "info", width = 6),
      box(title = 'The flow of the study', id= "box_r", img(src = "MDD_flow.png",style = 'width: 100%;'), status = "info", align='center'),
      spsComps::heightMatcher("box_l","box_r")
      ),
     fluidRow(
      # box(, width = 3),
      box(title = 'tSNE plot of MMD', id = 'box_t', status = "warning", 
        solidHeader = TRUE, collapsible = FALSE,
        img(src = "MDD_Majortype.png", style = 'width: 90%;'), align='center'),
      spsComps::heightMatcher("box_t", "box_v"),
      box(title = 'tSNE plot of gene exp', id = 'box_v', status = "warning", 
        solidHeader = TRUE, collapsible = FALSE,
        plotOutput("tSNE_plot"), 
        downloadButton('tSNE_download', 'Download'),width = 6),

      box(title = 'Violin plot of gene exp', status = "primary", 
        solidHeader = TRUE, collapsible = FALSE,
        plotOutput("Violin_plot", height = 250), 
        downloadButton('Violin_download', 'Download'),width = 6),
      box(title = 'Box plot of gene exp', status = "primary", solidHeader = TRUE, collapsible = FALSE,
        plotOutput("Box_plot", height = 250),
        downloadButton('Box_download', 'Download'),width = 6),

      box(title = 'DataTable',
        DTOutput("dataset"),
        downloadButton('dataset_download', 'Download'), width = 12)
      )
    )

)

server <- function(session,input, output) {
  
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

  observeEvent(input$gene, { 
     if(!(input$gene %in% colnames(datasetInput()))) { 
      alert("ERROR: The input of 'Search for A Gene' may be incorrect or absent.") 
     } 
    })
  
  output$text_error <-  renderText({
    if(!(input$gene %in% colnames(datasetInput()))) {
    shinyBS::createAlert(session, "alert", "myValueAlert", title = "Search for A Gene: Invalid input", style = "danger", content = "ERROR: The input of 'Search for A Gene' may be incorrect or absent.")
    }
    else{ 
    shinyBS::closeAlert(session, "myValueAlert") 
    }
  })

  output$tSNE_plot <- renderPlot({
    dat.all <- datasetInput()
    dat.all$colours <- datasetInput()[,input$gene]
    ggplot(dat.all, aes(tSNE_1, tSNE_2)) + geom_point(aes(color = colours)) + 
    scale_colour_gradientn(colours=viridis:: plasma(100)) + 
    labs(x='', y='', title='Scatter Plot',col = 'Gene Expression') +
    theme_classic() + 
    theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(size=12),legend.position='right')
    })
  output$tSNE_download <- downloadHandler(
    filename = function() {
      paste0('tSNE plot of MMD.pdf')
    },
    content = function(file) {
      dat.all <- datasetInput()
      dat.all$colours <- datasetInput()[,input$gene]
      pdf(file, width=6, height=5)
      p2 <- ggplot(dat.all, aes(tSNE_1, tSNE_2)) + geom_point(aes(color = colours)) + 
        scale_colour_gradientn(colours=viridis:: plasma(100)) + 
        labs(x='', y='', title='Scatter Plot',col = 'Gene Expression') +
        theme_classic() + 
        theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(size=12),legend.position='right')
      print(p2)
      dev.off()
    },contentType ="image/png"
  )

  output$Violin_plot <- renderPlot({
    dat.all <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('subtype','MajorType',input$gene)]
    dat.all$colours <- dat.all[,input$gene]
    if (length(input$check) == 1) {
        dat.all$plot_type <- dat.all[,'subtype']
      }
      else{
        dat.all$plot_type <- dat.all[,'MajorType']
      }
    ggplot(dat.all) + geom_violin(aes(plot_type, colours, color = plot_type)) + 
      labs(x='', y='', title='Violin plot') +
      scale_fill_manual(values = mycolors) +
      theme_classic() + 
      theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(size=12),axis.text.x=element_text(angle=45, hjust=1, vjust=1),legend.position='none')
    
  })
  output$Violin_download <- downloadHandler(
    filename = function() {
      paste0('cluster_Vioplot', '_', input$gene, '.pdf', sep = '')
    },
    content = function(file) {
      dat.all <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('subtype','MajorType',input$gene)]
      dat.all$colours <- dat.all[,input$gene]
      if (length(input$check) == 1) {
        dat.all$plot_type <- dat.all[,'subtype']
      }
      else{
        dat.all$plot_type <- dat.all[,'MajorType']
      }
      pdf(file, width=6, height=5)
      p2 <- ggplot(dat.all) + geom_violin(aes(plot_type, colours, color = plot_type)) + 
        labs(x='', y='', title='Violin plot') +
        scale_fill_manual(values = mycolors) +
        theme_classic() + 
        theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(size=12),axis.text.x=element_text(angle=45, hjust=1, vjust=1),legend.position='none')
      print(p2)
      dev.off()
    }
  )

    output$Box_plot <- renderPlot({
      dat.all <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('subtype','MajorType', input$gene)]
      dat.all$colours <- dat.all[,input$gene]
      if (length(input$check) == 1) {
        dat.all$plot_type <- dat.all[,'subtype']
      }
      else{
        dat.all$plot_type <- dat.all[,'MajorType']
      }
      ggplot(dat.all) + geom_boxplot(aes(plot_type, colours, color = plot_type)) + 
        labs(x='', y='', title='Boxplot') +
        scale_fill_manual(values = mycolors) +
        theme_classic() + 
        theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(  size=12),axis.text.x=element_text(angle=45, hjust=1, vjust=1),legend.position='none')
  })

  output$Box_download <- downloadHandler(
    filename = function() {
      paste0('cluster_Boxplot', '_', input$gene, '.pdf', sep = '')
    },
    content = function(file) {
      dat.all <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('subtype','MajorType',input$gene)]
      dat.all$colours <- dat.all[,input$gene]
      if (length(input$check) == 1) {
        dat.all$plot_type <- dat.all[,'subtype']
      }
      else{
        dat.all$plot_type <- dat.all[,'MajorType']
      }
      pdf(file, width=6, height=5)
      p1 <- ggplot(dat.all) + geom_boxplot(aes(plot_type, colours, color = plot_type)) + 
        labs(x='', y='', title='Boxplot') +
        scale_fill_manual(values = mycolors) +
        theme_classic() + 
        theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(size=12), axis.text.x=element_text(angle=45, hjust=1, vjust=1),legend.position='none')
      print(p1)
      dev.off()
    }
  )

  output$dataset <- renderDT({
    data.set <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('Clusters','MajorType','subtype',input$gene)]
    })
  output$dataset_download <- downloadHandler(
    filename = function() {
      paste0('Exp_MMD', input$gene, '_.csv', sep='')
    },
    content = function(file) {
      data.set <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('Clusters','MajorType','subtype',input$gene)]
      write.csv(data.set, file)
    }
  )
}

shinyApp(ui, server,option = list(launch.browser = TRUE))
