library(RColorBrewer)
library(ggsci)
library(spsComps)
library(ggrepel)
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
    textInput("gene",'search for a gene',value = 'PCP4'),
    column(12, submitButton('search',icon("search")), align = 'center', style = "margin-bottom: 10px;"
        , style = "margin-top: -10px;")
  ),

  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = 'Info', id= "box_l",
        helpText('Website for Visualizing the snRNA-seq profile of the Brodmann Area 9 (BA9) of the Post-Mortem Brain tissue of 17 Control Subjects and 17 Major Depressive Disorder (MDD) Cases.',
          a('(See the paper more information)', href = "https://www.nature.com/articles/s41593-020-0621-y",target="_blank")), 
        status = "info", width = 6),
      box(title = 'The flow of the study', id= "box_r", img(src = "MDD_flow.png",style = 'width: 100%;'), status = "info", align='center'),
      spsComps::heightMatcher("box_l","box_r")
      ),
     fluidRow(
      box(title = 'Box plot of gene exp', status = "primary", solidHeader = TRUE, collapsible = FALSE,
        plotOutput("plot1", height = 250), 
        downloadButton('downloadplot1', 'Download'),width = 6),
      box(title = 'tSNE plot of gene exp', status = "warning", solidHeader = TRUE, collapsible = FALSE,
        plotOutput("plot2", height = 250), 
        downloadButton('downloadplot2', 'Download'),width = 6),
      box(title = 'Violin plot of gene exp', id = 'box_v', status = "primary", 
        solidHeader = TRUE, collapsible = FALSE,
        plotOutput("Violin_plot", height = 250), 
        downloadButton('downloadplot3', 'Download'),width = 6),
      box(title = 'tSNE plot of MMD', id = 'box_t', status = "warning", solidHeader = TRUE, collapsible = FALSE,
        img(src = "MDD_Majortype.png", style = 'width: 80%;'), align='center'),
      spsComps::heightMatcher("box_v", "box_t"),


      box(title = 'DataTable',
        DTOutput("dataset"),
        downloadButton('downloadData3', 'Download'), width = 12)
      )
    )

)

server <- function(input, output) {
  datasetInput <- reactive({
    mat.all <- readRDS('scMDD_dat.rds')
    })
  
  output$plot1 <- renderPlot({
    dat.all <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('subtype','MajorType',input$gene)]
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
      theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(size=12),axis.text.x=element_text(angle=45, hjust=1, vjust=1),legend.position='none')
    
  })


  output$downloadplot1 <- downloadHandler(
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


  output$plot2 <- renderPlot({
    dat.all <- datasetInput()
    dat.all$colours <- datasetInput()[,input$gene]
    ggplot(dat.all, aes(tSNE_1, tSNE_2)) + geom_point(aes(color = colours)) + 
    scale_colour_gradientn(colours=viridis:: plasma(100)) + 
    labs(x='', y='', title='Scatter Plot',col = 'Gene Expression') +
    theme_classic() + 
    theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(size=12),legend.position='right')
    })
  output$downloadplot2 <- downloadHandler(
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
  output$downloadplot3 <- downloadHandler(
    filename = function() {
      paste0('cluster_Boxplot', '_', input$gene, '.pdf', sep = '')
    },
    content = function(file) {
      dat.all <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('MajorType',input$gene)]
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
  output$tSNE_plot <- renderImage({
    
    outfile <- tempfile(fileext='MDD_subtype.png')
    # Return a list
      list(src = outfile,
           alt = "This is alternate text")
    }, deleteFile = TRUE)

  output$dataset <- renderDT({
    data.set <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('Clusters','MajorType','subtype',input$gene)]
    })
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste0('Exp_MMD', input$gene, '.csv', sep='')
    },
    content = function(file) {
      data.set <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('Clusters','MajorType','subtype',input$gene)]
      write.csv(data.set, file)
    }
  )
}

shinyApp(ui, server)


