library(RColorBrewer)
library(ggsci)
library(ggrepel)

mycolors <- unique(c(brewer.pal(name="Dark2", n = 8), 
  brewer.pal(name="Accent", n = 7), brewer.pal(name="Set1", n = 9), 
  brewer.pal(name="Set3", n = 12), brewer.pal(name="Set2", n = 8), 
  brewer.pal(name="Paired", n = 5), brewer.pal(name="Paired", n = 10)[7:10], 
  brewer.pal(name="Pastel1", n = 9), brewer.pal(name="Pastel2", n = 7)))

# install.packages("shinythemes")
# install.packages("shinydashboard")
library(shinythemes)
library(DT)
# devtools::install_github("RinteRface/bs4Dash")
# library(Seurat)

library(bs4Dash)
library(ggplot2)
# func --------------------------------------------------------------------

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {

  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
  }

###################################################################################
ui <- dashboardPage(
  dashboardHeader(title = "Cancer_database"),
  dashboardSidebar(skin = "light", childIndent = FALSE,
   
   sidebarMenu(sidebarHeader(title = 'The imput of Panel'),

    menuSubItem(fluidRow(column(width = 12,
      textInput("gene",'search for a gene',value = 'CD3D')),
                        column(width = 12,
      submitButton('search',icon("search"))))),

    menuSubItem(
       column(
        width = 6,
        tags$style(".container { border:10px solid steelblue; width: 100%; height: 200px; overflow-y: scroll; }"),
        dropdownButton(
          label = "Select the cell types", status = "default", width = 220,
          tags$div(
            class = "container",
            checkboxGroupInput(inputId = "check", label = "Choose", 
              choices = c("Ex","Inhib","Mix","Oligos","Astros","Micro_Macro","OPCs","Endo"), 
              selected = c("Ex","Inhib","Mix","Oligos","Astros","Micro_Macro","OPCs","Endo"))
           )
          )
        )
     )
   )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = 'info', helpText('the information of MDD'), status = "info", width = 6),
      infoBox(h3(34), "Samples", icon = icon("solid fa-users"), color = 'lightblue', fill = TRUE, width = 3),
      infoBox(h3(78886), "Cells", icon = icon("solid fa-chart-pie"), color = 'orange', fill = TRUE, width = 3),
      ),
     fluidRow(
      # box(, width = 3),
      box(title = 'Box plot of gene exp', status = "primary", solidHeader = TRUE, collapsible = FALSE,
        plotOutput("plot1", height = 250), 
        downloadButton('downloadplot1', 'Download'),width = 6),
      box(title = 'tSNE plot of MMD', status = "warning", solidHeader = TRUE, collapsible = FALSE,
        plotOutput("plot2", height = 250), 
        downloadButton('downloadplot2', 'Download'),width = 6),

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
    dat.all <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('MajorType',input$gene)]
    dat.all$colours <- dat.all[,input$gene]
    ggplot(dat.all) + geom_boxplot(aes(MajorType, colours, color = MajorType)) + 
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
      dat.all <- datasetInput()[which(datasetInput()$MajorType %in% input$check),c('MajorType',input$gene)]
      dat.all$colours <- dat.all[,input$gene]
      pdf(file, width=6, height=5)
      p1 <- ggplot(dat.all) + geom_boxplot(aes(MajorType, colours, color = MajorType)) + 
      labs(x='', y='', title='Boxplot') +
      scale_fill_manual(values = mycolors) +
      theme_classic() + 
      theme(plot.title=element_text(color='black',size=15,face='bold',hjust=0.5),axis.text=element_text(size=12),axis.text.x=element_text(angle=45, hjust=1, vjust=1),legend.position='none')
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

  output$dataset <- renderDT({
    data.set <- datasetInput()[,c('res.2.5','MajorType','subtype',input$gene)]
    })
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste0('Exp_MMD', input$gene, '.csv', sep='')
    },
    content = function(file) {
      data.set <- datasetInput()[,c('res.2.5','MajorType','subtype',input$gene)]
      write.csv(data.set, file)
    }
  )
}

shinyApp(ui, server)

