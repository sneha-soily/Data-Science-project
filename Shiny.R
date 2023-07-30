library(shiny)
library(shinydashboard)  # for Dashboard
library(shinyWidgets)    # for radio button widgets
library(ggplot2)         # for ggplot
library(dplyr)           # select functions are covered in the library
library(DT)              # for using %>% which works as a pipe in R code
library(car)             # to generate QQ plot to test Normal distribution of data


#Button formatting function
styleButtonBlue<- function(){
  "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:#4682B4;
                        border-color: #ffffff;
                        border-width:3px;
                        height:35px;
                        width:180px;
                        font-size: 13px;"
}


#max frequency plus 8% to ylim in histogram
fngetMaxFreq <-function(x){
  H <-hist(x, plot = FALSE)
  return(round(max(H$counts)*1.08))
}


ui <- dashboardPage(
  dashboardHeader(title = "Dynamic Plots"),
  dashboardSidebar(
    width = "300px",
    # Input: Select a file ----
    fileInput("file",label = 'Select: csv. tx, excel or rds',
              multiple = FALSE,
              accept = c("text/csv/Excel",
                         "text/comma-separated-values,text/plain/excel",
                         ".csv",".txt",".xls",".xlsx",".rds")),
    # Horizontal line ----
    tags$hr(),
    column(
      width = 5,
      offset = 1,
      align = "left",
      fluidRow(
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
      )
    ),
    column(
      width = 5,
      offset = 0,
      align = "left",
      fluidRow(
        br(),
        br(),
        br(),
        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Qot." = '"',
                                 "Single Qot." = "'"),
                     selected = '"')
      )
    ),
    
    column(
      # Horizontal line ----
      tags$hr(),
      width = 12,
      align="center",
      actionButton(inputId = "mgetfileclick",label = "Get Data!",style = styleButtonBlue()),
      actionButton(inputId = "mshowplot",label = "Show Plot!",style = styleButtonBlue()),
      actionButton(inputId = "mshowtable",label = "Show Table!",style = styleButtonBlue())
    )
  ),
  
  dashboardBody(
    column(
      width = 12,
      align = "center",
      fluidRow(
        box(
          width = 4,
          height = 500,
          title = "Plot Type & Variable",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          radioGroupButtons(
            inputId = "mplottype",
            label = "Select Plot Type",
            choices = c("Box Plot", "Hist/Bar", "QQ Plot", "Density Plot"),
            individual = TRUE,
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle",
                           style = "color: red"),
              no = tags$i(class = "fa fa-circle-o",
                          style = "color: steelblue")
            )
          ),
          
          tags$hr(),
          x <- uiOutput('radio_Btns'),
          actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable",style = styleButtonBlue()),
        ),
        uiOutput(outputId = "mtableorplot")
        
      ) #fluid Row Closure
    ) #column closure
  ) # dashboardBody closure
) #dashboardpage closure






server <- function(input, output, session) {
  
  vars_mydata <- reactiveValues(mydata=NULL)
  
  observeEvent(input$file,{
    ext <- tools::file_ext(input$file$name)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    if (ext == "rds"){
      vars_mydata$mydata <- as.data.frame(readRDS(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "xls" || ext == 'xlsx'){
      vars_mydata$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else{
      tryCatch(
        {
          vars_mydata$mydata <- read.csv(input$file$datapath,
                                         header = input$header,
                                         sep = input$sep,
                                         quote = input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    }
    vars_mydata$mydata <- na.omit(vars_mydata$mydata)
    vars_mydata$mydata <- vars_mydata$mydata[complete.cases(vars_mydata$mydata), ]
  })
  
  
  observeEvent(input$mgetfileclick,{
    output$radio_Btns <- renderUI({
      options <- colnames(vars_mydata$mydata) # The options are dynamically generated on the server
      
      radioGroupButtons(
        inputId = "radioreply",
        label = "Select variable",
        choices = options,
        individual = TRUE,
        size = "sm", #size options are "xl", "sm","normal","lg"
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle",
                       style = "color: red"),
          no = tags$i(class = "fa fa-circle-o",
                      style = "color: steelblue"))
      )# radioGroupbtn closure
    }) # renderUI closure
  }) #observeevent closure
  
  
  observeEvent(input$mshowplot,{
    output$mtableorplot <- renderUI({
      box(
        width = 8,
        height = 500,
        title = textOutput("mplottitle"),
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        plotOutput('mmultiplot', height = 425)
      ) #box closure
    })    
  })
  
  
  
  observeEvent(input$mshowtable,{
    output$mtableorplot <- renderUI({
      box(
        width = 8,
        height = 500,
        title = "Data Table",
        status = "warning",
        solidHeader = TRUE,
        collapsible = FALSE,
        DT::dataTableOutput('mdatatable', height = 425)
      ) #box closure
    })    
  })
  
  
  output$mdatatable <- DT::renderDataTable({
    DT::datatable( vars_mydata$mydata,
                   rownames = FALSE,
                   width = NULL,
                   height = NULL,
                   editable = TRUE,
                   selection = list(mode = "single", selected = c(1), target = 'row'),
                   fillContainer = getOption("DT.fillContainer", TRUE),
                   options = list(
                     lengthMenu = list(c(10, 25, 50,-1), c('10', '25','50' ,'All')),
                     paging = TRUE,
                     lenthChange=TRUE,
                     searching = FALSE,
                     fixedColumns = FALSE,
                     autoWidth = FALSE,
                     ordering = FALSE
                   ),
                   
                   class ='cell-border stripe compact white-space: nowrap', #where you got this multiple classes: https://rstudio.github.io/DT/
    )
  })
  
  
  
  output$mmultiplot <-renderPlot({
    par(mfrow = c(1, 1))
    tryCatch({            
      
      switch(input$mplottype,
             "Box Plot" =  boxplot(x = vars_mydata$mydata[ ,input$radioreply],
                                   main = NULL,
                                   labels = TRUE,
                                   col = "#87CEFA",
                                   border = "black",
                                   # col = "orange",
                                   # border = "brown",
                                   notch = TRUE)+
               points(pch = 16,cex = 1.5,mean(vars_mydata$mydata[ ,input$radioreply]),col="red"),
             
             "Hist/Bar"= 
               if (is.character(vars_mydata$mydata[,input$radioreply])  == TRUE){
                 # Outside bars
                 df<- data.frame(table(vars_mydata$mydata[,input$radioreply]))
                 ggplot(data=df, aes(x=Var1, y=Freq)) +
                   geom_bar(stat="identity", fill="#87CEFA")+
                   xlab(input$radioreply) +
                   ylab("Frequency") +
                   geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
                   theme_minimal()
               }
             else{hist(vars_mydata$mydata[,input$radioreply],
                       main = NULL,
                       labels = TRUE,
                       col = "#87CEFA",
                       border = "white",
                       breaks = 5,
                       ylim = c(0,fngetMaxFreq(vars_mydata$mydata[,input$radioreply])),
                       xlab = paste("Bin of ",input$radioreply))%>%
                 abline(v = mean(vars_mydata$mydata[,input$radioreply], na.rm = T),
                        col = "red",
                        lwd = 2)%>%
                 abline(v = median(vars_mydata$mydata[,input$radioreply], na.rm = T),
                        col = "black",
                        lwd = 2)},
             
             "QQ Plot" = qqPlot(unlist(vars_mydata$mydata[input$radioreply])),   
             
             "Density Plot" =ggplot(vars_mydata$mydata, aes(x=vars_mydata$mydata[,input$radioreply])) +
               geom_density(fill = "#87CEFA")+xlab(input$radioreply)+
               geom_vline(data=vars_mydata$mydata, aes(xintercept = mean(vars_mydata$mydata[,input$radioreply])), colour='red') +
               geom_vline(data=vars_mydata$mydata, aes(xintercept = median(vars_mydata$mydata[,input$radioreply])), colour='black'),
             
      )}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  })
  
  
  # here we fix the Title for each plot
  output$mplottitle <- renderText({
    paste(switch(input$mplottype,
                 "Box Plot" = "Box Plot    (red dot represents mean)",
                 "Hist/Bar"= 
                   if (is.character(vars_mydata$mydata[,input$radioreply])  == TRUE){
                     "Bar Chart for this Character Variable"
                   }
                 else{
                   "Histogram   (red line is mean and black is median)"
                 },
                 
                 "QQ Plot" = "Quantile-Quantile Plot",   
                 "Density Plot" ="Density Plot   (red line is mean and black is median)",
    ),"-",input$radioreply)
  })
  
  
  
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$mbtndelete,{
    alert(input$radioreply)
    showModal(
      if(length(input$radioreply)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete variable:",input$radioreply ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    temp <- select(vars_mydata$mydata,-c(input$radioreply))
    vars_mydata$mydata <<- temp
    removeModal()
    
    options <- colnames(vars_mydata$mydata) # The options are dynamically generated on the server
    updateRadioGroupButtons(session,
                            inputId = "radioreply",
                            choices = options,
                            size = "sm", #size options are "xl", "sm","normal","lg"
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-circle",
                                           style = "color: red"),
                              no = tags$i(class = "fa fa-circle-o",
                                          style = "color: steelblue"))
    )
  })
  
}

shinyApp(ui, server)

