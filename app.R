# This app executes FLIMDAST (short for "FLIM DAtaSet Tool") that performs analysis of 
# the Fluorescence Lifetime Imaging (FLIM) time course experiment

# developed for R version 3.3.3

# Author: Dina Dikovskaya 

# The app requires the following packages to be installed before running:

# shiny
# shinyFiles
# DT
# plyr
# dplyr
# ggplot2
# abind
# colorpicker
# shinydashboard
# shinyWidgets


library(shiny)
library(shinyFiles)
library(DT)
library(plyr)
library(dplyr)
library(shinyWidgets)
library(shinydashboard)
library(colourpicker)
library(abind)
library(ggplot2)

source("./UIfunctions.R", local = TRUE) 

ui <- dashboardPage( 
  dashboardHeader(title = "FLIMDAST"),
  
  dashboardSidebar( 
    
    sidebarMenu(  id = "tabs",
                  
                  menuItem("Experiment Layout",tabName = "layout"),
                  menuItem("Single Object Time Course",tabName = "cell"),
                  menuItem("Main Table",tabName = "MainTable"),
                  menuItem("Plot",tabName = "plotting"),
                  menuItem("Quantify",tabName = "quant")
    )
  ),
  
  dashboardBody( 
    tabItems(
      tabItem(tabName = "layout",
              h2( "Experiment Layout"),
              fluidRow(column(width = 4,
                              numericInput("NoC","Number of experimental conditions",1,1,8)
              )),
              uiOutput('dynamic_selections'),
              br(),
              
              fluidRow(column(width = 4,
                              radioButtons("AddRegions", "Number of additional regions",
                                           choices = c("none", "one","two"),
                                           selected = "none")
              ),
              
              column(width = 2,
                     conditionalPanel(
                       condition ="input.AddRegions!='none'",
                       textInput("Regions","Name region 1",value = "nucl")
                     )
              ),
              column(width = 2,       
                     conditionalPanel(
                       condition ="input.AddRegions=='two'",
                       textInput("Regions2","Name region 2",value = "cyt")
                     )
              )
              ),
              
              fluidRow(column(width = 4, 
                              radioButtons("Timecourse", "Time-course divided into parts",
                                           choices = c("no", "two parts","three parts"),
                                           selected = "no")
              ),
              column (width = 8, 
                      conditionalPanel( 
                        condition = "input.Timecourse=='no'",
                        fluidRow(
                          column(width = 6,
                                 numericInput("NrMeas","Number of time points",2,2,40)
                          )
                        )
                      ),
                      conditionalPanel( 
                        condition = "input.Timecourse!='no'",
                        fluidRow(
                          column(width = 3,
                                 uiOutput("NrMeasP1")
                          ),
                          column(width = 3,
                                 uiOutput("NrMeasP2")
                                 
                          ),
                          column(width = 3,
                                 conditionalPanel( 
                                   condition = "input.Timecourse=='three parts'",
                                   uiOutput("NrMeasP3")
                                 )
                          )
                        )
                      )
              )
              )
      ),
      
      tabItem(tabName = "cell",
              h2( "Single Object Time Course"),
              h3(textOutput("text")),
              fluidRow(column (width = 4,
                               selectInput("Condition","Experimental condition",
                                           choices=c("control","treated"),
                                           selected="control",
                                           multiple = FALSE)
              ),
              column (width = 4,
                      selectInput("TimePoint","Time point",
                                  choices=c("T1","T2"),
                                  selected="T1",
                                  multiple = FALSE)
              ),
              column (width = 4,
                      selectInput("FileType","Is this a reference?",
                                  choices=c("yes","no"),
                                  selected="yes",
                                  multiple = FALSE)
              )
              ),
              fluidRow(column (width = 4, 
                               shinyFilesButton("T1", "Choose fluorescence lifetime file" ,
                                                title = "Please select a file:", multiple = TRUE,
                                                buttonType = "default", class = NULL)
              ),
              column (width = 4),
              column (width = 4,
                      materialSwitch(inputId = "from_names", label = "autofill files",value = FALSE, status = "primary")
              )
              ),
              
              textOutput("T1_filepath"),
              br(),
              
              
              fluidRow(column (width = 4, 
                               shinyFilesButton("photons", "Choose photon number file for entire cell" ,
                                                title = "Please select a file:", multiple = TRUE,
                                                buttonType = "default", class = NULL)
              ),
              column (width = 8, 
                      conditionalPanel (
                        condition ="input.from_names==true",
                        replPatternSet("e","photons_ez.txt")
                      )
              )
              ),
              
              textOutput("photons_filepath"),
              br(),
              
              
              conditionalPanel(
                condition ="input.AddRegions!='none'",
                fluidRow(column (width = 4, 
                                 uiOutput("RegionalPhotons")
                ),
                column (width = 8, 
                        conditionalPanel (
                          condition ="input.from_names==true",
                          replPatternSet("r1","photons_nz.txt")
                        )
                )
                ),
                textOutput("RegPhotons_filepath"),
                br()
              ),
              conditionalPanel(
                condition ="input.AddRegions=='two'",
                fluidRow(column (width = 4, 
                                 uiOutput("RegionalPhotons2")
                ),
                column (width = 8, 
                        conditionalPanel (
                          condition ="input.from_names==true",
                          replPatternSet("r2","photons_cz.txt")
                        )
                )
                ),        
                textOutput("RegPhotons2_filepath"),
                br()
              ),
              
              fluidRow(column (width = 3, 
                               actionButton("update", "Add")
              ),
              column (width = 3,
                      div(uiOutput("send_to_table")), align = "middle" ),
              column (width = 3,
                      div(uiOutput("send_to_plot")), align = "middle" ),
              column (width = 3,
                      div(actionButton("delete", "Delete last entry")), align = "right"
              )
              ),
              br(),
              box(width = 12,DTOutput("table2"))
      ),
      
      tabItem(tabName = "MainTable",
              h2( "Main Table"),
              fluidRow(column (width = 3,
                               uiOutput('QuantifyAll')),
                       column (width = 3,
                               div(uiOutput('SaveRes')),align = "middle"),
                       column (width = 3,
                               div(uiOutput("send_to_plot1")), align = "middle" ),                 
                       column (width = 3,
                               div(uiOutput('DownloadAllPlots1'),align = "right")
                       )
              ),
              h3( "Experiment data location"),
              DTOutput("together"),
              fluidRow(column (width = 3,
                               shinySaveButton("saved_data", "Download Main Table" ,
                                               title = "Please save the file:", filename = "FLIMDASTdata", filetype = list(text=c("csv")),
                                               buttonType = "default", class = NULL)
              ),
              column (width = 3,
                      div(actionButton("EmptyTable", "Empty Main Table"),align = "middle")
              ), 
              column (width = 3,
                      div(actionButton("UploadTable", "Upload table from file"),align = "middle")
              ),
              column (width = 3,
                      div(uiOutput("useTbl"),align = "right")
              )
              ),
              br(),
              fluidRow(column(width = 6, uiOutput("uploadTbl"))),
              DTOutput("uploadedTable"),
              conditionalPanel(
                condition ="output.Results",
                h3( "Result Table")
              ),
              
              DTOutput("Results")
      ),
      
      tabItem(tabName = "plotting",
              fluidRow(column (width = 5, 
                               h2("Plot")),
                       column (width = 7,
                               div(uiOutput("NoData"),align = "middle"))
              ),
              fluidRow( 
                column(width = 4,uiOutput('DownloadAllPlots')),
                column(width = 4,
                       actionButton("UsePlot","Use these settings")),
                column(width = 4,
                       actionButton("RemovePlot","Remove last settings"))
                ),
              fluidRow(
                br(),
                column(width = 4),
                column(width = 8,
                uiOutput('cameras'))
              ),
              br(),
              fluidRow( 
                column(width = 5, 
                       fluidRow(
                         column(width = 12,
                                datasetChoice("s","fisrt","black")
                         )
                       ),
                       fluidRow(
                         conditionalPanel(
                           condition ="input.radio_s=='yes'",
                           
                           column(width = 12,
                                  datasetChoice("s1","overlay","green")
                           )
                         )
                       ),
                       fluidRow(
                         conditionalPanel(
                           condition ="input.radio_s1=='yes'&&input.radio_s=='yes'",
                           
                           column(width = 12,
                                  datasetChoice("s2","overlay","green")
                           )
                         )
                       ),
                       fluidRow(
                         conditionalPanel(
                           condition ="input.radio_s2=='yes'&&input.radio_s1=='yes'&&input.radio_s=='yes'",
                           
                           column(width = 12,
                                  datasetChoice("s3","overlay","green")
                           )
                         )
                       )
                ),
                column(width = 7, 
                       box(width = 12,
                           fluidRow(column(width = 2,
                                           strong("photon number scale:")
                           ),
                           column(width = 10,
                                  noUiSliderInput("xrange", NULL,
                                                  min = 0, max = 2500,
                                                  value = c(0,2500),
                                                  step = 10,
                                                  color = "#727272",
                                                  orientation ="horizontal")
                           )
                           ),
                           div(shinySaveButton("downloadPlot", "Download plot" ,
                                               title = "Please save the plot:", filename = "overlay", filetype = list(picture=c("tiff")),
                                               buttonType = "default", class = "btn btn-sm"),align = "right"),
                           fluidRow(column(width = 2,
                                           noUiSliderInput("yrange", "tm scale:",
                                                           min = 0, max = 2500,
                                                           value = c(1400,2200),
                                                           step = 10,
                                                           orientation ="vertical",
                                                           direction ="rtl",
                                                           color = "#727272",
                                                           width = "100px", 
                                                           height = "280px"
                                           )),
                                    column(width = 10,
                                           br(),
                                           plotOutput("scatterplot", height = "300px" )
                                    )
                           )         
                       )
                )) 
              
      ),
      
      tabItem(tabName = "quant",
              fluidRow( column (width = 5,
                                h2( "Quantify")),
                        column (width = 7,
                                div(uiOutput("NoData1"),align = "middle"))
              ),
              fluidRow( 
                column(width = 5, 
                       uiOutput("QuantifyAll1")),
                column(width = 7,
                       uiOutput('SaveRes1'))
              ),
              br(),
              fluidRow( 
                column(width = 5, 
                       fluidRow(
                         box(width = 8,
                             selectInput("datasetq","Select reference dataset",
                                         choices=c("no data provided"),
                                         selected="no data provided",
                                         multiple = FALSE)
                         ),
                         column(width = 4,
                                materialSwitch(inputId = "firstScatter",label = "data",value = TRUE, status = "primary"),
                                materialSwitch(inputId = "firstLoess",label = "model",value = FALSE, status = "primary")
                         )    
                       ),
                       fluidRow(
                         box(width = 8,
                             selectInput("datasetq1","Select non-reference dataset",
                                         choices=c("no data provided"),
                                         selected="no data provided",
                                         multiple = FALSE)
                         ),
                         column(width = 4, 
                                materialSwitch(inputId = "secondScatter",label = "data",value = TRUE, status = "primary"),
                                materialSwitch(inputId = "secondLoess",label = "model",value = FALSE, status = "primary")
                         )
                       ),
                       fluidRow(
                         column(width=6,
                                actionButton("UseSettings","Use these settings")),
                         column(width=6,
                                actionButton("RemoveSettings","Remove last settings"))
                       ),
                       fluidRow(
                         br(),
                         uiOutput('Qcameras')
                       )
                ),
                column(width = 7, 
                       box(width = 12,
                           div(shinySaveButton("downloadQuant", "Download plot" ,
                                               title = "Please save file:", filename = "fitted", filetype = list(picture=c("tiff")),
                                               buttonType = "default", class = "btn btn-sm"),align = "right"),
                           plotOutput("scatterplotQuant",height = "380px"
                           )),
                       
                       fluidRow(column(width = 2,
                                       strong("photon number limits:")
                       ),
                       column(width = 10,
                              noUiSliderInput("limits", NULL,
                                              min = 0, max = 2500,
                                              value = c(0,2500),
                                              step = 10,
                                              color = "#727272",
                                              margin = 20,
                                              orientation ="horizontal")
                       )
                       ),
                       tableOutput("calcul")
                )
              ),
              conditionalPanel(
                condition ="output.Results1",
                h3( "Result Table")
              ),
              DTOutput("Results1")
      )
    )
  )
)

server <- function(input, output, session) {
  
  curr<-dev.cur()
  
  #generating desired number of panels to name experimental conditions
  observeEvent(input$NoC,{
    output$dynamic_selections = renderUI({
      lapply(1:as.integer(input$NoC),function(i){
        fluidRow(
          column(4,
                 textInput(inputId = sprintf("Cond%s",i),label=sprintf("Condition %s",i),value = "control")
                 
          )
        )
      })
    })
  })
  
  ConditionsList<-reactiveValues()
  
  observe({
    ConditionsList$List<-unlist(sapply(1:as.integer(input$NoC),function(i)
      input[[sprintf("Cond%s",i)]])) 
    
    updateSelectInput(session, inputId = "Condition",
                      choices = ConditionsList$List,
                      selected = ConditionsList$List[1])   
  })
  
  #next Cell ID number for current condition
  CellNumber <- reactive(
    { if(is.element(input$Condition,valuesTogether$df$Condition)){
      aa<-valuesTogether$df%>%
        filter(Condition == input$Condition) %>%
        select(Cell)
      CellNumber<-max(as.numeric(aa[[1]]))+1
    } else {CellNumber <- 1}
    }) 
  
  output$text<-renderText({sprintf("Cell %s",CellNumber())}) 
  
  volumes = getVolumes()
  valuesTogether <- reactiveValues() 
  valuesSelected <- reactiveValues()
  dataForPlot<-reactiveValues() #may be remove? where used?
  datalocation<-reactiveValues()
  datalocation$t1<-"" #not sure it works - need check
  datalocation$e<-""
  datalocation$r1<-""
  datalocation$r2<-""
  datalocation$name_r1<-""
  datalocation$name_r2<-""
  datalocation$name_e<-""
  
  #structure for the Main Table
  valuesTogether$df <- data.frame(Condition = numeric(0),
                                  Cell = numeric(0),
                                  Time_Point = numeric(0), 
                                  Reference = numeric(0), 
                                  T1_path = numeric(0),
                                  photon_path = numeric(0),
                                  region_photon_path1 = numeric(0),
                                  region_1_name = numeric(0),
                                  region_photon_path2 = numeric(0),
                                  region_2_name = numeric(0)
  ) 
  
  #structure for the Single Object Time Course table that feeds into the Main Table
  valuesSelected$df <- data.frame(Condition = numeric(0),
                                  Cell = numeric(0),
                                  Time_Point = numeric(0), 
                                  Reference = numeric(0), 
                                  T1_path = numeric(0),
                                  photon_path = numeric(0),
                                  region_photon_path1 = numeric(0),
                                  region_1_name = numeric(0),
                                  region_photon_path2 = numeric(0),
                                  region_2_name = numeric(0)
  ) 
  
  #structure for selected dataset used for settings
  valuesSelected$selRow<- data.frame(Condition = numeric(0),
                                     Cell = numeric(0),
                                     Time_Point = numeric(0), 
                                     Reference = numeric(0), 
                                     T1_path = numeric(0),
                                     photon_path = numeric(0),
                                     region_photon_path1 = numeric(0),
                                     region_1_name = numeric(0),
                                     region_photon_path2 = numeric(0),
                                     region_2_name = numeric(0)
  )
  
  
  number_of_designs<-reactive(nrow(filter(valuesSelected$SettingsTable, type == "Plot")))
  number_of_settings<-reactive(nrow(filter(valuesSelected$SettingsTable, type == "Quant")))
  length_of_main_table<-reactive(nrow(valuesTogether$df))
  
  
  #choice of datasets for Plot and Quantify tabs
  observe({
    req(nrow(valuesSelected$selRow)>0)
    updateSelectInput(session,"datasets","Select first dataset",
                      choices = RefSel(),
                      selected = RefSel()[1])
    updateSelectInput(session,"datasets1","Select overlay dataset",
                      choices = Sel(),
                      selected = NonRefSel()[1])
    updateSelectInput(session,"datasets2","Select overlay dataset",
                      choices = Sel(),
                      selected = NonRefSel()[1])
    updateSelectInput(session,"datasets3","Select overlay dataset",
                      choices = Sel(),
                      selected = NonRefSel()[1])
    updateSelectInput(session,"datasetq","Select reference dataset",
                      choices = RefSel(),
                      selected = RefSel()[1])
    updateSelectInput(session,"datasetq1","Select non-reference dataset",
                      choices = NonRefSel(),
                      selected = NonRefSel()[1])
  })
  
  #removing radio buttons from the last overlay 
  observeEvent(input$radio_s2, {
    removeUI(
      selector = "#radio_s3"
    )
  })
  
  #empty table for the overlay settings
  valuesSelected$SettingsTable<- data.frame(type = numeric(0),
                                            Nr = numeric(0),
                                            Xmin = numeric(0),
                                            Xmax = numeric(0),
                                            Ymin = numeric(0),
                                            Ymax = numeric(0),
                                            FirstDatasetChoice = numeric(0), 
                                            FirstDatasetColor = numeric(0), 
                                            SecondDataset = numeric(0),
                                            SecondDatasetChoice = numeric(0), 
                                            SecondDatasetColor = numeric(0),
                                            ThirdDataset = numeric(0), 
                                            ThirdDatasetChoice = numeric(0), 
                                            ThirdDatasetColor = numeric(0),
                                            ForthDataset = numeric(0), 
                                            ForthDatasetChoice = numeric(0),
                                            ForthDatasetColor = numeric(0)
  )
  
  #populating time point choices in the time course 
  output$NrMeasP1 <- renderUI({
    numericInput("NrMP1","Time points in part 1",1,1,40)
  })
  
  output$NrMeasP2 <- renderUI({
    numericInput("NrMP2","Time points in part 2",1,1,40)
  })
  
  output$NrMeasP3 <- renderUI({
    numericInput("NrMP3","Time points in part 3",1,1,40)
  })
  
  
  state <- reactiveValues()
  
  observe({
    state$a <- ifelse(is.numeric(input$NrMeas),(ifelse(input$Timecourse=="no",input$NrMeas,0)),0)
    state$x <- ifelse(is.numeric(input$NrMP1),(ifelse(input$Timecourse!="no",input$NrMP1,0)),0)
    state$y <- ifelse(is.numeric(input$NrMP2),(ifelse(input$Timecourse!="no",input$NrMP2,0)),0)
    state$z <- ifelse(is.numeric(input$NrMP3),(ifelse(input$Timecourse=="three parts",input$NrMP3,0)),0)
  })
  
  
  FMorig<-reactive({c(c(sprintf("T-%s",0:state$a)),c(sprintf("T1-%s",0:state$x)),c(sprintf("T2-%s",0:state$y)),c(sprintf("T3-%s",0:state$z))   )
  }) #choice of time points with zeros
  
  FM<-reactive({FMorig()[!grepl("-0",FMorig())]
  })  #choice of time points
  
  usedFM<-reactive(as.list(unique(valuesSelected$df[3]))) #time points that are already included into table
  remFM<-reactive(setdiff(FM(),usedFM()[[1]])) #remaining (unused) time points
  
  observe({
    updateSelectInput(session, inputId = "TimePoint",
                      choices = remFM(),
                      selected = remFM()[1])   
  })
  
  #----------- selecting locations of the data files --------------
  
  #uploading file locations 
  shinyFileChoose(input, "T1", roots = volumes)
  shinyFileChoose(input, "photons", roots = volumes)
  
  observe({
    req(length(input$T1)>1)
    T1_file_selected<-parseFilePaths(volumes, input$T1)
    datalocation$t1<-as.character(T1_file_selected[4])
  }) # uploading the path for t1 data file from user-defined location
  output$T1_filepath <- renderText(datalocation$t1)
  
  # uploading the path for photon data file from user-defined location 
  # or generating it from the t1 file path:
  
  
  # conditional appearance of panel asking user to upload photon files for the regions  
  output$RegionalPhotons <- renderUI({
    shinyFilesButton("RegPhotons", paste0("Choose photon number file for ",input$Regions),
                     title = "Please select a file:", multiple = TRUE,
                     buttonType = "default", class = NULL)
  })
  
  output$RegionalPhotons2 <- renderUI({
    shinyFilesButton("RegPhotons2", paste0("Choose photon number file for ",input$Regions2),
                     title = "Please select a file:", multiple = TRUE,
                     buttonType = "default", class = NULL)
  })
  
  shinyFileChoose(input, "RegPhotons", roots = volumes)
  shinyFileChoose(input, "RegPhotons2", roots = volumes)
  
  
  # uploading the path for photon data files from user-defined location 
  # or generating it from the t1 file path - possibly duplication of actions 
  source("./CoL.R", local = TRUE)  
  
  observe({
    ChoiceOfLocation("test1",1)
    ChoiceOfLocation("test2",2)
    ChoiceOfLocation("test3",3)
  }) 
  
  #----------- adding measurements to the table--------------
  observeEvent(input$update, {
    
    # controls that all reqiured files are selected before adding entry for measurement
    if(nchar(datalocation$t1)==0) { 
      showModal(modalDialog(
        "Choose all file locations",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(nchar(datalocation$e)==0|datalocation$e=="file does not exist") { 
      showModal(modalDialog(
        "Choose all file locations",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    if(input$AddRegions!='none'){
      if(nchar(datalocation$r1)==0|datalocation$r1=="file does not exist") { 
        showModal(modalDialog(
          "Choose all file locations",
          style = "font-size:20px",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      if(input$AddRegions=='two'){
        if(nchar(datalocation$r2)==0|datalocation$r2=="file does not exist") { 
          showModal(modalDialog(
            "Choose all file locations",
            style = "font-size:20px",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      }
    }
  })
  
  
  observeEvent(input$update, {  
    
    req(input$T1) # control if there is T1 file present
    req(nchar(datalocation$e)>0)
    req(datalocation$e!="file does not exist")# control if there is photons file present
    
    if(input$AddRegions!='none'){
      req(nchar(datalocation$r1)>0&datalocation$r1!="file does not exist")
      if(input$AddRegions=='two'){
        req(nchar(datalocation$r2)>0&datalocation$r2!="file does not exist")}
    }# controls if there are photons files present for regions
    
    # displays message if there are no more time points:
    if (length(setdiff(FM(),valuesSelected$df$Time_Point))==0){
      showModal(modalDialog(
        "All time points are used",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    # stops adding entries if there all time points are used
    validate(
      need(length(setdiff(FM(),valuesSelected$df$Time_Point))>0, message = "All time points are used")
    )
    
    #  generates a table entry for one FLIM measurement containing data type description
    #  and all contributing files names and their locations (t1, photons for entire cell, 
    #  and optional photons for one or two regions)   
    newLine <- c(input$Condition,
                 CellNumber(),
                 input$TimePoint,
                 input$FileType, 
                 datalocation$t1,
                 datalocation$e,
                 datalocation$r1,
                 datalocation$name_r1,
                 datalocation$r2,
                 datalocation$name_r2
    )
    
    #adding above table entry to the table
    valuesSelected$df[nrow(valuesSelected$df) + 1,] <- newLine 
    
    #updating the data type choice (reference or not):
    
    usedRef<-as.list(unique(valuesSelected$df[4])) #data types already in the table
    
    #fill the tab with the remaining choice of the data types
    if("yes"%in%usedRef[[1]]){
      updateSelectInput(session, inputId = "FileType",
                        choices = "no",
                        selected = "no") 
    }else{
      updateSelectInput(session, inputId = "FileType",
                        choices = c("no","yes"),
                        selected = "yes") 
    }
    
  })
  
  
  #----------- removing unwanted measurement from the table--------------
  observeEvent(input$delete, { 
    
    #deletes last table entry and updates the choice of data types
    valuesSelected$df <- valuesSelected$df[-nrow(valuesSelected$df),] #removes the last table entry
    
    usedRef<-as.list(unique(valuesSelected$df[4])) #data types that are already included into table
    
    #fill the tab with the remaining choice of the data types
    if("yes"%in%usedRef[[1]]){
      updateSelectInput(session, inputId = "FileType",
                        choices = "no",
                        selected = "no") 
    }else{
      updateSelectInput(session, inputId = "FileType",
                        choices = c("no","yes"),
                        selected = "yes") 
    }
    
  })
  #-----------  measurement table and action buttons--------------
  #displays the table that is being assembled
  output$table2 <- renderDT(valuesSelected$df,options=list(autoWidth = TRUE,
                                                           info = FALSE,
                                                           "bFilter"= FALSE,
                                                           paging = FALSE,
                                                           scrollX = T,
                                                           columnDefs = list(list(className = 'dt-center', targets = 1:4)
                                                           ),
                                                           initComplete = htmlwidgets::JS(
                                                             "function(settings, json) {$(this.api().table().container()).css({'font-size': '10pt'});",
                                                             "}")
  )
  )
  
  output$send_to_table<-renderUI({ 
    req(nrow(valuesSelected$df)>0)
    actionButton("CellDone", "Save to main table",class = "btn btn-primary")
  })
  
  output$send_to_plot<-renderUI({ 
    req(nrow(valuesSelected$df)>1)
    actionButton("toVis", "Use for settings",class = "btn btn-primary")
  }) 
  
  
  #similar button for main table
  output$send_to_plot1<-renderUI({ 
    req(nrow(valuesTogether$df)>1)
    actionButton("toVis1", "Use for settings",class = "btn btn-primary")
  }) 
  
  
  #--------------  moving measurements to the Main Table-------------- 
  observeEvent(input$CellDone,{
    
    # check if there are entries in the measurement table
    if(nrow(valuesSelected$df)==0){
      showModal(modalDialog(
        "Please add data to the table",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(
      need(nrow(valuesSelected$df)>0,message = "please add file location")
    )
    
    # check if there is a reference measurement among them
    if(!"yes"%in%valuesSelected$df$Reference){ 
      showModal(modalDialog(
        "Please choose Reference time point",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
      
    }
    
    validate(
      need("yes"%in%valuesSelected$df$Reference,message = "please chose reference time point")
    )# displays error message if there are no more data types
    
    # adding content of measurement table (containing one cell time course) to the Main Table
    valuesTogether$df<-rbind.fill(valuesTogether$df,valuesSelected$df)
    
    # clearing up current information in the measurement table and updating file type choice
    valuesSelected$df <- valuesSelected$df[-c(1:nrow(valuesSelected$df)),] 
    
    updateSelectInput(session, inputId = "FileType",
                      choices = c("no","yes"),
                      selected = "yes")
  })
  
  #-------------------------------- Main Table tab------------- 
  #displays the Main Table that is being assembled  
  output$together <- renderDT(valuesTogether$df,options=list(autoWidth = TRUE,
                                                             scrollX = T,
                                                             columnDefs = list(list(className = 'dt-center', targets = c(1:4,8,10))
                                                             ),
                                                             initComplete = htmlwidgets::JS(
                                                               "function(settings, json) {$(this.api().table().container()).css({'font-size': '10pt'});",
                                                               "}")
  )
  ) 
  
  #saves Main Table
  observeEvent(input$saved_data,{
    
    #check if table is empty
    if(nrow(valuesTogether$df)<1){ 
      showModal(modalDialog(
        "Please add measurements",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    req(nrow(valuesTogether$df)>0)
    
    shinyFileSave(input,"saved_data",roots=volumes)
    fileinfo<-parseSavePath(volumes,input$saved_data)
    if (nrow(fileinfo)>0) {
      write.csv(valuesTogether$df,as.character(fileinfo$datapath))
    }
  })
  
  #emptying Main Table
  observeEvent(input$EmptyTable,{
    valuesTogether$df<-valuesTogether$df[-c(1:nrow(valuesTogether$df)),] 
  })
  
  #uploading previously saved Main Table
  observeEvent(input$UploadTable,{
    
    #calls the upload button
    output$uploadTbl<-renderUI({
      fileInput("UTfile", "Choose Main Table file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      
    })
    
    #displays uploaded file and action button to use it as a Main Table
    output$uploadedTable<-renderDataTable(UplTrim(),
                                          options=list(autoWidth = TRUE,
                                                       scrollX = T,
                                                       columnDefs = list(list(className = 'dt-center', targets = c(1:4,8,10))
                                                       ),
                                                       initComplete = htmlwidgets::JS(
                                                         "function(settings, json) {$(this.api().table().container()).css({'font-size': '10pt'});",
                                                         "}")
                                          )
    )
    
    output$useTbl<-renderUI({
      req(input$UTfile)
      req(nrow(Upl())>0)
      actionButton("UseTable","Use as Main Table",class = "btn btn-primary")
    })
    
  })
  
  #taking info from uploaded file 
  Upl <- reactive(read.csv(input$UTfile$datapath,
                           header = TRUE,
                           sep = ","))
  
  UplTrim<-reactive({
    req(input$UTfile)
    convert<-Upl()[-1]
    convert[is.na(convert)]<-""
    return(convert)
  })
  
  #checking if the uploaded table has the same region selection and region names as current settings; 
  #if not - the current settings are overwriten by the values in the uploaded file 
  #the Main Table is overwriten by the uploaded file
  #and the display returs to that before uploading
  observeEvent(input$UseTable,{
    mismatch<-0 
    ModText1<-""
    ModText2<-""
    ModText3<-""
    
    if(((unique(UplTrim()["region_1_name"]))=="")&&
       (length(unique(UplTrim()["region_1_name"]))==1)){ 
      RegNr<-"none"} else {
        if (((unique(UplTrim()["region_2_name"]))=="")&&
            (length(unique(UplTrim()["region_2_name"]))==1)){
          RegNr<-"one"} else {RegNr<-"two"}
      }
    
    if (RegNr!=input$AddRegions) {
      ModText1<-paste0("Number of additional regions in uploaded file: ", RegNr,". ")
      updateRadioButtons(session,inputId = "AddRegions", 
                         label = "Number of additional regions", 
                         choices = c("none","one","two"),
                         selected = RegNr)
      mismatch<-1
    }
    
    if (RegNr!="none") {
      reg1<-as.character(unlist(unique(UplTrim()["region_1_name"])[1]))
      if (input$Regions!=reg1) {
        ModText2<-"Uploaded name of region 1 is different from selected. "
        updateTextInput(session,"Regions","Name region 1",value = reg1)
        mismatch<-1
      }
      if (RegNr=="two"){
        reg2<-as.character(unlist(unique(UplTrim()["region_2_name"])[1]))
        if (input$Regions2!=reg2) {
          ModText3<-"Uploaded name of region 2 is different from selected. "
          updateTextInput(session,"Regions2","Name region 2",value = reg2)
          mismatch<-1
        }
      }
    }
    
    ModText<-paste0(ModText1,ModText2,ModText3,"Settings are updated.")
    if (mismatch==1){
      showModal(modalDialog(
        ModText,
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    output$uploadedTable<-renderDataTable(NULL)  
    output$uploadTbl<-renderUI(NULL)
    valuesTogether$df<-UplTrim()
    output$useTbl<-renderUI(NULL)
  })
  
  #-------------------------------- quantification of the data ------------
  #quantifying fluorescence lifetime changes (shift) and statistics for the entire experimnent 
  
  source("./Fitting.R", local = TRUE) 
  source("./Plotting.R", local = TRUE)  
  
  #making action buttons in Main Table and Quant tabs when there is something to quantify
  output$QuantifyAll<-renderUI({
    req(length_of_main_table()>0)
    req(number_of_settings()>0)
    actionButton("caclulateAll","Quantify all data",class = "btn btn-primary")
  })
  
  output$QuantifyAll1<-renderUI({
    req(length_of_main_table()>0)
    req(number_of_settings()>0)
    actionButton("caclulateAll1","Quantify all data",class = "btn btn-primary")
  })
  
  
  #generating main storage file with pixel intensities and fluorescence lifetimes for all data
  MainData<-reactive({
    withProgress(message = "extracting pixel values",value = 0, 
                 style = getShinyOption("progress.style", default = "notification"),
                 {  
    DT<-setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("t1","e","r1","r2","cond","cell","timepoint","ref")) 
    
    # using file locations from the Main Table, to read and combine all pixel values
    # of all cells in the experiment into one dataframe 
    for (i in 1:nrow(valuesTogether$df)) {
      Measurement<-BuildingData(valuesTogether$df,i)
      Measurement$cond=valuesTogether$df[i,"Condition"]
      Measurement$cell=as.numeric(valuesTogether$df[i,"Cell"])
      Measurement$timepoint=valuesTogether$df[i,"Time_Point"]
      Measurement$ref=valuesTogether$df[i,"Reference"]
      DT<-rbind(DT,Measurement)
      incProgress(1/nrow(valuesTogether$df), detail = paste(""))
    }
                 })
    return(DT)
  })
  
  # calculates fluorescence lifetime changes for entire experiment
  Res<-reactive({ 
    
    CondList<-unlist(unique(valuesTogether$df[,"Condition"]))
    
    #making Results table with extra columns for "shift" values
    Results<-valuesTogether$df[-c(5:10)]
    NewCol<-c(sprintf("shift%s",1:number_of_settings()))
    Results[,NewCol]<-NA
    # calculating shift values
    for(cnd in CondList) {
      
      CellList<-unlist(unique(valuesTogether$df%>%filter(Condition==cnd)%>%select(Cell)))
      
      for(cl in CellList) {
        #obtaining reference and non-reference time points
        RefTimePoint<-unlist(valuesTogether$df%>%filter(Condition==cnd,Cell==cl,Reference=="yes")%>%select(Time_Point))
        NonrefTimePoint<-unlist(valuesTogether$df%>%filter(Condition==cnd,Cell==cl,Reference=="no")%>%select(Time_Point))
        for (Q in 1:number_of_settings()) {
          Index1<-as.numeric(unlist(valuesSelected$SettingsTable%>%filter(type=="Quant",Nr==Q)%>%select(FirstDatasetChoice)))
          Index2<-as.numeric(unlist(valuesSelected$SettingsTable%>%filter(type=="Quant",Nr==Q)%>%select(SecondDataset)))
          Xminloc<-as.numeric(unlist(valuesSelected$SettingsTable%>%filter(type=="Quant",Nr==Q)%>%select(Xmin)))
          Xmaxloc<-as.numeric(unlist(valuesSelected$SettingsTable%>%filter(type=="Quant",Nr==Q)%>%select(Xmax)))
          
          data_blackloc<-reactive(subset(MainData(),cond==cnd&cell==cl&timepoint==RefTimePoint&MainData()[,Index1]>0))
          MMBloc<-quantile(data_blackloc()$e, prob = c(0.005,0.995))
          RefCurve<-Fitting(data_blackloc)
          
          for (currTP in NonrefTimePoint) { 
            
            DataRow<-which(Results$Condition==cnd&Results$Cell==cl&Results$Time_Point==currTP)  
            trans<-c("entire cell",valuesTogether$df[DataRow,"region_1_name"],valuesTogether$df[DataRow,"region_2_name"])
            transNote<-c("cell",valuesTogether$df[DataRow,"region_1_name"],valuesTogether$df[DataRow,"region_2_name"])
            
            data_greenloc<-reactive(subset(MainData(),cond==cnd&cell==cl&timepoint==currTP&MainData()[,Index2]>0))
            
            MMGloc<-quantile (data_greenloc()$e, prob = c(0.005,0.995))
            commonMinloc<-max(MMBloc[[1]],MMGloc[[1]],Xminloc)
            commonMaxloc<-min(MMBloc[[2]],MMGloc[[2]],Xmaxloc)
            NonRefCurve<-Fitting(data_greenloc)
            
            ResCol<-sprintf("shift%s",Q)
            
            Results[DataRow, ResCol]<-shift_calc_fitted(RefCurve,NonRefCurve,commonMinloc,commonMaxloc)
            N0<-ifelse((Xminloc==0)&(Xmaxloc==2500),"no limits",paste0(Xminloc,":",Xmaxloc))
            N1<-paste0(transNote[Index1-1],"/",transNote[Index2-1],", ",N0)
            Note<-paste0(N1,".  Change in fluorescence lifetime of ",trans[Index2-1]," relative to reference ",trans[Index1-1],
                         ", limited between ",Xminloc," and ",Xmaxloc," photons per pixel" )
            NoteCol<-sprintf("shift%s_settings",Q)
            Results[DataRow, NoteCol]<-Note
          }
        }
      }
    }
    return(Results)
  }) 
  
  # makes result table that combines changes in fluorescence lifetime with mean values
  ComplRes<-reactive({
    
    meanDT<-aggregate(list(mean_t1 = MainData()$t1,mean_photons = MainData()$e),
                      by=list(Condition = MainData()$cond,
                              Cell = MainData()$cell,
                              Time_Point = MainData()$timepoint),
                      mean)
    
    if((""%in%unique(valuesTogether$df[,"region_1_name"]))&
       (length(unique(valuesTogether$df[,"region_1_name"]))==1)) {
      meanDTr1<-meanDT[1:3]
      meanDTr2<-meanDT[1:3]
    }else{
      DTr1<-subset(MainData(),MainData()$r1==1)
      meanDTr1<-aggregate(list(mean_t1_reg1=DTr1$t1,
                               mean_photons_reg1=DTr1$e),
                          by=list(Condition=DTr1$cond,
                                  Cell=DTr1$cell,
                                  Time_Point=DTr1$timepoint),
                          mean)
      if((""%in%unique(valuesTogether$df[,"region_2_name"]))&
         (length(unique(valuesTogether$df[,"region_2_name"]))==1)) {
        meanDTr2<-meanDT[1:3]
      }else{
        DTr2<-subset(MainData(),MainData()$r2==1)
        meanDTr2<-aggregate(list(mean_t1_reg2=DTr2$t1,
                                 mean_photons_reg2=DTr2$e),
                            by=list(Condition=DTr2$cond,
                                    Cell=DTr2$cell,
                                    Time_Point=DTr2$timepoint),
                            mean)
      }
    }
    
    #combining shift results with averages for each cell in the experiment
    ResTable<-merge(Res(),meanDT,by=c("Condition","Cell","Time_Point"),all=T)
    ResTable<-merge(ResTable,meanDTr1,by=c("Condition","Cell","Time_Point"),all=T)
    ResTable<-merge(ResTable,meanDTr2,by=c("Condition","Cell","Time_Point"),all=T)
    return(ResTable)
  })
  
  
  
  # displays the result table in Quantify tab and accompanied "download" button
  observeEvent(input$caclulateAll,{
    
    withProgress(message = "calculating",value = 0, 
                 style = getShinyOption("progress.style", default = "notification"),
                 { incProgress(1/3, detail = paste("data assembly"))
                   a<-nrow(MainData())
                   incProgress(1/3, detail = paste("fluorescence lifetime changes"))
                   b<-nrow(Res())
                   incProgress(1/3, detail = paste("mean values"))
                   c<-nrow(ComplRes())
                 })
    
    CN<-colnames(ComplRes()[c(5:(4+number_of_settings()),(2*number_of_settings()+5):ncol(ComplRes()))])
    CNs<-colnames(ComplRes()[(5+number_of_settings()):(2*number_of_settings()+4)])
    
    #problem with table:if displayed (because of other measurments in the table), non-selected areas are the same as entire cells
    output$Results<-renderDT({
      isolate(datatable(ComplRes(),
                options=list(autoWidth = TRUE, 
                             scrollX = T,
                             columnDefs = list(list(className = 'dt-center',targets = c(1:ncol(ComplRes())))
                                               
                             ),
                             initComplete = htmlwidgets::JS(
                               "function(settings, json) {$(this.api().table().container()).css({'font-size': '10pt'});",
                               "}")
                )
      ) %>%formatRound(CN,digits = 2, mark = "")%>%formatStyle(CNs, "column-width" = '18px', "overflow" = "auto","white-space" = "nowrap")
    )})
    
    output$SaveRes<-renderUI({ 
      shinySaveButton("save_results", "Download Result Table" ,
                      title = "Please save file:", filename = "ResultTable", filetype = list(text=c("csv")),
                      buttonType = "default", class = "btn btn-primary")
    })
  })
  
  # displays the result table to Main table tab and accompanied "download" button
  observeEvent(input$caclulateAll1,{
    
    withProgress(message = "calculating",value = 0, 
                 style = getShinyOption("progress.style", default = "notification"),
                 { incProgress(1/3, detail = paste("data assembly"))
                   a<-nrow(MainData())
                   incProgress(1/3, detail = paste("fluorescence lifetime changes"))
                   b<-nrow(Res())
                   incProgress(1/3, detail = paste("mean values"))
                   c<-nrow(ComplRes())
                 })
    
    CN<-colnames(ComplRes()[c(5:(4+number_of_settings()),(2*number_of_settings()+5):ncol(ComplRes()))])
    CNs<-colnames(ComplRes()[(5+number_of_settings()):(2*number_of_settings()+4)])
    
    #table needs improvement, the same as above
    output$Results1<-renderDT({isolate( 
      datatable(ComplRes(),
                options=list(autoWidth = TRUE, 
                             scrollX = T,
                             columnDefs = list(list(className = 'dt-center',targets = c(1:ncol(ComplRes())))
                                               
                             ),
                             initComplete = htmlwidgets::JS(
                               "function(settings, json) {$(this.api().table().container()).css({'font-size': '10pt'});",
                               "}")
                )
      ) %>%formatRound(CN,digits = 2, mark = "")%>%formatStyle(CNs, "column-width" = '18px', "overflow" = "auto","white-space" = "nowrap")
   ) })
    
    output$SaveRes1<-renderUI({ 
      shinySaveButton("save_results1", "Download Results" ,
                      title = "Please save file:", filename = "ResultTable", filetype = list(text=c("csv")),
                      buttonType = "default", class = "btn btn-primary")
    })
  })
  
  #downloading results table from Quantify tab
  observeEvent(input$save_results,{
    
    shinyFileSave(input,"save_results",roots=volumes)
    fileinfo<-parseSavePath(volumes,input$save_results)
    if (nrow(fileinfo)>0) {
      write.csv(ComplRes(),as.character(fileinfo$datapath))
    }
  })
  
  #downloading results table from Main Table tab
  observeEvent(input$save_results1,{
    
    shinyFileSave(input,"save_results1",roots=volumes)
    fileinfo<-parseSavePath(volumes,input$save_results1)
    if (nrow(fileinfo)>0) {
      write.csv(ComplRes(),as.character(fileinfo$datapath))
    }
  })
  
  # ---------------------plotting all data--------------------------  
  
  # making action buttons in Main Table and Plot tabs, when there is something to plot
  
  output$DownloadAllPlots<-renderUI({ 
    req(length_of_main_table()>0)
    req(number_of_designs()>0)
    downloadButton("download_Plot"," Plot all data",class = "btn btn-primary")
  })
  
  output$DownloadAllPlots1<-renderUI({ 
    req(length_of_main_table()>0)
    req(number_of_designs()>0)
    downloadButton("download_Plot1"," Plot all data",class = "btn btn-primary")
  })
  
  #making table that combines measurements from Main Table and plot settings  
  ComboPlotTable<-reactive({
    req(length_of_main_table()>0)
    req(number_of_designs()>0)
   
     withProgress(message = "making data table",value = 0, 
                 style = getShinyOption("progress.style", default = "notification"),
                 {  
    
    together_short<-valuesTogether$df[-c(5,6,7,9)] 
    nonref_table<-filter(together_short, Reference =="no")
    PlotSetTable<-filter(valuesSelected$SettingsTable,type =="Plot")
    
    ext_table<-nonref_table
    vec<-c(1:nrow(ext_table))
    
    Ref_TimePoint<-lapply(vec,function(x){together_short[ref_row(x,nonref_table,together_short),"Time_Point"]})
    Ref_region_1_name<-lapply(vec,function(x){together_short[ref_row(x,nonref_table,together_short),"region_1_name"]})
    Ref_region_2_name<-lapply(vec,function(x){together_short[ref_row(x,nonref_table,together_short),"region_2_name"]})
    ext_table$Ref_TimePoint<-unlist(Ref_TimePoint[])
    ext_table$Ref_region_1_name<-unlist(Ref_region_1_name[])
    ext_table$Ref_region_2_name<-unlist(Ref_region_2_name[])  
    ext_table$PlotNr<-nrow(PlotSetTable) 
    
    ext_table_wPlots<-ext_table %>%  
      slice(rep(seq_len(n()), PlotNr)) %>% 
      select(-PlotNr)
    
    ext_table_wPlots1<-cbind(ext_table_wPlots,PlotSetTable)
                 })
    
    return(ext_table_wPlots1)
  })
  
  # plotting all data
  AllPlots<-reactive({
    req(nrow(ComboPlotTable())>0)
    req(nrow(MainData())>0)
    
    withProgress(message = "making plots",value = 0, 
                 style = getShinyOption("progress.style", default = "notification"),
                 {  
                   
    PlotsCount<-c(1:nrow(ComboPlotTable()))
    
    Second_label<-NA
    Third_label<-NA
    Forth_label<-NA
    SecondColor<-NA
    ThirdColor<-NA
    ForthColor<-NA
    
   content<-function(x) {
     
                data_bl<-MainDataSelectionR(x,ComboPlotTable,MainData)
                data_gr<-MainDataSelectionNR(x,ComboPlotTable,MainData) 
                area<-c("entire cell",ComboPlotTable()[x,"Ref_region_1_name"],ComboPlotTable()[x,"Ref_region_2_name"])
                area1<-c("entire cell",ComboPlotTable()[x,"region_1_name"],ComboPlotTable()[x,"region_2_name"])
                
                Index1<-as.numeric(ComboPlotTable()[x,"FirstDatasetChoice"])
                first_data<-AreaSel(data_bl,Index1)
                First_label<-paste0(ComboPlotTable()[x,"Ref_TimePoint"]," ",area[Index1-1]," (ref)")
                FirstColor<-as.character(ComboPlotTable()[x,"FirstDatasetColor"])
                Xmin<-as.numeric(ComboPlotTable()[x,"Xmin"])
                Xmax<-as.numeric(ComboPlotTable()[x,"Xmax"])
                Ymin<-as.numeric(ComboPlotTable()[x,"Ymin"])
                Ymax<-as.numeric(ComboPlotTable()[x,"Ymax"])
                
                Data2<-as.numeric(ComboPlotTable()[x,"SecondDatasetChoice"])
                Index2<-as.numeric(ComboPlotTable()[x,"SecondDataset"])
                Data3<-as.numeric(ComboPlotTable()[x,"ThirdDatasetChoice"])
                Index3<-as.numeric(ComboPlotTable()[x,"ThirdDataset"])
                Data4<-as.numeric(ComboPlotTable()[x,"ForthDatasetChoice"])
                Index4<-as.numeric(ComboPlotTable()[x,"ForthDataset"])
                
                SecondTimePoint<-ComboPlotTable()[x,"Ref_TimePoint"]

      re<-ggplot(first_data, aes(x = e, y = t1, color = "black")) +
        geom_point() +
        theme_classic() +
        labs(x="photon number",
             y="tm, ps") +
        theme(text =element_text(size = 16)) +
        theme(legend.title = element_blank(),
              legend.justification = c(1, 1), 
              legend.position="bottom",
              legend.direction="vertical")+
        ylim(Ymin,Ymax) +
        xlim(Xmin,Xmax) 
      
      if (!is.na(Index2)){
         SecondColor<-as.character(ComboPlotTable()[x,"SecondDatasetColor"])
         if (Data2 == 1) {
           overlaydata1<-AreaSel(data_bl,Index2)
           SecondTimePoint<-ComboPlotTable()[x,"Ref_TimePoint"]
           ar<-area[Index2-1]
         } else {
           overlaydata1<-AreaSel(data_gr,Index2)
           SecondTimePoint<-ComboPlotTable()[x,"Time_Point"]
           ar<-area1[Index2-1]
         }
         Second_label<-paste0(SecondTimePoint," ",ar)
         overlay1<-geom_point(data=overlaydata1,aes(color = "blue"),alpha=0.4)
         re<-re+overlay1
        
        if (!is.na(Index3)){
          ThirdColor<-as.character(ComboPlotTable()[x,"ThirdDatasetColor"])
          if (Data3 == 1) {
            overlaydata2<-AreaSel(data_bl,Index3)
            SecondTimePoint<-ComboPlotTable()[x,"Ref_TimePoint"]
            ar<-area[Index3-1]
          } else {
            overlaydata2<-AreaSel(data_gr,Index3)
            SecondTimePoint<-ComboPlotTable()[x,"Time_Point"]
            ar<-area1[Index3-1]
          }
          Third_label<-paste0(SecondTimePoint," ",ar)
          overlay2<-geom_point(data=overlaydata2,aes(color = "red"),alpha=0.4)
          re<-re+overlay2
          
          if (!is.na(Index4)){
            ForthColor<-as.character(ComboPlotTable()[x,"ForthDatasetColor"])
            if (Data4 == 1) {
              overlaydata3<-AreaSel(data_bl,Index4)
              SecondTimePoint<-ComboPlotTable()[x,"Ref_TimePoint"]
              ar<-area[Index4-1]
            } else {
              overlaydata3<-AreaSel(data_gr,Index4)
              SecondTimePoint<-ComboPlotTable()[x,"Time_Point"]
              ar<-area1[Index4-1]
            }
            Forth_label<-paste0(SecondTimePoint," ",ar)
            overlay3<-geom_point(data=overlaydata3,aes(color = "yellow"),alpha=0.4)
            re<-re+overlay3
          
            }
        }
      }
      TitleText2<-ifelse(2%in%c(Data2,Data3,Data4),paste0("\n with ",ComboPlotTable()[x,"Ref_TimePoint"]," (ref) ",Data2,Data3,Data3),"")
      TitleText<-paste0(ComboPlotTable()[x,"Condition"]," cell ",ComboPlotTable()[x,"Cell"]," ",SecondTimePoint," ",TitleText2)
      
      re<-re + scale_colour_manual(values = c("black" = FirstColor, "blue" = SecondColor,"red" = ThirdColor,"yellow" = ForthColor ),
                          labels = c("black" = First_label, "blue" = Second_label,"red" = Third_label, "yellow" = Forth_label))+
        guides(fill = guide_legend(override.aes = list(size = 2,alpha=1)))+
        theme(legend.margin = margin(-0.5,0,0,0, unit="cm"))+
        ggtitle(TitleText) +
        theme(plot.title = element_text(hjust = 0.5))
      
      incProgress(1/PlotsCount, detail = paste(""))
      
    return(re)
   }
   
    plotvector<-lapply(PlotsCount,content)
    
                 }) 
    return(plotvector)
  
  })
  
  
  #downloading plots for the entire experiment from the Plot tab   
  output$download_Plot<-downloadHandler(
    filename = function(){
      paste("FLIMDASTplots","pdf",sep = ".")
    },
    content = function(file) {
      
      withProgress(message = "printing",value = 0, 
                   style = getShinyOption("progress.style", default = "notification"),
                   {  
       
      pdf(file) 
 
      for (i in 1:length(AllPlots())){
        plot(AllPlots()[[i]])
        incProgress(1/length(AllPlots()), detail = paste(""))
      }
                             
      dev.off()
      
                   }
      )
    }
  )
  
  #downloading plots for the entire experiment from the Main Table tab   
  output$download_Plot1<-downloadHandler(
    filename = function(){
      paste("FLIMDASTplots","pdf",sep = ".")
    },
    content = function(file) {
      
      withProgress(message = "printing",value = 0, 
                   style = getShinyOption("progress.style", default = "notification"),
                   {  
                     
      pdf(file) 
                     
      for (i in 1:length(AllPlots())){
          plot(AllPlots()[[i]])
          incProgress(1/length(AllPlots()), detail = paste(""))
      }
                     
       dev.off()
                     
        }
      )
    }
  )
  
  
  
  
  # --------- accepting data for setting plots and quantification from selected measurements------
  
  #signs if there are no data
  output$NoData<-renderUI({
    req(nrow(valuesSelected$selRow)<1)
    h4("Select data for settings in the Main Table or 
       the Single Object Time course tab")
  })
  
  output$NoData1<-renderUI({
    req(nrow(valuesSelected$selRow)<1)
    h4("Select data for settings in the Main Table or 
       the Single Object Time course tab")
  })
  
  
  #from the selection in the measurement table in the Single Object Time Course tab 
  observeEvent(input$toVis,{
    #checks whether there are exactly two measurements selected, one of which is a reference  
    if(is.null(input$table2_rows_selected)){
      showModal(modalDialog(
        "Highlight one reference and one non-reference entry in the table",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    req(input$table2_rows_selected)
    
    if(length(input$table2_rows_selected)!=2){
      showModal(modalDialog(
        "Highlight two entries in the table",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    validate(
      need(length(input$table2_rows_selected)==2,"choose two time points to compare")
    ) 
    
    selRow <- valuesSelected$df[input$table2_rows_selected,]
    
    if (!"yes"%in%selRow[[4]]) {
      showModal(modalDialog(
        "One of the entries has to be a reference",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }     
    validate(
      need("yes"%in%selRow[[4]], "One of the time points has to be a reference")
    ) 
    
    if (!"no"%in%selRow[[4]]) {
      showModal(modalDialog(
        "One of the entries has to be a non-reference",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }     
    validate(
      need("no"%in%selRow[[4]], "One of the time points has to be a non-reference")
    )       
    
    valuesSelected$selRow<-selRow[order(selRow$Reference, decreasing=TRUE),]
    
  })
  
  #from the selection in the Main Table
  observeEvent(input$toVis1,{
    #checks whether there are exactly two measurements selected, one of which is a reference  
    if(is.null(input$together_rows_selected)){
      showModal(modalDialog(
        "Highlight one reference and one non-reference entry in the table",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    req(input$together_rows_selected)
    
    if(length(input$together_rows_selected)!=2){
      showModal(modalDialog(
        "Highlight two entries in the table",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    validate(
      need(length(input$together_rows_selected)==2,"choose two time points to compare")
    ) 
    selRow <- valuesTogether$df[input$together_rows_selected,]
    
    if (!"yes"%in%selRow[[4]]) {
      showModal(modalDialog(
        "One of the entries has to be a reference",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }     
    validate(
      need("yes"%in%selRow[[4]], "One of the time points has to be a reference")
    ) 
    
    if (!"no"%in%selRow[[4]]) {
      showModal(modalDialog(
        "One of the entries has to be a non-reference",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }     
    validate(
      need("no"%in%selRow[[4]], "One of the time points has to be a non-reference")
    ) 
    
    valuesSelected$selRow<-selRow[order(selRow$Reference, decreasing=TRUE),]
    
  })  
  
  #generating full datasets for settings and their annotations  
  ref_flatData<-reactive(BuildingData(valuesSelected$selRow,1))
  flatData<-reactive(BuildingData(valuesSelected$selRow,2))
  bothdata<-reactive(list(ref_flatData(),flatData()))
  
  RefSel<-reactive(unlist(c("reference entire cell", 
                            ifelse(as.character(valuesSelected$selRow[1,8])!="", paste0("reference ",as.character(valuesSelected$selRow[1,8])), list(NULL)), 
                            ifelse(as.character(valuesSelected$selRow[1,10])!="", paste0("reference ",as.character(valuesSelected$selRow[1,10])), list(NULL))
  )))
  NonRefSel<-reactive(unlist(c("non-reference entire cell",
                               ifelse(as.character(valuesSelected$selRow[2,8])!="", paste0("non-reference ",as.character(valuesSelected$selRow[2,8])),list(NULL)),
                               ifelse(as.character(valuesSelected$selRow[2,10])!="", paste0("non-reference ",as.character(valuesSelected$selRow[2,10])),list(NULL)))))
  
  Sel<-reactive(c(RefSel(),NonRefSel()))
  
  #--------- settings of the plots layouts----------------------------
  
  #selection of the overlay datasets
  s1r<-reactive(if (input$radio_s=="yes"){
    if (which(Sel()==input$datasets1)%in%c(1:length(RefSel()))) {
      which(Sel()==input$datasets1)+1
    } else {
      which(Sel()==input$datasets1)-length(RefSel())+1
    } 
  } else {NA}
  )
  
  dataind_s1<-reactive (if (input$radio_s=="yes"){
    if (which(Sel()==input$datasets1)%in%c(1:length(RefSel()))) {1}  else {2}
  } else {NA}
  )
  
  s2r<-reactive(if (input$radio_s1=="yes"){
    if (which(Sel()==input$datasets2)%in%c(1:length(RefSel()))) {
      which(Sel()==input$datasets2)+1
    } else {
      which(Sel()==input$datasets2)-length(RefSel())+1
    } 
  } else {NA}
  )
  
  dataind_s2<-reactive (if (input$radio_s1=="yes"){
    if (which(Sel()==input$datasets2)%in%c(1:length(RefSel()))) {1}  else {2}
  } else {NA}
  )
  
  s3r<-reactive(if (input$radio_s2=="yes"){
    if (which(Sel()==input$datasets3)%in%c(1:length(RefSel()))) {
      which(Sel()==input$datasets3)+1
    } else {
      which(Sel()==input$datasets3)-length(RefSel())+1
    } 
  } else {NA}
  )
  
  dataind_s3<-reactive (if (input$radio_s2=="yes"){
    if (which(Sel()==input$datasets3)%in%c(1:length(RefSel()))) {1}  else {2}
  } else {NA}
  )
  
  #contructing a plot
  reNew<-reactive({
    
    data<-bothdata()[[1]][bothdata()[[1]][[which(RefSel()==input$datasets)+1]]!=0,]
    col_s<-input$color_s
    col_s1<-input$color_s1
    col_s2<-input$color_s2
    col_s3<-input$color_s3
    re<-ggplot(data, aes(x = e, y = t1,color="black")) +
      geom_point() +
      theme_classic() +
      labs(x="photon number",
           y="tm, ps") +
      theme(text =element_text(size = 16)) +
      theme(legend.title = element_blank(),
            legend.justification = c(1, 1), 
            legend.position="bottom",
            legend.direction="vertical")+
      ylim(input$yrange[1],input$yrange[2]) +
      xlim(input$xrange[1],input$xrange[2]) 
    
    if (input$radio_s=="yes"){
      validate(need(which(Sel()==input$datasets1)%in%c(1:6),message ="data not chosen"))
      overlaydata1<-bothdata()[[dataind_s1()]][bothdata()[[dataind_s1()]][[s1r()]]!=0,]
      overlay1<-geom_point(data=overlaydata1,aes(color = "blue"),alpha=0.4)
      re<-re+overlay1
      
      if (input$radio_s1=="yes"){
        validate(need(which(Sel()==input$datasets2)%in%c(1:6),message ="data not chosen")) 
        overlaydata2<-bothdata()[[dataind_s2()]][bothdata()[[dataind_s2()]][[s2r()]]!=0,]
        overlay2<-geom_point(data=overlaydata2,aes(color = "red"),alpha=0.4)
        re<-re+overlay2
        
        
        if (input$radio_s2=="yes"){
          validate(need(which(Sel()==input$datasets3)%in%c(1:6),message ="data not chosen")) 
          overlaydata3<-bothdata()[[dataind_s3()]][bothdata()[[dataind_s3()]][[s3r()]]!=0,]
          overlay3<-geom_point(data=overlaydata3,aes(color = "yellow"),alpha=0.4)
          re<-re+overlay3
        }
      }
    }
    
    re<-re + 
      scale_colour_manual(values = c("black"= col_s, "blue" = col_s1,"red" = col_s2,"yellow" = col_s3),
                          labels = c("black"= input$datasets,"blue" = input$datasets1,"red" = input$datasets2,"yellow" = input$datasets3))+
      
      guides(fill = guide_legend(override.aes = list(size = 2,alpha=1)))+
      theme(legend.margin = margin(-0.5,0,0,0, unit="cm"))
    
    return(re)
  })
  
  #making a plot, after checking that the data exists
  observe({ 
    req(nrow(valuesSelected$selRow)>0)
    validate(need(which(RefSel()==input$datasets)%in%c(1:6),message ="data not chosen")) 
    output$scatterplot<-renderPlot({reNew()
    }, height = function() {
      1.2*session$clientData$output_scatterplot_width 
    }
    ) 
  })
  
  #download of the current plot dispay
  observeEvent(input$downloadPlot, { 
    if(is.null(reNew())){
      showModal(modalDialog(
        "Choose data for plot",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    validate(
      need(!is.null(reNew()), "Choose data for plot")
    )
    
    shinyFileSave(input,"downloadPlot",roots=volumes)
    fileinfo1<-parseSavePath(volumes,input$downloadPlot)
    if (nrow(fileinfo1)>0) {
      tiff(file=as.character(fileinfo1$datapath),width=6, height=6, units="in", res=120)
      TitleText2<-ifelse(2%in%c(dataind_s1(),dataind_s2(),dataind_s3()),
                         paste0(" (ref),\n and ",valuesSelected$selRow[[2,1]]," cell ",valuesSelected$selRow[[2,2]]," ",valuesSelected$selRow[[2,3]]," (non-ref)"),"")
      TitleText<-paste0(valuesSelected$selRow[[1,1]]," cell ",valuesSelected$selRow[[1,2]]," ",valuesSelected$selRow[[1,3]],TitleText2)
      plot(reNew() + ggtitle(TitleText) +
             theme(plot.title = element_text(hjust = 0.5)))
      dev.off()
      
    }
  } )
  
  
  
  #recording plot settings
  observeEvent(input$UsePlot, { 
    its<-nrow(filter(valuesSelected$SettingsTable, type == "Plot"))+1
    newLineSettings <- c("Plot",
                         its,
                         input$xrange[1],
                         input$xrange[2], 
                         input$yrange[1],
                         input$yrange[2],
                         which(RefSel()==input$datasets)+1,
                         input$color_s, 
                         s1r(),
                         dataind_s1(),
                         ifelse(input$radio_s=="yes",input$color_s1,NA),
                         s2r(),
                         dataind_s2(),
                         ifelse(input$radio_s1=="yes",input$color_s2,NA),
                         s3r(),
                         dataind_s3(),
                         ifelse(input$radio_s2=="yes",input$color_s3,NA)
    )
    
    valuesSelected$SettingsTable[nrow(valuesSelected$SettingsTable) + 1,] <- newLineSettings #adding above table entry to the table
    
    # building the correct number of camera icons
    verc<-rep(source(file = "Cam.R", local = T)[1],times = number_of_designs())
    
    # displaying the camera icons
    output$cameras = renderUI({
      tags$ol(verc) 
    })
    
  })
  
  # removing last of the plot settings, and the camera icon from the list
  observeEvent(input$RemovePlot, {
    subData<-valuesSelected$SettingsTable%>%filter(type == "Plot")%>%select(Nr)
    
    if(!length(unlist(subData[1]))>0){
      showModal(modalDialog(
        "No more designs",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need (length(unlist(subData[1]))>0,"no more designs"))
    
    N<-max(as.numeric(unlist(subData[1])))
    
    valuesSelected$SettingsTable<-valuesSelected$SettingsTable[!(valuesSelected$SettingsTable$type=="Plot"&valuesSelected$SettingsTable$Nr==N),]
    
    verc<-rep(source(file = "Cam.R", local = T)[1],times = number_of_designs())
    
    output$cameras = renderUI({
      tags$ol(verc) 
    })
    
  })
  
  
  #--------------------------settings of fluorescence lifetime quantification-------------------
  
  #selection of datasets for quantification settings
  qi<-reactive({which(RefSel()==input$datasetq)})
  q1i<-reactive({which(NonRefSel()==input$datasetq1)})
  
  data_black<-reactive({bothdata()[[1]][bothdata()[[1]][[qi()+1]]!=0,]})
  data_green<-reactive({bothdata()[[2]][bothdata()[[2]][[q1i()+1]]!=0,]})
  
  #calculation of the photon number  range within which to quantfy fluorescence lifetime difference
  MMB<-reactive({quantile (data_black()$e, prob = c(0.005,0.995))})
  MMG<-reactive({quantile (data_green()$e, prob = c(0.005,0.995))})
  
  commonMin<-reactive({max(MMB()[[1]],MMG()[[1]],input$limits[1])})
  commonMax<-reactive({min(MMB()[[2]],MMG()[[2]],input$limits[2])})
  
  #calculation of the fluorescence lifetime difference
  Shift<-reactive(shift_calc(data_black,data_green,commonMin,commonMax))
  
  #constructing the plot with data and models, to show how the calculations are done
  Qplot<-reactive({
    if (input$firstScatter==TRUE) {FirstCol ="black"} else {FirstCol = NA}
    if (input$secondScatter==TRUE) {SecondCol = "grey60"} else {SecondCol = NA}
    if (input$firstLoess==TRUE) {FirstLineCol = "blue"} else {FirstLineCol = NA}
    if (input$secondLoess==TRUE) {SecondLineCol = "red"} else {SecondLineCol = NA}
    if (input$firstLoess==TRUE|input$secondLoess==TRUE) {
      lineCol = "grey"
      SubtitleText<-paste0("difference in fluorescence lifetime: ",round(Shift(), digits = 2)," ps")
    } else {
      lineCol = NA
      SubtitleText<-""
    }
    
    TitleText<-paste0(valuesSelected$selRow[[1,3]]," (ref) and ",valuesSelected$selRow[[2,3]]," (non-ref)")
    
    qrpl<-ggplot(data = data_black(), aes(x = e, y = t1,color="black")) +
      geom_point() +
      theme_classic() +
      labs(x="photon number",
           y="tm, ps") +
      theme(text =element_text(size = 16)) +
      theme(legend.title = element_blank(),
            legend.justification = c(1, 1), 
            legend.position="bottom",
            legend.direction="vertical")+
      xlim(c(input$xrange[1],input$xrange[2]))+
      ylim(c(input$yrange[1],input$yrange[2]))+
      geom_point(data=data_green(),aes(color = "green"),alpha=0.4)+
      geom_smooth(data=data_black(),method="loess", se=FALSE, fullrange=FALSE,xseq = commonMin():commonMax(),  level=0.95, span = 0.1, color=FirstLineCol)+
      geom_vline(xintercept = commonMin(), linetype="dotted", color = lineCol, size=1) +
      geom_vline(xintercept = commonMax(), linetype="dotted", color = lineCol, size=1) +
      geom_smooth(data=data_green(),method="loess", se=FALSE, fullrange=FALSE,xseq = commonMin():commonMax(),  level=0.95, span = 0.1, color=SecondLineCol)+ 
      scale_colour_manual(values = c("black"= FirstCol, "green" = SecondCol),
                          labels = c("black"= input$datasetq,"green" = input$datasetq1))+
      guides(fill = guide_legend(override.aes = list(size = 2,alpha=1)))+
      theme(legend.margin = margin(-0.5,0,0,0, unit="cm"))+
      ggtitle(bquote(atop(.(TitleText),atop(italic(.(SubtitleText))))))+
      theme(plot.title = element_text(hjust = 0.5))
    return(qrpl)
  })
  
  #displaying the above plot
  observe({ 
    req(nrow(valuesSelected$selRow)>0)
    
    validate(need(qi()%in%c(1:3),message ="data not chosen"),
             need(q1i()%in%c(1:3),message ="data not chosen") )
    
    output$scatterplotQuant<-renderPlot({Qplot()}) 
    
  })
  
  #downloading the above plot
  observeEvent(input$downloadQuant, { 
    if(is.null(Qplot())){
      showModal(modalDialog(
        "Choose data for plot",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    validate(
      need(!is.null(Qplot()), "Choose data for plot")
    )
    
    shinyFileSave(input,"downloadQuant",roots=volumes)
    fileinfo2<-parseSavePath(volumes,input$downloadQuant)
    if (nrow(fileinfo2)>0) {
      tiff(file=as.character(fileinfo2$datapath),width=6, height=6, units="in", res=120)
      TitleText<-paste0(valuesSelected$selRow[[1,3]]," (ref) and ",valuesSelected$selRow[[2,3]]," (non-ref)")
      SubtitleText<-paste0("difference in fluorescence lifetime: ",round(Shift(), digits = 2)," ps")
      plot(Qplot() + ggtitle(bquote(atop(.(TitleText),atop(italic(.(SubtitleText)))))) +
             
             theme(plot.title = element_text(hjust = 0.5)))
      dev.off()
      
    }
  } )
  
  #recording the quantification settings
  observeEvent(input$UseSettings, { 
    its<-nrow(filter(valuesSelected$SettingsTable, type == "Quant"))+1
    newLineSettings <- c("Quant",
                         its,
                         input$limits[1],
                         input$limits[2], 
                         NA,
                         NA,
                         qi()+1,
                         NA, 
                         q1i()+1,
                         2,
                         NA,
                         NA,
                         NA,
                         NA,
                         NA,
                         NA,
                         NA
    )
    
    valuesSelected$SettingsTable[nrow(valuesSelected$SettingsTable) + 1,] <- newLineSettings #adding above table entry to the table
    
    # building the correct number of camera icons
    verc<-rep(source(file = "Cam.R", local = T)[1],times = its)
    
    # displaying the camera icons
    output$Qcameras = renderUI({
      tags$ol(verc) 
    })
    
  })
  
  # removing last form the quantification settings, and the camera icon from the list
  observeEvent(input$RemoveSettings, {
    subData<-valuesSelected$SettingsTable%>%filter(type == "Quant")%>%select(Nr)
    if(!length(unlist(subData[1]))>0){
      showModal(modalDialog(
        "No more settings",
        style = "font-size:20px",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    validate(need (length(unlist(subData[1]))>0,"no more settings"))
    
    N<-max(as.numeric(unlist(subData[1])))
    
    valuesSelected$SettingsTable<-valuesSelected$SettingsTable[!(valuesSelected$SettingsTable$type=="Quant"&valuesSelected$SettingsTable$Nr==N),]
    
    its<-nrow(filter(valuesSelected$SettingsTable, type == "Quant"))
    
    verc<-rep(source(file = "Cam.R", local = T)[1],times = its)
    
    output$Qcameras = renderUI({
      tags$ol(verc) 
    })
    
  })
  
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

