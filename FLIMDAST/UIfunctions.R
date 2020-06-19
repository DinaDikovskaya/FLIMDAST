replPatternSet <-function (id,replace_with){ 
  pat1ID<-paste0("repl_",id)
  pat2ID<-paste0(id,"_name")
  fluidRow(column (width = 1),
           column (width = 2,
                   HTML('<h5>replace </h5>')),
           column (width = 3,
                   textInput(pat1ID,label = NULL, value = "t1.asc")),
           column (width = 1,
                   HTML('<h5>with </h5>')),
           column (width = 5,
                   textInput(pat2ID,label = NULL, value = replace_with))
  )
}

datasetChoice <-function (id,layer,select_col){ 
  dataID<-paste0("dataset",id)
  colorID<-paste0("color_",id)
  descript<-paste0("Select ",layer," dataset")
  radioID<-paste0("radio_",id)
  fluidRow(
    box(width = 8,
        selectInput(dataID,descript,
                    choices=c("no data provided"),
                    selected="no data provided",
                    multiple = FALSE),
        br(),
        radioButtons(radioID,"Overlay with another dataset?",choices = c("no","yes"), selected = "no", inline = TRUE)),
    box(width = 4, 
        colourInput(
          colorID, "select color", select_col,
          returnName = "TRUE",
          showColour = "background",
          palette = "limited"
        ))
  )
}
