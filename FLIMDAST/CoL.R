ChoiceOfLocation<-function(id,RegNr=c(1,2,3)){
  if (RegNr==1){
    answer<-c("none","one","two")
    inputFile1<-"photons"
    inputFile2<-"photons"
    outputID<-"e"
    outputText<-"photons_filepath"
  } else {
    if (RegNr==2){
      answer<-c("one","two")
      inputFile1<-"RegPhotons"
      inputFile2<-"Regions"
      outputID<-"r1"
      outputText<-"RegPhotons_filepath"
    } else {
      answer<-c("two")
      inputFile1<-"RegPhotons2"
      inputFile2<-"Regions2"
      outputID<-"r2"
      outputText<-"RegPhotons2_filepath"
    }
  }
  name_ID<-paste0("name_",outputID)
  repl_ID<-paste0("repl_",outputID)
  ID_name<-paste0(outputID,"_name")
  
  
  RegPhotons_file_selected<-parseFilePaths(volumes, input[[inputFile1]])
  if(input$AddRegions%in%answer){ 
    datalocation[[name_ID]]<-input[[inputFile2]]
    req(length(input[[inputFile1]])>1|(length(input$T1)>1&input$from_names!=FALSE))
    if (input$from_names==FALSE&length(input[[inputFile1]])>1){ 
      datalocation[[outputID]]<-as.character(RegPhotons_file_selected[4])
    } else {
      
      if(!grepl(input[[repl_ID]],datalocation$t1)){
        showModal(modalDialog(
          "no such pattern in fluorescence lifetime file name",
          style = "font-size:20px",
          easyClose = TRUE,
          footer = NULL
        ))
        datalocation[[outputID]]<-"file does not exist"
        
      } else { 
        
        if (file.exists (sub(input[[repl_ID]],input[[ID_name]] ,datalocation$t1))){
          
          
          datalocation[[outputID]]<-sub(input[[repl_ID]],input[[ID_name]] ,datalocation$t1)
        } else {datalocation[[outputID]]<-"file does not exist"}
      }
    }
  }
  else{
    datalocation[[outputID]]<-""
    datalocation[[name_ID]] <-""
  }
  output[[outputText]] <- renderText(datalocation[[outputID]])    
  
  
  res<-list(datalocation[[outputID]],
            datalocation[[name_ID]]
  )
  
  return(res)
}
