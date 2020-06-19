MainDataSelection<-function(data,table,row){
  sel<-data()%>%filter(cond==table[row,"Condition"]&
                         cell==table[row,"Cell"]&
                         timepoint==table[row,"Time_Point"])
  return(sel)
}

AreaSelection<-function(data,index) {
  regsel<-subset(data(), data()[,index]>0)
  return(regsel)
}

ref_row<-function(row,nonref_file,together_file){ 
  which(together_file$Condition==nonref_file[row,"Condition"]&
          together_file$Cell==nonref_file[row,"Cell"]&
          together_file$Reference=="yes")}

MainDataSelectionR<-function(row,table,data){
  sel<-data()%>%filter(cond==table()[row,"Condition"]&
                         cell==table()[row,"Cell"]&
                         timepoint==table()[row,"Ref_TimePoint"])
  return(sel)
}

MainDataSelectionNR<-function(row,table,data){
  sel<-data()%>%filter(cond==table()[row,"Condition"]&
                         cell==table()[row,"Cell"]&
                         timepoint==table()[row,"Time_Point"])
  return(sel)
}

AreaSel<-function(data,index) {
  regsel<-subset(data, data[,index]>0)
  return(regsel)
}