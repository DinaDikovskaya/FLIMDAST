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
