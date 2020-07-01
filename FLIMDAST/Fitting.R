Fitting<-function(DT){
  DT_ord<-DT()[order(DT()$e),]
  lo<-loess(t1~e,data=DT_ord,span=0.10)
  fit <- predict(lo)
  fit_holder=data.frame(Ph=DT_ord$e,Tm=fit)
  scaled=list()
  for (i in min(fit_holder$Ph):max(fit_holder$Ph)) {
    a<-subset(fit_holder,Ph==i) 
    if (NROW(a)>0) {
      if (length(unique(a$Tm))==1) {scaled<-append(scaled,unique(a$Tm))} 
      else {scaled<-append(scaled,mean(unique(a$Tm)))}
    } else {scaled<-append(scaled,0)}
  }
  NonZeroPos<-which(!scaled==0)
  scaledext=list()
  for (i in 1:length(scaled)) {
    if (scaled[i]!=0) {scaledext<-append(scaledext,scaled[i])} else {
      PrevNonZeroPos<-max(NonZeroPos[which(NonZeroPos<i)])
      leftboundary<-scaled[PrevNonZeroPos]
      NextNonZeroPos<-min(NonZeroPos[which(NonZeroPos>i)])
      rightboundary<-scaled[NextNonZeroPos]
      replVal<-(unlist(leftboundary)+unlist(rightboundary))/2
      scaledext<-append(scaledext,replVal)
    }
  }
  SPAN <- seq(min(fit_holder$Ph),max(fit_holder$Ph),by=1) 
  scaled_holder=data.frame(Ph=c(SPAN),Tm=unlist(scaledext))
  return(scaled_holder)
}

shift_calc<-function(data1,data2,comMin,comMax) {
  
  Fitted_black<-Fitting(data1)
  Fitted_green<-Fitting(data2)
  
  sub_Fitted_black<-subset(Fitted_black, Ph>=comMin() & Ph<=comMax())
  sub_Fitted_green<-subset(Fitted_green, Ph>=comMin() & Ph<=comMax())
  Sh<-sum(sub_Fitted_green$Tm-sub_Fitted_black$Tm)/(comMax()-comMin())
  return(Sh)
}


shift_calc_fitted<-function(fit1,fit2,comMin,comMax) {
  
  if ((comMax-comMin)>=20) { 
    sub_Fitted_black<-subset(fit1, Ph>=comMin & Ph<=comMax)
    sub_Fitted_green<-subset(fit2, Ph>=comMin & Ph<=comMax)
    Sh<-sum(sub_Fitted_green$Tm-sub_Fitted_black$Tm)/(comMax-comMin)
  } else Sh<-NA
  return(Sh)
}

BuildingData<-function(MeasurementTable,RowNr){
  NameReg1 = as.character(MeasurementTable[RowNr,8])
  NameReg2 = as.character(MeasurementTable[RowNr,10])
  
  datapath_t1=as.character(MeasurementTable[RowNr,5])
  datapath_e=as.character(MeasurementTable[RowNr,6])
  datapath_r1=as.character(MeasurementTable[RowNr,7])
  datapath_r2=as.character(MeasurementTable[RowNr,9])
  
  t1<-as.matrix(read.csv(datapath_t1,header=FALSE, sep = "",as.is=TRUE))
  e<-as.matrix(read.csv(datapath_e,header=FALSE, sep = "",as.is=TRUE))
  if (NameReg1!=""){
    r1<-as.matrix(read.csv(datapath_r1,header=FALSE, sep = "",as.is=TRUE))
  } else  { r1<-e 
         #r1[]<-0 
          }
  
  if (NameReg2!=""){
    r2<-as.matrix(read.csv(datapath_r2,header=FALSE, sep = "",as.is=TRUE))
  } else { r2<-e
         #r2[]<-0
        }
  #combine, if dataframes are the same size
  if ((dim(e) == dim(t1)) && (dim(r2) == dim(t1)) && (dim(r2) == dim (t1))){ 
  ARR=abind(t1,e,r1,r2,along = 3)
  ARR[,,3:4]=ifelse(ARR[,,3:4]>0,1,0)
  flat=as.data.frame(as.matrix(ftable(ARR)))
  colnames(flat)=c("t1","e","r1","r2")
  flatData=subset(flat,t1!=0 & e!=0) 
  } else
  {flatData<-NULL}
  return(flatData)
}

