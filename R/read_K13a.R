#'Read the wind data from HKW
#'@description Function to read the wind speed and direction from the HKZ lidar. Data can be
#'downloaded from \url{https://www.windopzee.net/meet-locaties/hollandse-kust-zuid-hkz/data/}.
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement (63m,91m,116m,141m,166m,191m,216m,241m,266m,291m).
#'@param what choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).
#'@importFrom rlang .data
#'@author Marieke Dirksen
#'@export
read_K13A<-function(dir="D:/data/Lidar/K13/",h=60,what="Speed"){
  # I<-df[,grep("*_Altitude$*", colnames(df))]
  # heights<-subset(df,select=I)
  # heights<-gather(heights,"h","val")

  h_opt<-c(63,91,116,141,166,191,216,241,266,291)
  # names(df_height)<-c("nr","height")
  h_exists<-h %in% h_opt
  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 63,91,116,141,166,191,216,241,266 or 291")
    return(FALSE)
  }

  hkz<-list.files(dir,pattern="*K13*",
                  recursive = TRUE,full.names = TRUE)
  df<-do.call("rbind",lapply(hkz,function(x){data.table::fread(x,skip=1,header=TRUE)}))
  df<-df[-1,]

  t.vec<-df$`Timestamp(UTC)`
  t.vec<-gsub("[A-Z]"," ",t.vec)
  t.vec<-as.POSIXct(t.vec,format="%Y-%m-%d %H:%M:%S ") #



  if(what=="Speed"){
    clmn.I<-df[,grep("*WsHor_avg$*", colnames(df))]
    u<-subset(df,select=clmn.I) #.data$

    heights<-gsub("K13A_H","",names(u))
    heights<-as.numeric(gsub("[^0-9.-]", "", heights))

    I.h<-which(heights==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","u","h")
      df_h$u<-as.numeric(df_h$u)
    }

    if(length(I.h)==2){
      message("Two columns with this height level, omitting the non-configurable reference height")
      I.ref<-grep(".*refh.*",colnames(u)[I.h])
      hg<-subset(u,select=I.h)
      hg<-subset(hg,select=-I.ref)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","u","h")
    }
    df_h<-df_h[stats::complete.cases(df_h),]
    return(df_h)
  }

  if(what=="Direction"){
    I<-df[,grep("*_Wd$", colnames(df))]
    u<-subset(df,select=I) #.data$

    heights<-gsub("K13A_H","",names(u))
    heights<-as.numeric(gsub("[^0-9.-]", "", heights))

    I.h<-which(heights==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","dir","h")
      df_h$dir<-as.numeric(df_h$dir)
    }

    if(length(I.h)==2){
      message("Two columns with this height level, omitting the non-configurable reference height")
      I.ref<-grep(".*refh.*",colnames(u)[I.h])
      hg<-subset(u,select=I.h)
      hg<-subset(hg,select=-I.ref)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","u","h")
    }
    df_h<-df_h[stats::complete.cases(df_h),]
    return(df_h)

  }
}
