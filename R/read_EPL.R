#'Read the wind data from EPL
#'@description Function to read the wind speed and direction from the EPL lidar. Data can be
#'requested from \url{https://www.windopzee.net/meet-locaties/europlatform/}.
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement (63m,91m,116m,141m,166m,191m,216m,241m,266m,291m).
#'@param what choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).
#' @importFrom rlang .data
#'@author Marieke Dirksen
#'@export
read_EPL<-function(dir="D:/data/Lidar/EPL/",h=63,what="Speed"){
  h_opt<-c(63,91,116,141,166,191,216,241,266,291)
  h_exists<-h %in% h_opt
  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 63,91,116,141,166,191,216,241,266 or 291")
    return(FALSE)
    }
  epl<-list.files(dir,recursive = TRUE,full.names = TRUE)
  df<-do.call("rbind",lapply(epl,function(x){data.table::fread(x,skip=1)}))
  df<-df[-1,]
  t.vec<-df$`Timestamp(UTC)`
  t.vec<-gsub("[A-Z]"," ",t.vec)
  t.vec<-as.POSIXct(t.vec,format="%Y-%m-%d %H:%M:%OS ") #

  if(what=="Speed"){
    I.clms<-df[,grep("*WsHor_avg$*", colnames(df))]
    u<-subset(df,select=I.clms) #.data$

    heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

    I.h<-which(heights==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","u","h")
      df_h$u<-as.numeric(df_h$u)
      df_h[df_h$u==9999]<-NA
    }else{
      message("multiple columns selected, returning FALSE")
      return(FALSE)
    }
    df_h<-df_h[stats::complete.cases(df_h),]
  return(df_h)
  }

  if(what=="Direction"){
    I<-df[,grep("*Wd$", colnames(df))]
    u<-subset(df,select=I) #.data$

    heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

    I.h<-which(heights==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","dir","h")
      df_h$dir<-as.numeric(df_h$dir)
      df_h[df_h$dir==9999]<-NA
    } else{
      message("multiple columns selected, returning FALSE")
      return(FALSE)
    }
    df_h<-df_h[stats::complete.cases(df_h),]
   return(df_h)

  }
}
