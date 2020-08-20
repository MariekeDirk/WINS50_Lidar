#'Read the wind data from FINO2
#'@description Function to read the wind speed and direction from the FINO2 lidar. Data can be
#'requested from \url{https://orsted.com/en/our-business/offshore-wind/wind-data}.
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement (63m,91m,116m,141m,166m,191m,216m,241m,266m,291m).
#'@param what choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).
#' @importFrom rlang .data
#'@author Marieke Dirksen
#'@export
read_FINO3<-function(dir="D:/data/Lidar/FINO3/",h=69,what="Speed"){
  h_opt<-c(49,59,69,79,89,107,127,147,187,227,267)
  h_exists<-h %in% h_opt
  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 49,59,69,79,89,107,127,147,187,227 or 267")
    return(FALSE)
    }

  df$Date<-as.character(df$Date)
  # t.vec<-gsub("[A-Z]"," ",t.vec)
  df$Date<-as.POSIXct(df$Date,format="%Y%m%d%H%M") #

  if(what=="Speed"){
  #what is the difference between windspeed usa and cup?
  fino3<-list.files(dir,pattern="FINO3_windspeed_cup_*",recursive = TRUE,full.names = TRUE)

  df<-fread(fino3[1],na.strings = "-99.999",skip=6,sep = "\t")
  names(df)<-c("Time","Value","Minimum","Maximum","Deviation","Quality")
  # df<-do.call("cbind",lapply(fino3,function(x){data.table::fread(x)}))


  df_h<-subset(df,select=c("Date","u","Height"))
  df_h<-df_h[which(df_h$Height==h),]
  names(df_h)<-c("time","u","h")
  df_h$u<-as.numeric(df_h$u)
  }
  if(what=="Direction"){
  df_h<-subset(df,select=c("Date","Dir","Height"))
  df_h<-df_h[which(df_h$Height==h),]
  names(df_h)<-c("time","dir","h")
  df_h$dir<-as.numeric(df_h$dir)
  }


  df_h<-df_h[stats::complete.cases(df_h),]
  return(df_h)


}
