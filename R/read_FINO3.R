<<<<<<< HEAD
#'Read the wind data from FINO towers
=======
#'Read the wind data from FINO2
>>>>>>> 8fc5b7b127cb858cb4ac9f14cf110a8346189fb3
#'@description Function to read the wind speed and direction from the FINO2 lidar. Data can be
#'requested from \url{https://orsted.com/en/our-business/offshore-wind/wind-data}.
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement (63m,91m,116m,141m,166m,191m,216m,241m,266m,291m).
#'@param what choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).
#' @importFrom rlang .data
#'@author Marieke Dirksen
#'@export
<<<<<<< HEAD
read_FINO<-function(dir="D:/data/Lidar/",stn="FINO1",h=69,what="Speed"){
  if(stn=="FINO1"){
  dir<-paste0(dir,stn)
  h_opt<-c(31,41,51,61,71,81,91,101,107)

    h_exists<-h %in% h_opt
      if(h_exists==FALSE){
        message("Height level not included returning FALSE, try 31,41,51,61,71,81,91,101 or 107")
      return(FALSE)
      }
  }else if(stn=="FINO2"){
  dir<-paste0(dir,stn)
  h_opt<-c(49,59,69,79,89,107,127,147,187,227,267)

    h_exists<-h %in% h_opt
      if(h_exists==FALSE){
      message("Height level not included returning FALSE, try 49,59,69,79,89,107,127,147,187,227 or 267")
      return(FALSE)
    }
  }else if(stn=="FINO3"){
  dir<-paste0(dir,stn)
  h_opt<-c(49,59,69,79,89,107,127,147,187,227,267)

    h_exists<-h %in% h_opt
      if(h_exists==FALSE){
      message("Height level not included returning FALSE, try 49,59,69,79,89,107,127,147,187,227 or 267")
      return(FALSE)
    }
  }else{message("station name not used, please try FINO1,FINO2 or FINO3. returning FALSE")
    return(FALSE)
    }


=======
read_FINO3<-function(dir="D:/data/Lidar/FINO3/",h=69,what="Speed"){
  h_opt<-c(49,59,69,79,89,107,127,147,187,227,267)
  h_exists<-h %in% h_opt
  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 49,59,69,79,89,107,127,147,187,227 or 267")
    return(FALSE)
    }

>>>>>>> 8fc5b7b127cb858cb4ac9f14cf110a8346189fb3
  df$Date<-as.character(df$Date)
  # t.vec<-gsub("[A-Z]"," ",t.vec)
  df$Date<-as.POSIXct(df$Date,format="%Y%m%d%H%M") #

  if(what=="Speed"){
  #what is the difference between windspeed usa and cup?
<<<<<<< HEAD
  fino3<-list.files(dir,pattern=paste0("FINO3_windspeed_cup_",h,"*"),recursive = TRUE,full.names = TRUE)
=======
  fino3<-list.files(dir,pattern="FINO3_windspeed_cup_*",recursive = TRUE,full.names = TRUE)
>>>>>>> 8fc5b7b127cb858cb4ac9f14cf110a8346189fb3

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
