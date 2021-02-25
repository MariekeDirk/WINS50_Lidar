#'Read the wind data from FINO towers
#'@description Function to read the wind speed and direction from the FINO2 lidar. Data can be
#'requested from \url{https://orsted.com/en/our-business/offshore-wind/wind-data}.
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement.
#'@param what choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).
#'@param stn station name FINO1, FINO2 or FINO3
#' @importFrom rlang .data
#'@author Marieke Dirksen
#'@export
read_FINO<-function(dir="D:/data/Lidar/",
                    stn="FINO1",h=41,
                    windspeed="usa",winddir_eq="usa",
                    what="Speed"){

  if(what=="Speed"){
    if(stn=="FINO1" & windspeed=="cup"){
      dir<-paste0(dir,stn)
      h_opt<-c(41,51,61,71,81,91)

      h_exists<-h %in% h_opt
      if(h_exists==FALSE){
        message("Height level not included returning FALSE, try 31,41,51,61,71,81 or 91")
        return(FALSE)
      }
      fino<-list.files(dir,pattern=paste0("*deg_mc_*"),recursive = TRUE,full.names = TRUE)
      fino<-fino[grep(pattern=paste0(h,"m"),fino)]
      df<-fread(fino,na.strings = "-999",skip=6,sep="\t")
    }else if(stn=="FINO2" & windspeed=="cup"){
      dir<-paste0(dir,stn)
      h_opt<-c(32,42,52,62,72,82,92)

      h_exists<-h %in% h_opt
      if(h_exists==FALSE){
        message("Height level not included returning FALSE, try 49,59,69,79,89,107,127,147,187,227 or 267")
        return(FALSE)
      }
      fino<-list.files(dir,pattern=paste0("*deg_mc_*"),recursive = TRUE,full.names = TRUE)
      fino<-fino[grep(pattern=paste0(h,"m"),fino)]
      df<-fread(fino,na.strings = "-999",skip=6,sep="\t")
    }else if(stn=="FINO3" & windspeed=="cup"){
      dir<-paste0(dir,stn)
      h_opt<-c(51,71,91) #mast correctec values (multiple files!)

      h_exists<-h %in% h_opt
      if(h_exists==FALSE){
        message("Height level not included returning FALSE, try 51,71 or 91")
        return(FALSE)
      }
      message("Using the 105 deg mast corrected wind speed, not reading the 225 and 345 deg.")
      fino<-list.files(dir,pattern=paste0("*105deg_mc_*"),recursive = TRUE,full.names = TRUE)
      fino<-fino[grep(pattern=paste0(h,"m"),fino)]
      df<-fread(fino,na.strings = "-99.999",skip=6,sep="\t")
    }else if(stn=="FINO1" & windspeed=="usa"){
    dir<-paste0(dir,stn)
    h_opt<-c(42,62,82) #mast correctec values (multiple files!)

    h_exists<-h %in% h_opt
    if(h_exists==FALSE){
      message("Height level not included returning FALSE, try 42, 62 or 82")
      return(FALSE)
    }
    fino<-list.files(dir,pattern=paste0("*windspeed_usa_*"),recursive = TRUE,full.names = TRUE)
    fino<-fino[grep(pattern=paste0(h,"m"),fino)]
    df<-fread(fino,na.strings = "-999",skip=6,sep="\t")
  }else if(stn=="FINO2" & windspeed=="usa"){
    dir<-paste0(dir,stn)
    if(h==42){
    fino<-list.files(dir,pattern=paste0("*windspeed_usa_42*"),recursive = TRUE,full.names = TRUE)
    }
    if(h==62){
    fino<-list.files(dir,pattern=paste0("*windspeed_usa_62*"),recursive = TRUE,full.names = TRUE)
    }
    if(h==82){
    fino<-list.files(dir,pattern=paste0("*windspeed_usa_82*"),recursive = TRUE,full.names = TRUE)
    }
    fino<-fino[grep(pattern=paste0(h,"m"),fino)]
    df<-fread(fino,na.strings = "-999",skip=6,sep="\t")

  }else if(stn=="FINO3" & windspeed=="usa"){
    message("Using the 105 deg mast corrected wind speed, not reading the 225 and 345 deg.")
    dir<-paste0(dir,stn)
    if(h==61){
    fino<-list.files(dir,pattern=paste0("*windspeed_usa_61*"),recursive = TRUE,full.names = TRUE)
    }
    if(h==101){
    fino<-list.files(dir,pattern=paste0("*windspeed_usa_101*"),recursive = TRUE,full.names = TRUE)
    }
    fino<-fino[grep(pattern=paste0(h,"m"),fino)]
    df<-fread(fino,na.strings = "-99.999",skip=6,sep="\t")
  }else{message("station name not used, please try FINO1,FINO2 or FINO3. returning FALSE")
    return(FALSE)
  }



  names(df)<-c("Time","Value","Minimum","Maximum","Deviation","Quality")
  df$Time<-as.POSIXct(df$Time,format="%Y-%m-%d %H:%M:%S")
  # df<-do.call("cbind",lapply(fino3,function(x){data.table::fread(x)}))


  df_h<-subset(df,select=c("Time","Value"))
  names(df_h)<-c("time","u")
  df_h$u<-as.numeric(df_h$u)
  df_h$h<-h
  }
  if(what=="Direction"){
    if(stn=="FINO3"){
      dir<-paste0(dir,stn)
      message("Code needs to be written for the following files")
      fino<-list.files(dir,pattern=paste0("winddirection*"),recursive = TRUE)
      print(fino)
    }else if(stn=="FINO2"){
      dir<-paste0(dir,stn)
      h_opt<-c(31,51,71,91)
      h_exists<-h %in% h_opt
      if(h_exists==FALSE){
        message("Height level not included returning FALSE, try 31,51,71 or 91")
        return(FALSE)
      }else{
        fino<-list.files(dir,pattern=paste0("winddirection_vane_",h,"m*"),recursive = TRUE,full.names = TRUE)
        df<-fread(fino,na.strings = "-99.999",skip=6,sep="\t")
        names(df)<-c("Time","Direction","Deviation","Quality")
        df$h<-h
        df<-df[which(df$Quality!=9),]
        df_h<-subset(df,select=c("Time","Direction","h"))

      }
    }else if(stn=="FINO1"){
      dir<-paste0(dir,stn)
      message("Code needs to be written for the following files")
      if(winddir_eq=="vane"){
      h_opt<-c(34,51,71,91)
        h_exists<-h %in% h_opt
        if(h_exists==FALSE){
          message("Height level not included returning FALSE, try 34,51,71 or 91")
          return(FALSE)
        }else{
          fino<-list.files(dir,pattern=paste0("winddirection_vane_",h,"*"),recursive = TRUE,
                           full.names=TRUE)
        }
      }
      if(winddir_eq=="usa"){
      h_opt<-c(42,62,82)
        h_exists<-h %in% h_opt
        if(h_exists==FALSE){
          message("Height level not included returning FALSE, try 42,62 or 82")
          return(FALSE)
        }else{
          fino<-list.files(dir,pattern=paste0("winddirection_usa_",h,"*"),recursive = TRUE,
                           full.names=TRUE)
        }
      }

      df<-fread(fino,na.strings = "-999.00",skip=6,sep="\t")
      names(df)<-c("Time","Direction","Deviation","Quality")
      df$h<-h
      df<-df[which(df$Quality!=9),]
      df_h<-subset(df,select=c("Time","Direction","h"))

    }else{
      return(FALSE)
      }


  }


  df_h<-df_h[stats::complete.cases(df_h),]
  return(df_h)
}
