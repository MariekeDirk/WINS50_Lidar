#'Read the wind data from HKN
#'@description Function to read the wind speed and direction from the HKN lidar. Data can be
#'requested from \url{https://offshorewind.rvo.nl/windwaternh}.
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement (4m,30m,40m,60m,80m,100m,120m,140m,160m,180m,200m).
#'@param what choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).
#'@param stn choose between HKNA and HKNB lidar locations.
#'@author Marieke Dirksen
#' @importFrom rlang .data
#'@export
read_HKN<-function(dir="D:/data/Lidar/HKN",h=60,what="Speed",stn="HKNA"){
  # I<-df[,grep("*_Altitude$*", colnames(df))]
  # heights<-subset(df,select=I)
  # heights<-gather(heights,"h","val")

  h_opt<-c(4,30,40,60,80,100,120,140,160,180,200)
  # names(df_height)<-c("nr","height")
  h_exists<-h %in% h_opt
  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 4,30,40,60,80,100,120,140,160,180 or 200")
    return(FALSE)
  }

  lidar_opt<-c("HKNA","HKNB")
  lid_exists<-stn %in% lidar_opt
  if(lid_exists==FALSE){
    message("Lidar station name unknown returning FALSE, try HKNA or HKNB")
    return(FALSE)
  }

  hkn<-list.files(dir,pattern="*WindResourceSpeedDirectionTIStat_F\\.csv$",
                  recursive = TRUE,full.names = TRUE)
  hkn.I<-hkn[grep(stn,hkn)]

  df<-do.call("rbind",lapply(hkn.I,function(x){data.table::fread(x)}))
  t.vec<-df$`TIMESTAMP (ISO-8601) UTC`
  t.vec<-gsub("[A-Z]"," ",t.vec)
  t.vec<-as.POSIXct(t.vec,format="%Y-%m-%d %H:%M:%S ") #



  if(what=="Speed"){
    I<-df[,grep("*WindSpeed*", colnames(df))]
    u<-subset(df,select=I) #.data$

    heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

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
    I<-df[,grep("*WindDir*", colnames(df))]
    u<-subset(df,select=I) #.data$

    heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

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
      I.ref<-grep(".*ref*",colnames(u)[I.h])
      hg<-subset(u,select=I.h)
      hg<-subset(hg,select=-I.ref)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","dir","h")
    }
    df_h<-df_h[stats::complete.cases(df_h),]
    return(df_h)

  }
}
