#'Read the wind data from ANH
#'@description Function to read the wind speed and direction from the ANH lidar. Data can be
#'requested from \url{https://offshorewind.rvo.nl/windwaternh}.
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement (40m,60m,76m,80m,100m,116m,160m,200m,250m,290m).
#'@param what choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).
#'@author Marieke Dirksen
#'@export
read_ANH<-function(dir="D:/data/Lidar/ANH/",h=60,what="Speed"){
  # I<-df[,grep("*_Altitude$*", colnames(df))]
  # heights<-subset(df,select=I)
  # heights<-gather(heights,"h","val")

  h_opt<-c(40,60,76,80,100,116,160,200,250,290)
  h_nr<-seq(1,10,by=1)
  df_height<-data.frame(h_nr,h_opt)
  names(df_height)<-c("nr","height")
  h_exists<-h %in% h_opt
  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 40,60,76,80,100,116,160,200,250 or 290")
    return(FALSE)
  }
  anh<-list.files(dir,pattern="*\\.csv$",recursive = TRUE,full.names = TRUE)
  df<-do.call("rbind",lapply(anh,function(x){data.table::fread(x)}))
  t.vec<-df$DataTimeStamp
  t.vec<-gsub("[A-Z]"," ",t.vec)
  t.vec<-as.POSIXct(t.vec,format="%Y-%m-%d %H:%M:%OS ") #



  if(what=="Speed"){
    I<-df[,grep("*_WindSpeed$*", colnames(df))]
    u<-subset(df,select=.data$I)

    # heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

    I.h<-which(df_height$height==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","u","h")
      df_h$u<-as.numeric(df_h$u)
    }else{
      message("multiple columns selected, returning FALSE")
      return(FALSE)
    }
    df_h<-df_h[stats::complete.cases(df_h),]
    return(df_h)
  }

  if(what=="Direction"){
    I<-df[,grep("*_WindDirection$", colnames(df))]
    u<-subset(df,select=.data$I)

    # heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

    I.h<-which(df_height$height==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","dir","h")
      df_h$dir<-as.numeric(df_h$dir)
    } else{
      message("multiple columns selected, returning FALSE")
      return(FALSE)
    }
    df_h<-df_h[stats::complete.cases(df_h),]
    return(df_h)

  }
}
