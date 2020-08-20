#'Read the wind data from WMR
#'@description Function to read the wind speed and direction from the WMR lidar. Data can be
#'requested from \url{https://orsted.com/en/our-business/offshore-wind/wind-data}.
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement (4m,30m,40m,60m,80m,100m,120m,140m,160m,180m,200m,290m).
#'@param what choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).
#'@importFrom rlang .data
#'@author Marieke Dirksen
#'@export
read_WMR<-function(dir="D:/data/Lidar/WMR",h=60,what="Speed"){
  # I<-df[,grep("*_Altitude$*", colnames(df))]
  # heights<-subset(df,select=I)
  # heights<-gather(heights,"h","val")

  h_opt<-c(4,30,40,60,80,100,120,140,160,180,200,290)
  h_nr<-seq(1,12,by=1)
  df_height<-data.frame(h_nr,h_opt)
  names(df_height)<-c("nr","height")
  h_exists<-h %in% h_opt

  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 4,30,40,60,80,100,120,140,160,180,200 or 290")
    return(FALSE)
  }

  wmr<-list.files(dir,pattern="*\\.csv$",
                  recursive = TRUE,full.names = TRUE)
  df<-do.call("rbind",lapply(wmr,function(x){data.table::fread(x)}))
  t.vec<-df$DataTimeStamp
  t.vec<-gsub("[A-Z]"," ",t.vec)
  t.vec<-as.POSIXct(t.vec,format="%Y-%m-%d %H:%M:%OS ") #



  if(what=="Speed"){
    C.I<-df[,grep("*WindSpeed$", colnames(df))]
    u<-subset(df,select=C.I) #.data$

    # heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

    I.h<-which(df_height$height==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h) #.data$
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
    message("WMR wind direction corrected for offset, work in progress")
    C.I<-df[,grep("*WindDirection$", colnames(df))]
    u<-subset(df,select=C.I) #.data$
    u$DirectionOffSet<-df$DirectionOffSet

    # heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

    I.h<-which(df_height$height==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h) #.data$
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","dir","h")
      df_h$dir<-as.numeric(df_h$dir)
      df_h$dir<-df_h$dir-as.numeric(u$DirectionOffSet)
      df_h<-df_h[complete.cases(df_h),]
      df_h$dir[df_h$dir<0]<-df_h$dir[df_h$dir<0]+360 #
      df_h$dir[df_h$dir>360]<-df_h$dir[df_h$dir>360]-360
    }
    return(df_h)
  }
    # I<-df[,grep("*WindDir*", colnames(df))]
    # D.offset<-grep("DirectionOffSet",colnames(df))#check offset!
    # u<-subset(df,select=c(I,D.offset))
    #
    # # heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))
    #
    # I.h<-which(df_height$height==h)
    # if(length(I.h)==1){
    #   hg<-subset(u,select=I.h)
    #   correction_term<-u$DirectionOffSet
    #   df_h<-cbind(t.vec,hg)
    #   df_h$h<-h
    #   names(df_h)<-c("time","dir","h")
    #   df_h$dir<-as.numeric(df_h$dir)
    # }
    #
    # if(length(I.h)==2){
    #   message("Two columns with this height level, omitting the non-configurable reference height")
    #   I.ref<-grep(".*refh.*",colnames(u)[I.h])
    #   hg<-subset(u,select=I.h)
    #   hg<-subset(hg,select=-I.ref)
    #   df_h<-cbind(t.vec,hg)
    #   df_h$h<-h
    #   names(df_h)<-c("time","u","h")
    # }}
    df_h<-df_h[stats::complete.cases(df_h),]
    return(df_h)

  }

