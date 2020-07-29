#' Read the wind speed from BWFZ
#' @description Reads the wind speed and direction from BWFZ at an existing measurement height. The data can be
#' downloaded from \url{https://offshorewind.rvo.nl/studiesborssele}. Note that the non-configurable reference
#' height of 40m is ignored during the height selection.
#' @param dir Directory where the wind files are stored.
#' @param h measurement height including 4m,30m, 40m, 60m, 80m, 100m, 120m, 140m, 160m, 180m and 200m.
#' @param what choose between Speed and Direction to get the wind speed (horizontal sqrt(u^2+v^2)) or wind direction.
#' @author Marieke Dirksen
#' @export
read_BWFZ<-function(dir,h=40,what="Speed"){
  h_opt<-c(4,30,40,60,80,100,120,140,160,180,200)
  h_exists<-h %in% h_opt
  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 4,30,40,60,80,100,120,140,160,180 or 200")
    return(FALSE)
  }

  bwfz<-list.files(dir,pattern="WindResourceSpeedDirectionTIStat",
                   recursive = TRUE,full.names = TRUE)
  df<-do.call("rbind",lapply(bwfz,data.table::fread))
  t.vec<-df$`TIMESTAMP (ISO-8601) UTC`
  t.vec<-gsub("[A-Z]"," ",t.vec)
  t.vec<-as.POSIXct(t.vec,format="%Y-%m-%d %H:%M:%S ") #,

  if(what=="Speed"){
  I<-df[,grep("^WindSpeed.*", colnames(df))]
  u<-subset(df,select=.data$I)

  heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

  I.h<-which(heights==h)
  if(length(I.h)==1){
  hg<-subset(u,select=I.h)
  df_h<-cbind(t.vec,hg)
  df_h$h<-h
  names(df_h)<-c("time","u","h")
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
    I<-df[,grep("^WindDir.*", colnames(df))]
    u<-subset(df,select=.data$I)

    heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

    I.h<-which(heights==h)
    if(length(I.h)==1){
      hg<-subset(u,select=I.h)
      df_h<-cbind(t.vec,hg)
      df_h$h<-h
      names(df_h)<-c("time","dir","h")
    }
    if(length(I.h)==2){
      message("Two columns with this height level, omitting the non-configurable reference height")
      I.ref<-grep(".*ref.*",colnames(u)[I.h])
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
