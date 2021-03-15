#'Read wind mast data from the North Sea
#'@description Function to read the wind speed and direction from the wind masts. Data can be
#'downloaded from KDP platform. The files include wind - windspeed, direction,
#'standarddeviation at a 10 minute interval.
#'
#'The original data structure of the files:
#'\itemize{
#' \item{FF_10M_10: }{is wind snelheid gem. zee herleid -> 10 m land hoogte sensor 10' eenheid m/s}
#' \item{DD_10: }{is wind richting gem. hoogte sensor 10' eenheid graad}
#' \item{DDN_10: }{is wind richting min. gem. hoogte sensor 10' eenheid graad}
#' \item{DD_STD_10: }{is wind richting std. dev. hoogte sensor 10' eenheid graad}
#' \item{DDX_10: }{is wind richting max. gem. hoogte sensor 10' eenheid graad}
#' \item{FF_SENSOR_10: }{is wind snelheid gemiddelde hoogte sensor 10' eenheid m/s}
#' \item{FF_10M_STD_10: }{is wind snelh. std. dev. herleid 10 m 10' eenheid m/s}
#' \item{FX_10M_10: }{is wind snelh. werkelijk max. zee herleid -> 10 m land hoogte sensor 10' eenheid m/s}
#' \item{FX_10M_MD_10: }{is wind snelh. zee : werkelijk max. land : marked discontinuity max. 10' eenheid m/s}
#' \item{FX_SENSOR_10: }{is wind snelheid werkelijk max. hoogte sensor 10' eenheid m/s}
#' \item{FX_SENSOR_MD_10: }{is wind snelh. luchtvaart max. hoogte sensor 10' eenheid m/s}
#' \item{SQUALL_10: }{is squall indicator eenheid boolean}
#'}
#'@param dir directory of the files (allowed to be stored in subfolders).
#'@param h height of the measurement.
#'@param stn station name (P11-B,AWG1,IJmond,B6-OHVS2,F3N,K13,Huibertgat,Oosterschelde,Vlakte_van_de_Raan,Lichteiland-Goeree,Europlatform)
#'@author Marieke Dirksen
#'@export
read_windmasts<-function(dir="D:/data/Lidar/windmasts_NS/",stn="P11-B"){
  # meta<-fread(paste0(dir,"metadata/coords_masts.csv"))
  # names(meta)<-c("name","lat","lon")
  # fls<-list.files(path=dir,pattern=".csv",full.names = TRUE)
stations_in<-c("P11-B","Oosterschelde","Vlakte_van_de_Raan","IJmond",
               "F3","AWG1","Huibertgat","K13","Lichteiland-Goeree",
               "Europlatform","B6-OHVS2")

stn_exists<-stn %in% stations_in
if(stn_exists==FALSE){
  message("station name not included returning FALSE, try P11-B,AWG1,IJmond,B6-OHVS2,F3N,K13,Huibertgat,Oosterschelde,Vlakte_van_de_Raan,Lichteiland-Goeree or Europlatform")
  return(FALSE)
}
  #select one of the files(nr of columns differs per file)
  #get the names out of the meta file
  df<-fread(paste0(dir,stn,".csv"))
  df$IT_DATETIME<-as.POSIXct(df$IT_DATETIME,format="%Y%m%d_%H%M%S_00000")
  df<-subset(df,select=c("IT_DATETIME","TOW.DD_10","TOW.FF_SENSOR_10"))
  names(df)<-c("time","dir","u")

  if(stn=="P11-B"){
    df$h=51.37}
  if(stn=="Oosterschelde" | stn=="Vlakte_van_de_Raan" | stn=="IJmond"){
    df$h=16.5}
  if(stn=="F3" | stn=="AWG1"){
    df$h=60}
  if(stn=="Huibertgat"){
    df$h=18}
  if(stn=="K13"){
    df$h=73.8}
  if(stn=="Lichteiland-Goeree"){
    df$h=38.3}
  if(stn=="Europlatform"){
    df$h=29.1}
  if(stn=="B6-OHVS2"){
    df$h=50.46}
  #set time to POSTixct
  #select ws and wd columns
  df<-df[complete.cases(df),]
  return(df)
}
