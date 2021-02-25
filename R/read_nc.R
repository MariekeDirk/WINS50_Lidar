#'Load nc file
#'@desciption Load nc file with meteorological variables from WINS50
#'@param nc_file name and full path to the file
#'@return a data frame with obs
#'@export
read_nc<-function(nc_file){
  nc_data<-nc_open(nc_file)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  h<-ncvar_get(nc_data,"height")
  t <- ncvar_get(nc_data, "time") #days since 2015-01-01
  t <- t0+(t*24*60*60)

  ua<-ncvar_get(nc_data,"ua") #eastward wind
  va<-ncvar_get(nc_data,"va") #northward wind
  ta <- ncvar_get(nc_data, "ta") #actual temperature
  reh <- ncvar_get(nc_data, "hur") #relative humidity
  p <- ncvar_get(nc_data,"p") #pressure

  ua<-data.table(ua); va<-data.table(va); ta<-data.table(ta)
  reh<-data.table(reh);p<-data.table(p)

  row.names(ta)<-h;row.names(ua)<-h;row.names(va)<-h
  row.names(reh)<-h;row.names(p)<-h

  df_ta<-gather_matrix(ta); names(df_ta)<-c("height","time","Ta")
  df_ua<-gather_matrix(ua); names(df_ua)<-c("height","time","u")
  df_va<-gather_matrix(va); names(df_va)<-c("height","time","v")
  df_reh<-gather_matrix(reh); names(df_reh)<-c("height","time","reh")
  df_p<-gather_matrix(p); names(df_p)<-c("height","time","P")

  dfs<-list(df_ta,df_ua,df_va,df_reh,df_p)

  merge.all <- function(x, y) {
    merge(x, y, all=TRUE, by=c("height","time"))
  }
  df_harm<-Reduce(merge.all,dfs)
  df_harm$ws<-sqrt(df_harm$u^2+df_harm$v^2)
  return(df_harm)
}
