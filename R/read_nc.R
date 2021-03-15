#'@title Load nc file
#'@desciption Load nc file with meteorological variables from WINS50
#'@param nc_file name and full path to the file
#'@return a data frame with obs
#'@export
read_nc<-function(nc_file){
  t0<-as.POSIXct("2018-12-01")
  nc_data<-nc_open(nc_file)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  h <- ncvar_get(nc_data,"height")
  t <- ncvar_get(nc_data, "time") #days since 2015-01-01
  t <- t0+(t*24*60*60)

  ua<-ncvar_get(nc_data,"ua") #eastward wind
  va<-ncvar_get(nc_data,"va") #northward wind
  ta <- ncvar_get(nc_data, "ta") #actual temperature
  reh <- ncvar_get(nc_data, "hur") #relative humidity
  p <- ncvar_get(nc_data,"p") #pressure
  tke<-ncvar_get(nc_data,"tke") #turbulent kinetic energy

  ua<-data.table(ua); va<-data.table(va); ta<-data.table(ta)
  reh<-data.table(reh);p<-data.table(p); tke<-data.table(tke);

  row.names(ta)<-h;row.names(ua)<-h;row.names(va)<-h
  row.names(reh)<-h;row.names(p)<-h; row.names(tke)<-h;

  df_ta<-gather_matrix(ta); names(df_ta)<-c("height","time","Ta")
  df_ua<-gather_matrix(ua); names(df_ua)<-c("height","time","u")
  df_va<-gather_matrix(va); names(df_va)<-c("height","time","v")
  df_reh<-gather_matrix(reh); names(df_reh)<-c("height","time","reh")
  df_p<-gather_matrix(p); names(df_p)<-c("height","time","P")
  df_tke<-gather_matrix(tke); names(df_tke)<-c("height","time","tke")

  dfs<-list(df_ta,df_ua,df_va,df_reh,df_p,df_tke)

  merge.all <- function(x, y) {
    merge(x, y, all=TRUE, by=c("height","time"))
  }
  df_harm<-Reduce(merge.all,dfs)
  df_harm$ws<-sqrt(df_harm$u^2+df_harm$v^2)
  df_harm$dir<-base::atan2(df_harm$u,df_harm$v)*(180/pi)+180
  return(df_harm)
}

#'Load the fluxes of the nc file
#'@desciption Load nc file with meteorological variables from WINS50
#'@param nc_file name and full path to the file
#'@return a data frame with obs
#'@export
read_flux_nc<-function(nc_file){
  t0<-as.POSIXct("2018-12-01")
  nc_data<-nc_open(nc_file)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  h<-ncvar_get(nc_data,"height")
  t <- ncvar_get(nc_data, "time") #days since 2015-01-01
  t <- t0+(t*24*60*60)

  hfls_eva<-ncvar_get(nc_data,"hfls_eva") #latent heat from evaporation [J/m2]
  hfls_sbl <- ncvar_get(nc_data, "hfls_sbl") #latent heat from sublimation
  hfss <- ncvar_get(nc_data, "hfss") #sensible heat
  tauu<-ncvar_get(nc_data,"tauu") #eastward wind stress
  tauv<-  ncvar_get(nc_data,"tauv") #northward wind stress

  hfls_eva<-data.table(hfls_eva);
  hfls_sbl<-data.table(hfls_sbl); hfss<-data.table(hfss)

  df_harm<-cbind(hfls_eva,hfls_sbl,hfss,tauu,tauv,t)
  return(df_harm)
}
