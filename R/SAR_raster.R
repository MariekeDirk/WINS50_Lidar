#'Raster from SAR
#'@description Create a regular lat lon grid from the SAR points data
#'@param nc_file nc file from SAR data
#'@param land_mask sf polygon to mask the land areas (for example the naturalearth dataset)
#'@param r_res resolution of the raster grid in meters
#'@author Marieke Dirksen
#'@importFrom stars read_ncdf
#'@importFrom sf st_as_sf st_bbox
#'@importFrom raster rasterize subset extent
#'@importFrom sp CRS
#'@export
#'
SAR_raster<-function(nc_file,land_mask=europe_sf,r_res=500){
  #read the data from SAR
  SAR_ws = stars::read_ncdf(nc_file,
                     curvilinear = c("longitude", "latitude"),
                     var=c("sar_wind","mask"),
                     ignore_bounds = TRUE)
  SAR_pnt<-sf::st_as_sf(SAR_ws, as_points = TRUE, merge = FALSE)
  SAR_pnt$mask<-as.numeric(SAR_pnt$mask)
  SAR_pnt<-SAR_pnt[SAR_pnt$mask==-1,]
  # mtch<-sf::st_intersects(SAR_pnt,europe_sf)
  # I <- apply(mtch,1,any)#select land areas
  I<-sapply(sf::st_intersects(SAR_pnt, land_mask),function(x){length(x)==0})
  SAR_pnt<-SAR_pnt[I,]
  SAR_pnt<-sf::st_transform(SAR_pnt,crs=CRS("+init=epsg:28992"))
  #crop the points to an extent
  bb<-sf::st_bbox(SAR_pnt)
  # bb<-st_bbox(c(xmin=2.7,ymin=51.46,xmax=3.4,ymax=51.85),crs=CRS("+init=epsg:4326"))
  # SAR_bb<-st_crop(SAR_pnt,bb,na.rm=TRUE)

  #create a raster object for the area

  # if(exists("r")){
  # raster_template<-r
  # }else{
  raster_template = raster::raster(raster::extent(SAR_pnt), resolution = r_res,
                                     crs = sp::CRS("+init=epsg:28992"))
  # }

  r<-raster::rasterize(SAR_pnt,raster_template,fun=mean)
  r<-raster::subset(r,2)
  return(r)
}


