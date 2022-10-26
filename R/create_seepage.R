#' Mask seepage raster to area waterbalance of interest (run create_afvoershape first)
#'
#'
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @importFrom sf st_read st_bbox st_transform
#'
#' @export
create_seepage <- function(waterbalance){
  #read geopackage
  sf <- st_read(paste0("output/", waterbalance, "/spatial/shape.gpkg"))

  #transform to 28992 to be sure
  sf.transformed <- st_transform(sf, 28992)

  #create bbox
  bbox <- st_bbox(sf.transformed) |> paste0(collapse = ",")

  #seepage url (LHM4.1 kwel-inzijging 2011-2018)
  seep_url <- paste0("https://modeldata-nhi-data.deltares.nl/geoserver/LHM/ows?",
                     "service=WCS&version=1.0.0",
                     "&request=GetCoverage",
                     "&coverage=LHM%3Akwel_mmd",
                     "&bbox=", bbox,
                     "&width=1200",
                     "&height=1300",
                     "&crs=EPSG%3A28992",
                     "&format=geotiff")

  #save tiff
  download.file(url = seep_url,
                destfile = paste0("output/", waterbalance, "/spatial/LHM4.1_seepinf_2011-2018_mmday.tiff"))



}


