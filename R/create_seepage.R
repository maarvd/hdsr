#' Mask seepage raster to area waterbalance of interest (run create_afvoershape first)
#'
#'
#' @param rast raster of LHM4.1 seepage
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @importFrom sf st_read
#' @importFrom terra rast crop vect mask writeRaster
#'
#' @export
create_seepage <- function(rast, waterbalance){
  #read shape
  aoi <- st_read(paste0("output/", waterbalance, "/spatial/shape.gpkg"))

  #load seepage
  seep <- terra::rast(rast)

  #crop
  seep <- terra::crop(seep, terra::vect(aoi))

  #mask
  seep <- terra::mask(seep, terra::vect(aoi))

  #write
  terra::writeRaster(seep, filename = paste0("output/", waterbalance, "/spatial/seepage.tiff"), overwrite = TRUE)
}
