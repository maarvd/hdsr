#' Subset afvoershape of waterbalance of interest
#'
#'
#' @param sf shapefile containing all afvoergebieden
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#'
#' @export
create_afvoershape <- function(sf, waterbalance){
  #load afvoer.shape
  afvoer.shape <- sf |> st_transform(28992) |> as.data.table()

  #convert filename to waterbalans.id
  waterbalans.id <- sapply(strsplit(filename, "_"), `[`, 2)
  waterbalans.id <- str_pad(waterbalans.id, 4, pad = "0")

  #select rel shape
  rel.shape <- afvoer.shape[CODE == paste0("AFVGEB", waterbalans.id)] |> st_as_sf()

  #save shape
  st_write(rel.shape, dsn = paste0("output/", waterbalance, "/spatial/shape.gpkg"), append = FALSE)
}

