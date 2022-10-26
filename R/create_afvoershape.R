#' Subset afvoershape of waterbalance of interest
#'
#'
#' @param sf shapefile containing all afvoergebieden (retrieve from data(afvoergebieden))
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @importFrom sf st_transform st_as_sf st_write
#'
#' @export
create_afvoershape <- function(sf, waterbalance){
  #load afvoer.shape
  afvoer.shape <- sf |> st_transform(28992) |> as.data.table()

  #convert filename to waterbalans.id
  waterbalans.id <- sapply(strsplit(waterbalance, "_"), `[`, 2)
  waterbalans.id <- paste0("KRW_", waterbalans.id)

  #select rel shape
  rel.shape <- afvoer.shape[afgb_id == waterbalans.id] |> st_as_sf()

  #yes no
  if(askYesNo(msg = paste0("Shapefile ", rel.shape$naam, " geselecteerd voor waterbalans ", waterbalance, ". Klopt dit?")) == TRUE){
    #save shape
    st_write(rel.shape, dsn = paste0("output/", waterbalance, "/spatial/shape.gpkg"), append = FALSE)
  } else{
    warning(paste0("Geen ruimtelijk bestand opgeslagen voor waterbalans ", waterbalance))
  }

  }

