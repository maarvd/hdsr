#' Subset afvoershape of waterbalance of interest
#'
#'
#' @param sf shapefile containing all afvoergebieden (retrieve from data(afvoergebieden))
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @import ggplot2
#' @importFrom sf st_transform st_as_sf st_write
#' @importFrom ggspatial annotation_map_tile
#' @importFrom gridExtra grid.arrange
#'
#' @export
create_afvoershape <- function(sf, waterbalance, check){
  #load afvoer.shape
  afvoer.shape <- sf |> st_transform(28992) |> as.data.table()

  #convert filename to waterbalans.id
  waterbalans.id <- sapply(strsplit(waterbalance, "_"), `[`, 2)
  waterbalans.id <- paste0("KRW_", waterbalans.id)

  #select rel shape
  rel.shape <- afvoer.shape[afgb_id == waterbalans.id] |> st_as_sf()

  #if check is desired
  if(check == TRUE){
    #new dev
    print(dev.new(1))

    #visualize
    plot1 <- ggplot(afvoergebieden) + geom_sf(fill = "white") + theme_void() +
      geom_sf(data = rel.shape, fill = "red")

    plot2 <- ggplot(rel.shape) + ggspatial::annotation_map_tile() + geom_sf(fill = NA, col = "red", linetype = "dashed", size = 1) + theme_void()

    gridExtra::grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

    if(askYesNo(msg = paste0("Shapefile ", rel.shape$naam, " geselecteerd voor waterbalans ", waterbalance, ". Klopt dit?")) == TRUE){

      #save shape
      st_write(rel.shape, dsn = paste0("output/", waterbalance, "/spatial/shape.gpkg"), append = FALSE)
    } else{
      warning(paste0("Geen ruimtelijk bestand opgeslagen voor waterbalans ", waterbalance))
    }
  } else if(check == FALSE){
    st_write(rel.shape, dsn = paste0("output/", waterbalance, "/spatial/shape.gpkg"), append = FALSE)
  }
}
