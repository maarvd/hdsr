#' Subset afvoershape of waterbalance of interest
#'
#'
#' @param sf shapefile containing all afvoergebieden (retrieve from data(afvoergebieden))
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @import ggplot2
#' @importFrom ggspatial annotation_map_tile
#' @importFrom sf st_transform st_as_sf st_write
#' @importFrom ggspatial annotation_map_tile
#' @importFrom gridExtra grid.arrange
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

  #visualize
  plot1 <- ggplot(afvoergebieden) + geom_sf(fill = "white") + theme_void() +
    geom_sf(data = rel.shape, fill = "red")

  plot2 <- ggplot(rel.shape) + ggspatial::annotation_map_tile() + geom_sf(fill = NA, col = "red", linetype = "dashed", size = 1) + theme_void()

  #write files
  #shape
  st_write(rel.shape, dsn = paste0("output/", waterbalance, "/spatial/shape.gpkg"), append = FALSE)

  #plots
  ggsave(filename = paste0("output/", waterbalance, "/spatial/shape overzicht.png"), plot = plot1, height = 20, width = 20, units = "cm")
  ggsave(filename = paste0("output/", waterbalance, "/spatial/shape detail.png"), plot = plot2, height = 20, width = 20, units = "cm")

}
