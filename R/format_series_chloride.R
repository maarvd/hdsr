#' Format chloride timeseries to Artesia format
#'
#'
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#'
#' @export
format_series_chloride <- function(waterbalance){
  #read
  stof <- readRDS(paste0("output/", waterbalance, "/raw/series_waterkwaliteit.RDS"))

  #select chloride
  cl_meet1 <- stof$meetlocatie1_cl
  cl_meet2 <- stof$meetlocatie2_cl
  cl_in1 <- stof$inlaat1_cl
  cl_in2 <- stof$inlaat2_cl
  cl_in3 <- stof$inlaat3_cl

  #adjust column names
  setnames(cl_meet1, old = "cl (mg/l)", new = paste0("MEETLOCATIE1|", stof$location_names$meetlocatie1))
  setnames(cl_meet2, old = "cl (mg/l)", new = paste0("MEETLOCATIE2|", stof$location_names$meetlocatie2))
  setnames(cl_in1, old = "cl (mg/l)", new = paste0("Inlaat1|", stof$location_names$inlaat1))
  setnames(cl_in2, old = "cl (mg/l)", new = paste0("Inlaat2|", stof$location_names$inlaat2))
  setnames(cl_in3, old = "cl (mg/l)", new = paste0("Inlaat3|", stof$location_names$inlaat3))

  #merge
  cl <- merge.data.table(cl_meet1, cl_meet2, by = "date", all = TRUE)
  cl <- merge.data.table(cl, cl_in1, by = "date", all = TRUE)
  cl <- merge.data.table(cl, cl_in2, by = "date", all = TRUE)
  cl <- merge.data.table(cl, cl_in3, by = "date", all = TRUE)

  #write
  fwrite(cl, file = paste0("output/", waterbalance, "/formatted/series_chloride.csv"))
}
