#' Format phosphorus timeseries to Artesia format
#'
#'
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#'
#' @export
format_series_phosphorus <- function(waterbalance){
  #read
  stof <- readRDS(paste0("output/", waterbalance, "/raw/series_waterkwaliteit.RDS"))

  #select fosfor
  p_meet1 <- stof$meetlocatie1_p
  p_meet2 <- stof$meetlocatie2_p
  p_in1 <- stof$inlaat1_p
  p_in2 <- stof$inlaat2_p
  p_in3 <- stof$inlaat3_p

  #adjust column names
  setnames(p_meet1, old = "p (mg/l)", new = paste0("MEETLOCATIE1|", stof$location_names$meetlocatie1))
  setnames(p_meet2, old = "p (mg/l)", new = paste0("MEETLOCATIE2|", stof$location_names$meetlocatie2))
  setnames(p_in1, old = "p (mg/l)", new = paste0("Inlaat1|", stof$location_names$inlaat1))
  setnames(p_in2, old = "p (mg/l)", new = paste0("Inlaat2|", stof$location_names$inlaat2))
  setnames(p_in3, old = "p (mg/l)", new = paste0("Inlaat3|", stof$location_names$inlaat3))

  #merge
  p <- merge.data.table(p_meet1, p_meet2, by = "date", all = TRUE)
  p <- merge.data.table(p, p_in1, by = "date", all = TRUE)
  p <- merge.data.table(p, p_in2, by = "date", all = TRUE)
  p <- merge.data.table(p, p_in3, by = "date", all = TRUE)

  #write
  fwrite(p, file = paste0("output/", waterbalance, "/formatted/series_fosfor.csv"))
}
