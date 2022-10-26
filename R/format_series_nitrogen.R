#' Format nitrogen timeseries to Artesia format
#'
#'
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#'
#' @export
format_series_nitrogen <- function(waterbalance){
  #read
  stof <- readRDS(paste0("output/", waterbalance, "/raw/series_waterkwaliteit.RDS"))

  #select fosfor
  n_meet1 <- stof$meetlocatie1_n
  n_meet2 <- stof$meetlocatie2_n
  n_in1 <- stof$inlaat1_n
  n_in2 <- stof$inlaat2_n
  n_in3 <- stof$inlaat3_n

  #adjust column names
  setnames(n_meet1, old = "n (mg/l)", new = paste0("MEETLOCATIE1|", stof$location_names$meetlocatie1))
  setnames(n_meet2, old = "n (mg/l)", new = paste0("MEETLOCATIE2|", stof$location_names$meetlocatie2))
  setnames(n_in1, old = "n (mg/l)", new = paste0("Inlaat1|", stof$location_names$inlaat1))
  setnames(n_in2, old = "n (mg/l)", new = paste0("Inlaat2|", stof$location_names$inlaat2))
  setnames(n_in3, old = "n (mg/l)", new = paste0("Inlaat3|", stof$location_names$inlaat3))

  #merge
  n <- merge.data.table(n_meet1, n_meet2, by = "date", all = TRUE)
  n <- merge.data.table(n, n_in1, by = "date", all = TRUE)
  n <- merge.data.table(n, n_in2, by = "date", all = TRUE)
  n <- merge.data.table(n, n_in3, by = "date", all = TRUE)

  #write
  fwrite(n, file = paste0("output/", waterbalance, "/formatted/series_nitrogen.csv"))
}
