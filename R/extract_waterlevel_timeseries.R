#' Extract waterlevels as timeseries
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param metingen_rownr amount of rows in sheet "metingen"
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#'
#' @export
extract_waterlevel_timeseries <- function(filepath, metingen_rownr, waterbalance){
  #read
  waterpeil <- suppressMessages(readxl::read_excel(filepath,
                                                   sheet = "Metingen",
                                                   range = paste0("B13:H", metingen_rownr),
                                                   guess_max = metingen_rownr)[, c(1, 7)] |> as.data.table())

  #tidy
  colnames(waterpeil) <- c("date", "peil (mNAP)")
  waterpeil[, date := ymd(date)]

  #save
  saveRDS(waterpeil, file = paste0("output/", waterbalance, "/raw/series_waterpeil.RDS"))
}
