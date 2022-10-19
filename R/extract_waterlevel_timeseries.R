#' Extract waterlevels as timeseries
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param metingen_rownr amount of rows in sheet "metingen"
#' @param savedir directory to save results to
#'
#'
#' @export
extract_waterlevel_timeseries <- function(filepath, metingen_rownr, savedir){
  #read
  waterpeil <- suppressMessages(readxl::read_excel(filepath,
                                                   sheet = "Metingen",
                                                   range = paste0("B13:H", metingen_rownr),
                                                   guess_max = metingen_rownr)[, c(1, 7)] |> as.data.table())

  #tidy
  colnames(waterpeil) <- c("date", "peil (mNAP)")
  waterpeil[, date := ymd(date)]

  #save
  saveRDS(waterpeil, file = paste0(savedir, "/series_waterpeil.RDS"))
}
