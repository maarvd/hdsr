#' Extract waterfluxes of in -and outlet
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param metingen_rownr amount of rows in sheet "metingen"
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#'
#' @export
extract_inoutlet_timeseries <- function(filepath, metingen_rownr, waterbalance){
  #read
  debiet <- suppressMessages(readxl::read_excel(filepath,
                                                sheet = "Metingen",
                                                range = paste0("B13:P", metingen_rownr),
                                                guess_max = metingen_rownr)[, c(1, 13, 15)] |> as.data.table())

  #tidy cols
  colnames(debiet) <- c("date", "uitlaat (m3)", "inlaat (m3)")

  #tidy date
  debiet[, date := ymd(date)]

  #save
  saveRDS(debiet, file = paste0("output/", waterbalance, "/raw/series_inuitlaat.RDS"))
}
