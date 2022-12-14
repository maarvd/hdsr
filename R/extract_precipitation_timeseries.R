#' Extract precipitation and evaporation timeseries
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param metingen_rownr amount of rows in sheet "metingen"
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom lubridate ymd
#' @importFrom janitor remove_empty
#'
#' @export

extract_precipitation_timeseries <- function(filepath, metingen_rownr, waterbalance){
  #read neerslag/verdamping
  prec <- readxl::read_excel(filepath,
                             sheet = "Metingen",
                             range = paste0("B13:E", metingen_rownr)) |> janitor::remove_empty(which = "rows") |> as.data.table()

  #tidy colnames
  colnames(prec) <- tolower(colnames(prec))
  colnames(prec) <- gsub("\\[|\\]|mm| ", "", colnames(prec))
  setnames(prec,
           old = c("datum", "p", "eref"),
           new = c("date", "prec (mm)", "eref (mm)"))

  #remove jaar, adjust classes of cols
  prec[, jaar := NULL]
  prec[, `prec (mm)` := as.numeric(`prec (mm)`)]
  prec[, `eref (mm)` := as.numeric(`eref (mm)`)]
  prec[, date := ymd(date)]

  #save
  saveRDS(prec, file = paste0("output/", waterbalance, "/raw/series_precipitation.RDS"))
}

