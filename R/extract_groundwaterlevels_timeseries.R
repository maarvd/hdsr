#' Extract groundwater levels as timeseries
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param metingen_rownr amount of rows in sheet "metingen"
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom lubridate ymd
#'
#'
#' @export
extract_groundwaterlevels_timeseries <- function(filepath, metingen_rownr, waterbalance){
  #read
  grondwater1 <- suppressMessages(readxl::read_excel(filepath,
                                                     sheet = "Metingen",
                                                     range = paste0("AG15:AH", metingen_rownr),
                                                     guess_max = metingen_rownr) |> remove_empty(which = 'rows') |> as.data.table())

  grondwater2 <- suppressMessages(readxl::read_excel(filepath,
                                                     sheet = "Metingen",
                                                     range = paste0("AK15:AL", metingen_rownr),
                                                     guess_max = metingen_rownr) |> remove_empty(which = 'rows') |> as.data.table())

  #add to list
  merged <- list("grondwaterstand1" = grondwater1,
                 "grondwaterstand2" = grondwater2)

  #tidy
  merged.tidied <- lapply(merged, FUN = function(x){
    #tidy colnames
    colnames(x) <- c("date", "grondwaterstand (cmNAP)")

    #tidy date
    x[, date := ymd(date)]

    #return
    return(x)
  })

  #save
  saveRDS(merged.tidied, file = paste0("output/", waterbalance, "/raw/series_grondwaterstand.RDS"))
}
