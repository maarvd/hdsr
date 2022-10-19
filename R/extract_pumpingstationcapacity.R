#' Extract pumping station capacity
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param savedir directory to save results to
#'
#'
#' @export
extract_pumpingstationcapacity <- function(filepath, savedir){
  #read
  gemaalcapaciteit <- suppressMessages(read_excel(filepath, sheet = "WAT", range = "B15:D16", col_names = FALSE)) |> as.data.table()

  #tidy
  colnames(gemaalcapaciteit) <- c("name", "unit", "value")
  gemaalcapaciteit$name <- c("gemaalcapaciteit (m3/min)", "inlaatcapaciteit (m3/min)")

  #convert gemaalcapaciteit to AGV names and units
  tidied <- data.table(name = c("maximale inlaatcapaciteit (m3/dag)", "maximale aflaatcapaciteit (m3/dag)"),
                       value = c(gemaalcapaciteit[name == "inlaatcapaciteit (m3/min)"]$value * 60 * 24,
                                 gemaalcapaciteit[name == "gemaalcapaciteit (m3/min)"]$value * 60 * 24))

  #save
  saveRDS(tidied, file = paste0(savedir, "/gemaalcapaciteit.RDS"))

}
