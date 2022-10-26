#' Extract a range of other settings
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#'

extract_other <- function(filepath, waterbalance){
  #Settings
  hoogte_slootbodem <- suppressMessages(read_excel(filepath, sheet = "Settings", range = "P23", col_names = FALSE, col_types = "list"))[[1]] |> unlist()

  #add to data.table
  other <- data.table(param = c("hoogte slootbodem (mNAP)"),
             value = c(hoogte_slootbodem))

  #save
  saveRDS(other, file = paste0("output/", waterbalance, "/raw/other.RDS"))
}
