#' Extract measurements of Cl, N and P as timeseries
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param metingen_rownr amount of rows in sheet "metingen"
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @importFrom janitor remove_empty
#' @importFrom readxl read_excel
#' @importFrom dplyr arrange
#' @importFrom lubridate ymd
#'
#' @export
extract_waterquality_timeseries <- function(filepath, metingen_rownr, waterbalance){
  #create table containing names and ranges
  stoffen_tabel <- data.table(locatie = c(rep("meetlocatie1", 3), rep("meetlocatie2", 3), rep("inlaat1", 3), rep("inlaat2", 3), rep("inlaat3", 3)),
                              param = rep(c("cl", "n", "p"), 5),
                              range = c(paste0("AO15:AP", metingen_rownr), #meetlocatie1, cl
                                        paste0("AQ15:AR", metingen_rownr), #meetlocatie1, n
                                        paste0("AS15:AT", metingen_rownr), #meetlocatie1, p
                                        paste0("AV15:AW", metingen_rownr), #meetlocatie2, cl
                                        paste0("AX15:AY", metingen_rownr), #meetlocatie2, n
                                        paste0("AZ15:BA", metingen_rownr), #meetlocatie2, p
                                        paste0("BC15:BF", metingen_rownr), #inlaat1, cl
                                        paste0("BG15:BJ", metingen_rownr), #inlaat1, n
                                        paste0("BK15:BN", metingen_rownr), #inlaat1, p
                                        paste0("CB15:CE", metingen_rownr), #inlaat2, cl
                                        paste0("CF15:CI", metingen_rownr), #inlaat2, n
                                        paste0("CJ15:CM", metingen_rownr), #inlaat2, p
                                        paste0("DA15:DD", metingen_rownr), #inlaat3, cl
                                        paste0("DE15:DH", metingen_rownr), #inlaat3, n
                                        paste0("DI15:DL", metingen_rownr)) #inlaat3, p
  )

  #loop through
  stof <- lapply(1:nrow(stoffen_tabel), FUN = function(x){
    #read, initial tidying
    if(grepl("^inlaat", stoffen_tabel[x]$locatie)){
      #only first and fourth column are relevant in the range
      stof <- readxl::read_excel(filepath,
                                 sheet = "Metingen",
                                 range = stoffen_tabel[x]$range)[, c(1,4)] |> janitor::remove_empty(which = "rows") |> as.data.table()
    } else if(grepl("^meetlocatie", stoffen_tabel[x]$locatie)){
      #range consists of all relevant columns
      stof <- readxl::read_excel(filepath,
                                 sheet = "Metingen",
                                 range = stoffen_tabel[x]$range) |> janitor::remove_empty(which = "rows") |> as.data.table()
    }

    #tidy colnames
    colnames(stof) <- tolower(colnames(stof))
    colnames(stof) <- gsub("\\[|\\]|\\/| ", "", colnames(stof))
    colnames(stof) <- gsub("mgl$", " (mg/l)", colnames(stof))

    #tidy date
    setnames(stof, old = "datum", new = "date")
    stof[, date := ymd(date)]

    #na.omit
    stof <- na.omit(stof)

    #arrange on date
    stof <- arrange(stof, stof$date)

    #return
    return(stof)
  })

  #add names
  names(stof) <- paste0(stoffen_tabel$locatie, "_", stoffen_tabel$param)

  #extract the names of inlaten and waterkwaliteitsmeetpunten
  meetlocatie1 = readxl::read_excel(path = filepath, sheet = "Metingen", range = "AO13", col_names = FALSE, col_types = "text") |> unlist()
  meetlocatie2 =  readxl::read_excel(path = filepath, sheet = "Metingen", range = "AV13", col_names = FALSE, col_types = "text") |> unlist()
  inlaat1 = readxl::read_excel(path = filepath, sheet = "Metingen", range = "BC13", col_names = FALSE, col_types = "text") |> unlist()
  inlaat2 = readxl::read_excel(path = filepath, sheet = "Metingen", range = "CB13", col_names = FALSE, col_types = "text") |> unlist()
  inlaat3 = readxl::read_excel(path = filepath, sheet = "Metingen", range = "DA13", col_names = FALSE, col_types = "text") |> unlist()

  location_names <- data.table(
    meetlocatie1 = ifelse(!is.null(meetlocatie1), meetlocatie1[[1]], NA),
    meetlocatie2 = ifelse(!is.null(meetlocatie2), meetlocatie2[[1]], NA),
    inlaat1 = ifelse(!is.null(inlaat1), inlaat1[[1]], NA),
    inlaat2 = ifelse(!is.null(inlaat2), inlaat2[[1]], NA),
    inlaat3 = ifelse(!is.null(inlaat3), inlaat3[[1]], NA)
  )

  #add location names to list
  stof <- c(stof, list("location_names" = location_names))

  #save
  saveRDS(stof, file = paste0("output/", waterbalance, "/raw/series_waterkwaliteit.RDS"))
}
