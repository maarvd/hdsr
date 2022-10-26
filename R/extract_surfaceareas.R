#' Extract surface areas
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @importFrom readxl read_excel
#'
#' @export
extract_surfaceareas <- function(filepath, waterbalance){
  #read settings of bodembakjes
  systeem_area1 <- suppressMessages(read_excel(filepath, sheet = "Settings", range = "R3:T17", col_names = FALSE)) |> as.data.table()
  systeem_area2 <-  suppressMessages(read_excel(filepath, sheet = "Settings", range = "V3:X17", col_names = FALSE)) |> as.data.table()
  water <- suppressMessages(read_excel(filepath, sheet = "Settings", range = "N15:P15", col_names = FALSE)) |> as.data.table()

  #combine systeem area1 and area2 (bodem & riolering) to list
  bodem <- list("area1" = systeem_area1,
                "area2" = systeem_area2)

  #tidy
  bodem <- lapply(bodem, FUN = function(x){
    #tidy colnames and cells
    colnames(x) <- c("name", "unit", "value")
    x[, name := tolower(name)]
    x[, name := gsub(" |\\.", "", name)]
    x[, unit := gsub("\\[|\\]| ", "", unit)]
    x[, unit := gsub("²", "2", unit)]
    x[, unit := gsub("³", "3", unit)]

    #select rel params
    x <- x[unit == "m2"]
    x <- x[name %in% c("averhard", "averhardgerioleerd", "aonverhard") & unit == "m2"]

    #create averhardongerioleerd
    x <- rbind(x, data.table(name = "averhardongerioleerd",
                             unit = "m2",
                             value = as.numeric(NA)))
    x[name == "averhardongerioleerd", value := x[name == "averhard"]$value - x[name == "averhardgerioleerd"]$value]
    x <- x[name != "averhard"]

    #return
    return(x)
  })
  colnames(water) <- c("name", "unit", "value")
  water[, name := tolower(name)]
  water[, name := gsub(" ", "", name)]
  water[, unit := gsub("²", "2", unit)]
  water[, unit := gsub("\\[|\\]| ", "", unit)]

  #merge soil and water
  combined <- c(bodem, list("water"=  water))

  #split onverhard in onverhard_gedraineerd and onverhard_ongedraineerd
  bodem1_slootdrainage <- suppressMessages(read_excel(filepath, sheet = "B1", range = "D33", col_names = FALSE))[[1]] |> as.vector()|> as.numeric()
  bodem1_buisdrainage <- suppressMessages(read_excel(filepath, sheet = "B1", range = "D38", col_names = FALSE))[[1]] |> as.vector()|> as.numeric()
  bodem1_extradrainage <- suppressMessages(read_excel(filepath, sheet = "B1", range = "D42", col_names = FALSE))[[1]] |> as.vector()|> as.numeric()

  bodem2_slootdrainage <- suppressMessages(read_excel(filepath, sheet = "B2", range = "D33", col_names = FALSE))[[1]] |> as.vector() |> as.numeric()
  bodem2_buisdrainage <- suppressMessages(read_excel(filepath, sheet = "B2", range = "D38", col_names = FALSE))[[1]] |> as.vector() |> as.numeric()
  bodem2_extradrainage <- suppressMessages(read_excel(filepath, sheet = "B2", range = "D42", col_names = FALSE))[[1]] |> as.vector() |> as.numeric()

  if(bodem1_slootdrainage == 1 | bodem1_buisdrainage == 1 | bodem1_extradrainage == 1){
    combined$area1[name == "aonverhard", name := "aonverhardgedraineerd"]
    combined$area1 <- rbind(combined$area1, data.table(
      name = "aonverhardongedraineerd",
      unit = "m2",
      value = 0
    ))
  } else{
    combined$area1[name == "aonverhard", name := "aonverhardongedraineerd"]
    combined$area1 <- rbind(combined$area1, data.table(
      name = "aonverhardgedraineerd",
      unit = "m2",
      value = 0
    ))
  }

  if(bodem2_slootdrainage == 1 | bodem2_buisdrainage == 1 | bodem2_extradrainage == 1){
    combined$area2[name == "aonverhard", name := "aonverhardgedraineerd"]
    combined$area2 <- rbind(combined$area2, data.table(
      name = "aonverhardongedraineerd",
      unit = "m2",
      value = 0
    ))
  } else{
    combined$area2[name == "aonverhard", name := "aonverhardongedraineerd"]
    combined$area2 <- rbind(combined$area2, data.table(
      name = "aonverhardgedraineerd",
      unit = "m2",
      value = 0
    ))
  }

  #save
  saveRDS(combined, file = paste0("output/", waterbalance, "/raw/opp.RDS"))
}#extracts surface areas

