#' Extract concentrations of Cl as constants
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param metingen_rownr amount of rows in sheet "metingen"
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @importFrom readxl read_excel
#'
#' @export
extract_stoffen_chloride <- function(filepath, metingen_rownr, waterbalance){
  #create table containing names and ranges
  cl <- data.table(
    StofNaam = "Chloride",
    InlaatType = c("initieel", "Afstroming", "Berekende inlaat", "Drain", "Inlaat1", "Inlaat2", "Inlaat3", "Inlaat4", "Kwel", "Neerslag", "Riolering", "Uitspoeling", "Uitlaat1", "Uitlaat2", "Verhard"),
    ReeksType = "Constant",
    WaardeAlfa = as.numeric(NA),
    Waarde = as.numeric(NA),
    StofIncrement = 0,
    Eenheid = "mg/l"
  )

  ##extract cl concentration of precipitation, seepage and sewage
  cl_prec_seep_rio <- readxl::read_excel(filepath,
                     sheet = "Cl",
                     range = "H35:H37",
                     col_names = FALSE) |> unlist() |> as.vector()
  cl[InlaatType == "Neerslag", Waarde := cl_prec_seep_rio[1]]
  cl[InlaatType == "Kwel", Waarde := cl_prec_seep_rio[2]]
  cl[InlaatType == "Riolering", Waarde := cl_prec_seep_rio[3]]

  ##extract cl concentrations of afstroming, verhard, uitspoeling, drainage
    #soil 1
  cl_bodem1 <- readxl::read_excel(filepath,
                                  sheet = "Cl",
                                  range = paste0("AV26:CC", metingen_rownr),
                                  col_names = FALSE)
  colnames(cl_bodem1) <- c("prec_verhard (kg)", "bronnen (kg)", "riool (kg)", "Pverhard-mv (kg", "Erfafspoeling (kg)", "Sverhard (kg)", "Clverhard (mg/l)", "Neerslagonverhard (kg", "Clmeteo-mv (kg)", "Clc1-mv (kg)", "som in mv (kg)", "clmv-c1 (kg)", "Afspoeling (kg)", "som uit mv (kg)", "smv (kg)", "clmv (mg/l)", "clc2-c1 (kg)", "bemesting (kg)", "infiltratie (kg)", "som in c1 (kg)", "cl c1-c2 (kg)", "drainage (kg)", "retentie sin (kg)", "som uit c1 (kg)", "sc1 (kg)", "cl-c1 (mg/l)", "cl-c1 stedelijk (mg/l)", "cl c1 landelijk (mg/l)", "kwel (kg)", "som in c2 (kg)", "wegzijging (kg)", "som uit c2 (kg)", "sc2 (kg)", "cl-c2 (mg/l)")
  cl_bodem1 <- as.data.table(cl_bodem1)

    #soil 2
  cl_bodem2 <- readxl::read_excel(filepath,
                                  sheet = "Cl",
                                  range = paste0("CE26:DL", metingen_rownr),
                                  col_names = FALSE)
  colnames(cl_bodem2) <- c("prec_verhard (kg)", "bronnen (kg)", "riool (kg)", "Pverhard-mv (kg", "Erfafspoeling (kg)", "Sverhard (kg)", "Clverhard (mg/l)", "Neerslagonverhard (kg", "Clmeteo-mv (kg)", "Clc1-mv (kg)", "som in mv (kg)", "clmv-c1 (kg)", "Afspoeling (kg)", "som uit mv (kg)", "smv (kg)", "clmv (mg/l)", "clc2-c1 (kg)", "bemesting (kg)", "infiltratie (kg)", "som in c1 (kg)", "cl c1-c2 (kg)", "drainage (kg)", "retentie sin (kg)", "som uit c1 (kg)", "sc1 (kg)", "cl-c1 (mg/l)", "cl-c1 stedelijk (mg/l)", "cl c1 landelijk (mg/l)", "kwel (kg)", "som in c2 (kg)", "wegzijging (kg)", "som uit c2 (kg)", "sc2 (kg)", "cl-c2 (mg/l)")
  cl_bodem2 <- as.data.table(cl_bodem2)

    #bind
  cl_bodem <- rbind(cl_bodem1, cl_bodem2)

  #cl-afstroming
  cl[InlaatType == "Afstroming", Waarde := mean(cl_bodem$`clmv (mg/l)`, na.rm = TRUE)]

  #cl-drain
  cl[InlaatType == "Drain", Waarde := mean(cl_bodem$`cl-c2 (mg/l)`,na.rm = TRUE)]

  #uitspoeling
  cl[InlaatType == "Uitspoeling", Waarde := mean(cl_bodem$`cl-c1 (mg/l)`, na.rm = TRUE)]

  #verhard
  cl[InlaatType == "Verhard", Waarde := mean(cl_bodem$`Clverhard (mg/l)`, na.rm = TRUE)]

  ##extract cl concentrations of inlaten
  cl_inlaten <- readxl::read_excel(filepath,
                                   sheet = "Cl",
                                   range = "D88:D131",
                                   col_names = FALSE) |> unlist() |> as.vector()
  cl_inlaten <- list(
    in1 <- cl_inlaten[1:12],
    in2 <-  cl_inlaten[17:28],
    in3 <-  cl_inlaten[33:44]
  )

  cl_inlaten <- lapply(cl_inlaten, FUN = function(x){
    mean(as.numeric(x), na.rm = TRUE)
  })
  cl[InlaatType == "Inlaat1", Waarde := cl_inlaten[[1]]]
  cl[InlaatType == "Inlaat2", Waarde := cl_inlaten[[2]]]
  cl[InlaatType == "Inlaat3", Waarde := cl_inlaten[[3]]]
  cl[InlaatType == "Berekende inlaat", Waarde := unlist(cl_inlaten) |> mean(na.rm = TRUE)]
  cl[is.nan(Waarde), Waarde := NA]

  ##assume 10 for cl-initieel
  cl[InlaatType == "initieel", Waarde := 10]

  #save
  saveRDS(cl, file = paste0("output/", waterbalance, "/raw/stoffen_chloride.RDS"))
}
