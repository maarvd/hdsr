#' Extract concentrations of P as constants
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
extract_stoffen_phosphorus <- function(filepath, metingen_rownr, waterbalance){
  #create table containing names and ranges
  p <- data.table(
    StofNaam = "Fosfor",
    InlaatType = c("Afstroming", "Berekende inlaat", "Drain", "Inlaat1", "Inlaat2", "Inlaat3", "Inlaat4", "Kwel", "Neerslag", "Riolering", "Uitspoeling", "Uitlaat1", "Uitlaat2", "Verhard"),
    ReeksType = "Constant",
    WaardeAlfa = as.numeric(NA),
    Waarde = as.numeric(NA),
    StofIncrement = 0,
    Eenheid = "mgP/l"
  )

  ##extract p concentration of precipitation, seepage and sewage
  p_prec_seep_rio <- readxl::read_excel(filepath,
                                         sheet = "P",
                                         range = "D35:D43",
                                         col_names = FALSE) |> unlist() |> as.vector()
  p[InlaatType == "Neerslag", Waarde := p_prec_seep_rio[1]]
  p[InlaatType == "Kwel", Waarde := p_prec_seep_rio[2]]
  p[InlaatType == "Riolering", Waarde := p_prec_seep_rio[9]]

  ##extract p concentrations of afstroming, verhard, uitspoeling, drainage
  #soil 1
  p_bodem1 <- readxl::read_excel(filepath,
                                  sheet = "P",
                                  range = paste0("BA26:CL", metingen_rownr),
                                  col_names = FALSE)
  colnames(p_bodem1) <- c("neerslag_verhard (kg)", "bronnen (kg)", "riool (kg)", "p verhard-mv (kg)", "p_erven (mg/l)", "erfafspoeling (kg)", "sverhard (kg)", "p_verhard (mg/l)", "neerslag onverhard (kg)", "pmeteo_mv (kg)", "pc1-mv (kg)", "sominmv (kg)", "pmv-c1 (kg)","afspoeling (kg)", "somuitmv (kg)", "smv (kg)", "p_mv (mg/l)", "pc2-c1 (kg)", "bemesting (kg)", "infiltratie (kg)", "mineralisatie (kg)", "sominc1 (kg)", "pc1-c2 (kg)", "drainage (kg)", "retentiefactor (kg/kg)", "retentie s (kg)", "retnetie sin (kg)", "somuit c1 (kg)", "sc1 (kg)", "p_c1 (mg/l)", "p_c1stedelijk (mg/l)", "pc1_landelijk (mg/l)", "kwel (kg)", "som in c2 (kg)", "wegzijging (kg)", "somuitc2 (kg)", "s_c2 (kg)", "p_c2 (mg/l)")
  p_bodem1 <- as.data.table(p_bodem1)

  #soil 2
  p_bodem2 <- readxl::read_excel(filepath,
                                  sheet = "P",
                                  range = paste0("CN26:DY", metingen_rownr),
                                  col_names = FALSE)
  colnames(p_bodem2) <- c("neerslag_verhard (kg)", "bronnen (kg)", "riool (kg)", "p verhard-mv (kg)", "p_erven (mg/l)", "erfafspoeling (kg)", "sverhard (kg)", "p_verhard (mg/l)", "neerslag onverhard (kg)", "pmeteo_mv (kg)", "pc1-mv (kg)", "sominmv (kg)", "pmv-c1 (kg)","afspoeling (kg)", "somuitmv (kg)", "smv (kg)", "p_mv (mg/l)", "pc2-c1 (kg)", "bemesting (kg)", "infiltratie (kg)", "mineralisatie (kg)", "sominc1 (kg)", "pc1-c2 (kg)", "drainage (kg)", "retentiefactor (kg/kg)", "retentie s (kg)", "retnetie sin (kg)", "somuit c1 (kg)", "sc1 (kg)", "p_c1 (mg/l)", "p_c1stedelijk (mg/l)", "pc1_landelijk (mg/l)", "kwel (kg)", "som in c2 (kg)", "wegzijging (kg)", "somuitc2 (kg)", "s_c2 (kg)", "p_c2 (mg/l)")
  p_bodem2 <- as.data.table(p_bodem2)

  #bind
  p_bodem <- rbind(p_bodem1, p_bodem2)

  #p-afstroming
  p[InlaatType == "Afstroming", Waarde := mean(p_bodem$`p_mv (mg/l)`, na.rm = TRUE)]

  #p-drain
  p[InlaatType == "Drain", Waarde := mean(p_bodem$`p_c2 (mg/l)`,na.rm = TRUE)]

  #uitspoeling
  p[InlaatType == "Uitspoeling", Waarde := mean(p_bodem$`p_c1 (mg/l)`, na.rm = TRUE)]

  #verhard
  p[InlaatType == "Verhard", Waarde := mean(p_bodem$`p_verhard (mg/l)`, na.rm = TRUE)]

  ##extract p concentrations of inlaten
  p_inlaten <- readxl::read_excel(filepath,
                                   sheet = "P",
                                   range = "D104:D147",
                                   col_names = FALSE) |> unlist() |> as.vector()
  p_inlaten <- list(
    in1 <- p_inlaten[1:12],
    in2 <-  p_inlaten[17:28],
    in3 <-  p_inlaten[33:44]
  )

  p_inlaten <- lapply(p_inlaten, FUN = function(x){
    mean(as.numeric(x), na.rm = TRUE)
  })
  p[InlaatType == "Inlaat1", Waarde := p_inlaten[[1]]]
  p[InlaatType == "Inlaat2", Waarde := p_inlaten[[2]]]
  p[InlaatType == "Inlaat3", Waarde := p_inlaten[[3]]]
  p[InlaatType == "Berekende inlaat", Waarde := unlist(p_inlaten) |> mean(na.rm = TRUE)]
  p[is.nan(Waarde), Waarde := NA]

  #save
  saveRDS(p, file = paste0("output/", waterbalance, "/raw/stoffen_fosfor.RDS"))
}
