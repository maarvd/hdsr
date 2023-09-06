#' Extract concentrations of N as constants
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
extract_stoffen_nitrogen <- function(filepath, metingen_rownr, waterbalance){
  #create table containing names and ranges
  n <- data.table(
    StofNaam = "Stikstof",
    InlaatType = c("Afstroming", "Berekende inlaat", "Drain", "Inlaat1", "Inlaat2", "Inlaat3", "Inlaat4", "Kwel", "Neerslag", "Riolering", "Uitspoeling", "Uitlaat1", "Uitlaat2", "Verhard"),
    ReeksType = "Constant",
    WaardeAlfa = as.numeric(NA),
    Waarde = as.numeric(NA),
    StofIncrement = 0,
    Eenheid = "mgN/l"
  )

  ##extract n concentration of precipitation, seepage and sewage
  n_prec_seep_rio <- readxl::read_excel(filepath,
                                        sheet = "N",
                                        range = "D35:D37",
                                        col_names = FALSE) |> unlist() |> as.vector()
  n[InlaatType == "Neerslag", Waarde := n_prec_seep_rio[1]]
  n[InlaatType == "Kwel", Waarde := n_prec_seep_rio[2]]
  n[InlaatType == "Riolering", Waarde := n_prec_seep_rio[3]]

  ##extract n concentrations of afstroming, verhard, uitspoeling, drainage
  #soil 1
  n_bodem1 <- readxl::read_excel(filepath,
                                 sheet = "N",
                                 range = paste0("BA26:CL", metingen_rownr),
                                 col_names = FALSE)
  colnames(n_bodem1) <-  c("neerslag_verhard (kg)", "bronnen (kg)", "riool (kg)", "bodem (kg)", "n_erven (mg/l)", "erfafspoeling (kg)", "sverhard (kg)", "n_verhard (mg/l)", "neerslag onverhard (kg)", "nmeteo_mv (kg)", "nc1-mv (kg)", "sominmv (kg)", "nmv-c1 (kg)","afspoeling (kg)", "somuitmv (kg)", "smv (kg)", "n_mv (mg/l)", "nc2-c1 (kg)", "bemesting (kg)", "infiltratie (kg)", "mineralisatie (kg)", "sominc1 (kg)", "nc1-c2 (kg)", "drainage (kg)", "retentiefactor S Tref (kg/kg)", "retentie s (kg)", "retnetie sin (kg)", "somuit c1 (kg)", "sc1 (kg)", "n_c1 (mg/l)", "n_c1stedelijk (mg/l)", "nc1_landelijk (mg/l)", "kwel (kg)", "som in c2 (kg)", "wegzijging (kg)", "somuitc2 (kg)", "s_c2 (kg)", "n_c2 (mg/l)")
  n_bodem1 <- as.data.table(n_bodem1)

  #soil 2
  n_bodem2 <- readxl::read_excel(filepath,
                                 sheet = "N",
                                 range = paste0("CN26:DY", metingen_rownr),
                                 col_names = FALSE)
  colnames(n_bodem2) <- c("neerslag_verhard (kg)", "bronnen (kg)", "riool (kg)", "bodem (kg)", "n_erven (mg/l)", "erfafspoeling (kg)", "sverhard (kg)", "n_verhard (mg/l)", "neerslag onverhard (kg)", "nmeteo_mv (kg)", "nc1-mv (kg)", "sominmv (kg)", "nmv-c1 (kg)","afspoeling (kg)", "somuitmv (kg)", "smv (kg)", "n_mv (mg/l)", "nc2-c1 (kg)", "bemesting (kg)", "infiltratie (kg)", "mineralisatie (kg)", "sominc1 (kg)", "nc1-c2 (kg)", "drainage (kg)", "retentiefactor S Tref (kg/kg)", "retentie s (kg)", "retnetie sin (kg)", "somuit c1 (kg)", "sc1 (kg)", "n_c1 (mg/l)", "n_c1stedelijk (mg/l)", "nc1_landelijk (mg/l)", "kwel (kg)", "som in c2 (kg)", "wegzijging (kg)", "somuitc2 (kg)", "s_c2 (kg)", "n_c2 (mg/l)")
  n_bodem2 <- as.data.table(n_bodem2)

  #bind
  n_bodem <- rbind(n_bodem1, n_bodem2)

  #n-afstroming
  n[InlaatType == "Afstroming", Waarde := mean(n_bodem$`n_mv (mg/l)`, na.rm = TRUE)]

  #n-drain
  n[InlaatType == "Drain", Waarde := mean(n_bodem$`n_c2 (mg/l)`,na.rm = TRUE)]

  #uitspoeling
  n[InlaatType == "Uitspoeling", Waarde := mean(n_bodem$`n_c1 (mg/l)`, na.rm = TRUE)]

  #verhard
  n[InlaatType == "Verhard", Waarde := mean(n_bodem$`n_verhard (mg/l)`, na.rm = TRUE)]

  ##extract n concentrations of inlaten
  n_inlaten <- readxl::read_excel(filepath,
                                  sheet = "N",
                                  range = "D113:D156",
                                  col_names = FALSE) |> unlist() |> as.vector()
  n_inlaten <- list(
    in1 <- n_inlaten[1:12],
    in2 <-  n_inlaten[17:28],
    in3 <-  n_inlaten[33:44]
  )

  n_inlaten <- lapply(n_inlaten, FUN = function(x){
    mean(as.numeric(x), na.rm = TRUE)
  })
  n[InlaatType == "Inlaat1", Waarde := n_inlaten[[1]]]
  n[InlaatType == "Inlaat2", Waarde := n_inlaten[[2]]]
  n[InlaatType == "Inlaat3", Waarde := n_inlaten[[3]]]
  n[InlaatType == "Berekende inlaat", Waarde := unlist(n_inlaten) |> mean(na.rm = TRUE)]
  n[is.nan(Waarde), Waarde := NA]

  #save
  saveRDS(n, file = paste0("output/", waterbalance, "/raw/stoffen_stikstof.RDS"))
}
