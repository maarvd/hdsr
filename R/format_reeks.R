#' Format reeks to Artesia format
#'
#'
#' @param waterbalance acode of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @importFrom terra rast as.data.frame
#' @importFrom lubridate day month
#' @importFrom dplyr arrange
#'
#' @export

format_reeks <- function(waterbalance){

  #read tidied oppervlakte
  opp <- fread(paste0("output/", waterbalance, "/formatted/opp.csv"))

  #read raw peilbeheer
  peilregime <- readRDS(paste0("output/", waterbalance, "/raw/peilregime.RDS"))

  #read seepage file and calc average seepage
  seep <- terra::rast(paste0("output/", waterbalance, "/Spatial/LHM4.1_seepinf_2011-2018_mmday.tiff"))
  seep <- terra::as.data.frame(seep, xy = FALSE)[[1]] |> mean(na.rm = TRUE)

  #calc wegzijging
  wegz <- seep * -1

  #verhard layer 2
  verhard <- data.table(BakjeID = unique(opp[BakjePyCode == "Verhard"]$BakjeID),
                        Laagvolgorde = 2,
                        ClusterType = "Qwegz",
                        ParamType = "Constant",
                        Waarde = wegz,
                        WaardeAlfa = as.numeric(NA),
                        StartDag = as.character(NA),
                        Eenheid = "mm/dag")

  #water
    #peilregime
  water.peil <- lapply(1:nrow(peilregime), FUN = function(x){
    #filter relevant row
    peil.rel <- peilregime[x,]

    #use gemaalpeil for hTargetMax and inlaatpeil for hTargetMin
    peil.rel <- data.table(BakjeID = unique(opp[BakjePyCode == "Water"]$BakjeID),
               Laagvolgorde = 1,
               ClusterType = c("hTargetMax", "hTargetMin"),
               ParamType = "ValueSeries",
               Waarde = c(peil.rel$`gemaalpeil (mnap)`, peil.rel$`inlaatpeil (mnap)`),
               WaardeAlfa = as.numeric(NA),
               StartDag = peil.rel$startdag,
               Eenheid = "mNAP")
  }) |> rbindlist()

    #seepage
  water.seep <- data.table(BakjeID = unique(opp[BakjePyCode == "Water"]$BakjeID),
                           Laagvolgorde = 1,
                           ClusterType = "Qwegz",
                           ParamType = "Constant",
                           Waarde = wegz,
                           WaardeAlfa = as.numeric(NA),
                           StartDag = as.character(NA),
                           Eenheid = "mm/dag")

    #bind
  water <- rbind(water.peil, water.seep)

  #onverhard ongedraineerd
  onverhard <- data.table(BakjeID = unique(opp[grepl("^oo_", BakjeID)]$BakjeID),
                          Laagvolgorde = 1,
                          ClusterType = "Qwegz",
                          ParamType = "Constant",
                          Waarde = wegz,
                          WaardeAlfa = as.numeric(NA),
                          StartDag = as.character(NA),
                          Eenheid = "mm/dag")

  #gedraineerd layer2
  drain <- data.table(BakjeID = opp[BakjePyCode == "Drain"]$BakjeID,
                      Laagvolgorde = 2,
                      ClusterType = "Qwegz",
                      ParamType = "Constant",
                      Waarde = wegz,
                      WaardeAlfa = as.numeric(NA),
                      StartDag = as.character(NA),
                      Eenheid = "mm/dag")


  #bind
  merged <- rbind(verhard, water, drain, onverhard)

  #tidy startdag
  merged[!is.na(StartDag), StartDag := paste0(day(StartDag), "-", month(StartDag, label = TRUE, abbr = TRUE))]

  #add EAGID and EAGCode
  merged[, EAGID := unique(opp$EAGID)]
  merged[, EAGCode := unique(opp$EAGCode)]

  #tidy column order
  merged <- merged[, c("EAGID", "EAGCode", "BakjeID", "Laagvolgorde", "ClusterType", "ParamType", "Waarde", "WaardeAlfa", "StartDag", "Eenheid")]

  #arrange
  merged <- dplyr::arrange(merged, desc(merged$BakjeID), merged$Laagvolgorde,  merged$BakjeID)

  #save to output
  fwrite(merged, file = paste0("output/", waterbalance, "/formatted/reeks.csv"))
}
