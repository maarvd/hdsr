#' Format extracted surface area data to Artesia format
#'
#'
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @importFrom dplyr arrange
#'
#' @export
format_surfaceareas <- function(waterbalance){
  #load oppervlakten
  opp <- readRDS(paste0("output/", waterbalance, "/raw/opp.RDS"))

  #rbindlist
  opp <- rbindlist(opp, idcol = TRUE)[, c("name", "value", ".id")]

  #tidy names
  opp[name == "ariool", name := "vh_rio"]
  opp[name == "averhard", name := "vh"]
  opp[name == "aonverhardgedraineerd", name := "oo_drain"]
  opp[name == "aonverhardongedraineerd", name := "oo"]
  opp[name == "awater", name := "wa"]

  #add column BakjePyCode
  opp[name == "vh_rio", BakjeOmschrijving := "VerhardGerioleerd"]
  opp[name == "vh", BakjeOmschrijving := "VerhardOngerioleerd"]
  opp[name == "oo", BakjeOmschrijving := "OnverhardOngedraineerd"]
  opp[name == "oo_drain", BakjeOmschrijving := "OnverhardGedraineerd"]
  opp[name == "wa", BakjeOmschrijving := "Water"]

  #make distinction in bodembakjes clear
  opp[grepl("area", .id), BakjeOmschrijving := paste0(BakjeOmschrijving, "_", .id)]
  opp[grepl("area", .id), name := paste0(name, "_", .id)]

  #add PyCode
  #current ones are (1) Verhard, 2(Onverhard), 3 (Drain) and 4 (MengRiool), 5 (Water)
  opp[grepl("OnverhardOngedraineerd", BakjeOmschrijving), BakjePyCode := "Onverhard"]
  opp[grepl("OnverhardGedraineerd", BakjeOmschrijving), BakjePyCode := "Drain"]
  opp[grepl("VerhardOngerioleerd", BakjeOmschrijving), BakjePyCode := "Verhard"]
  opp[grepl("Gerioleerd", BakjeOmschrijving) & !grepl("Ongerioleerd", BakjeOmschrijving), BakjePyCode := "MengRiool"]
  opp[grepl("Water", BakjeOmschrijving), BakjePyCode := "Water"]

  #set names
  setnames(opp, old = c("name", "value"), new = c("BakjeID", "OppWaarde"))

  #add EAG ID and EAGCode
  opp[, EAGID := waterbalance]
  opp[, EAGCode := waterbalance]

  #select rel columns in rel order
  opp <- opp[, c("EAGID", "EAGCode", "BakjeID", "BakjeOmschrijving", "BakjePyCode", "OppWaarde")]

  #combine 'verhard', 'drain' and 'mengrio' (multiple areas are aggregated in waterbalance)
  verhard <- data.table(EAGID = waterbalance, EAGCode = waterbalance, BakjeID = "vh", BakjeOmschrijving = "VerhardOngerioleerd", BakjePyCode = "Verhard", OppWaarde = sum(opp[BakjePyCode == "Verhard"]$OppWaarde))
  drain <- data.table(EAGID = waterbalance, EAGCode = waterbalance, BakjeID = "Drain", BakjeOmschrijving = "OnverhardGedraineerd", BakjePyCode = "Drain", OppWaarde = sum(opp[BakjePyCode == "Drain"]$OppWaarde))
  mengrio <- data.table(EAGID = waterbalance, EAGCode = waterbalance, BakjeID = "rio", BakjeOmschrijving = "VerhardGerioleerd", BakjePyCode = "MengRiool", OppWaarde = sum(opp[BakjePyCode == "MengRiool"]$OppWaarde))


  opp <- rbind(opp[!BakjePyCode %in% c("MengRiool", "Verhard", "Drain")], verhard, drain, mengrio)

  #arrange
  opp <- arrange(opp, opp$BakjeOmschrijving) |> as.data.table()

  #save
  fwrite(opp, file = paste0("output/", waterbalance, "/formatted/opp.csv"))
}
