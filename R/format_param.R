#' Format extracted parameters to Artesia format
#'
#'
#' @param waterbalance acode of waterbalance (e.g. KRWO_04_Kockengen)
#'
#'
#' @export
format_param <- function(waterbalance){
  #RFacOut = f_uitspoel
  #RFacIn = f_intrek
  #por = porositeit/bergingsruimte
  #hMax = max level
  #hInit = init level
  #hBottom = bodemhoogte
  #hTarget = streefpeil
  #QInMax = maximale inlaatcapaciteit
  #QOutMax = maximale uitlaatcapaciteit
  #EFacMin = min.gewasverdampingsfactor (rekenblad)
  #EFacMax = gewasverdampingsfacor (rekenblad)

  #read required files----
  #formatted oppervlakte file in order to extract the right EAG and Bakje IDs
  opp_formatted <- fread(paste0("output/", waterbalance, "/formatted/opp.csv"))

  #raw peilbeheer to estimate streefpeil
  peilregime_raw <- readRDS(paste0("output/", waterbalance, "/raw/peilregime.RDS"))

  #gemaalcapaciteit
  gemaalcapaciteit_raw <- readRDS(paste0("output/", waterbalance, "/raw/gemaalcapaciteit.RDS"))

  #other settings (slootbodemhoogte)
  other_raw <- readRDS(paste0("output/", waterbalance,"/raw/other.RDs"))

  #onverhard gedraineerd----
  #select rel bakjes
  bakjes.oo_drain <-
    opp_formatted$BakjeID[grepl("^oo_drain", opp_formatted$BakjeID)]

  #create empty data.table with rel params
  params.drain <- lapply(
    bakjes.oo_drain,
    FUN = function(x) {
      data.table(
        BakjeID = x,
        Laagvolgorde = c(rep(1, 7), rep(2, 7)),
        ParamCode = rep(c(
          "RFacOut", "RFacIn", "por", "hMax", "hInit", "EFacMin", "EFacMax"
        ), 2)
      )
    }
  ) |> rbindlist()

  #add defaults
  #fractie uitspoeling
  params.drain[Laagvolgorde == 1 & ParamCode == "RFacOut", Waarde := 0.5]
  params.drain[Laagvolgorde == 2 & ParamCode == "RFacOut", Waarde := 0.001]

  #fractie intrek
  params.drain[Laagvolgorde == 1 & ParamCode == "RFacIn", Waarde := 0]
  params.drain[Laagvolgorde == 2 & ParamCode == "RFacIn", Waarde := 0.001]

  #porositeit
  params.drain[ParamCode == "por", Waarde := 0.3]

  #max level
  params.drain[ParamCode == "hMax", Waarde := 0.3]

  #init level
  params.drain[ParamCode == "hInit", Waarde := 0.15]

  #maximale gewasverdamping
  params.drain[ParamCode == "EFacMax" & Laagvolgorde == 1, Waarde := 1]
  params.drain[ParamCode == "EFacMax" & Laagvolgorde == 2, Waarde := 0.75]

  #minimale gewasverdamping
  params.drain[ParamCode == "EFacMin" & Laagvolgorde == 1, Waarde := 0.75]
  params.drain[ParamCode == "EFacMin" & Laagvolgorde == 2, Waarde := 0.25]


  #onverhard ongedraineerd----
  #select rel bakjes
  bakjes.oo <- opp_formatted$BakjeID[grepl("^oo_", opp_formatted$BakjeID) & !grepl("drain", opp_formatted$BakjeID)]

  #create empty data.table with rel params
  params.onverhard <- lapply(
    bakjes.oo,
    FUN = function(x) {
      data.table(
        BakjeID = x,
        Laagvolgorde = 1,
        ParamCode = c("RFacOut", "RFacIn", "por", "hMax", "hInit", "EFacMin", "EFacMax")
      )
    }
  ) |> rbindlist()

  #add defaults
  #uitspoeling
  params.onverhard[ParamCode == "RFacOut", Waarde := 0.3]

  #intrek
  params.onverhard[ParamCode == "RFacIn", Waarde := 0.15]

  #porositeit/bergingsruimte
  params.onverhard[ParamCode == "por", Waarde := 0.6]

  #max level
  params.onverhard[ParamCode == "hMax", Waarde := 0.5]

  #init level
  params.onverhard[ParamCode == "hInit", Waarde := 0.25]

  #maximale gewasverdamping
  params.onverhard[ParamCode == "EFacMax", Waarde := 1]

  #minimale gewasverdamping
  params.onverhard[ParamCode == "EFacMin", Waarde := 0.75]

  #water----
  #data.table with rel params
  params.water <- data.table(
    BakjeID = opp_formatted$BakjeID[grepl("^wa", opp_formatted$BakjeID)],
    Laagvolgorde = 1,
    ParamCode = c("hBottom", "hTarget", "QInMax", "QOutMax")
  )

  #tidy peilregime in order to derive streefpeil
  peilregime <- data.table(
    date = seq(from = ymd(min(peilregime_raw$startdag)),
               to = ymd(paste0(max(year(peilregime_raw$startdag)), "-12-31")),
               by = 1))
  peilregime_raw[, date := ymd(startdag)]
  peilregime_raw[, `gemaalpeil (mnap)` := as.numeric(`gemaalpeil (mnap)`)]
  peilregime_raw[, `inlaatpeil (mnap)` := as.numeric(`inlaatpeil (mnap)`)]
  peilregime <- merge.data.table(peilregime, peilregime_raw[, c("date", "gemaalpeil (mnap)", "inlaatpeil (mnap)")], by = "date", all = TRUE)
  peilregime <- tidyr::fill(peilregime, c(`gemaalpeil (mnap)`, `inlaatpeil (mnap)`), .direction = "down")

  #assume streefpeil = average of gemaalpeil and inlaatpeil
  peilregime[, streefpeil := (`gemaalpeil (mnap)` + `inlaatpeil (mnap)`)/2]

  #add  params to waterbakje
  #streefpeil
  params.water[ParamCode == "hTarget", Waarde := mean(peilregime$streefpeil, na.rm = TRUE)]

  #maximale in en aflaatcapaciteit
  params.water[ParamCode == "QInMax", Waarde := gemaalcapaciteit_raw[name == "maximale inlaatcapaciteit (m3/dag)"]$value]
  params.water[ParamCode == "QOutMax", Waarde := gemaalcapaciteit_raw[name == "maximale aflaatcapaciteit (m3/dag)"]$value]

  #bodemhoogte
  params.water[ParamCode == "hBottom", Waarde := other_raw[param == "hoogte slootbodem (mNAP)"]$value]

  ##merge all bakjes together----
  params <- rbind(params.drain, params.onverhard, params.water)

  #add EAGID and EAGCode
  params[, EAGID := unique(opp_formatted$EAGID)]
  params[, EAGCode:= unique(opp_formatted$EAGCode)]

  #rel order
  params <- params[, c("EAGID", "EAGCode", "BakjeID", "Laagvolgorde", "ParamCode", "Waarde")]

  #add eenheid if applicable
  unique(params$ParamCode)
  params[ParamCode %in% c("hMax", "hInit"), eenheid := "m"]
  params[ParamCode %in% c("QInMax", "QOutMax"), eenheid := "m3/dag"]
  params[ParamCode %in% c("hBottom", "hTarget"), eenheid := "mNAP"]

  #save----
  fwrite(params, file = paste0("output/", waterbalance, "/formatted/param.csv"))
}

