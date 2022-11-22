#' Format series to Artesia format
#'
#'
#' @param waterbalance acode of waterbalance (e.g. KRWO_04_Kockengen)
#'
#' @import data.table
#' @export
format_series <- function(waterbalance){
  #read series
  prec <- readRDS(paste0("output/", waterbalance, "/raw/series_precipitation.RDS"))
  peil <- readRDS(paste0("output/", waterbalance, "/raw/series_waterpeil.RDS"))
  inuit <- readRDS(paste0("output/", waterbalance, "/raw/series_inuitlaat.RDS"))

  #check names of inlaten based on waterquality measurement locations\
  waterkwaliteit <- readRDS(paste0("output/", waterbalance, "/raw/series_waterkwaliteit.RDS"))
  inlaat1 <- Reduce(f = function(x, y) merge.data.table(x, y, by = "date", all = TRUE), x =list(waterkwaliteit$inlaat1_cl,waterkwaliteit$inlaat1_n,waterkwaliteit$inlaat1_p))
  inlaat2 <- Reduce(f = function(x, y) merge.data.table(x, y, by = "date", all = TRUE), x =list(waterkwaliteit$inlaat2_cl,waterkwaliteit$inlaat2_n,waterkwaliteit$inlaat2_p))
  inlaat3 <- Reduce(f = function(x, y) merge.data.table(x, y, by = "date", all = TRUE), x =list(waterkwaliteit$inlaat3_cl,waterkwaliteit$inlaat3_n,waterkwaliteit$inlaat3_p))
  inlaatnames <- waterkwaliteit$location_names

  #check which inlaten are present and check their names
  inlets <- data.table(inlaat = c("inlaat1", "inlaat2", "inlaat3"),
                       name = as.character(NA),
                       present = as.character(NA))

    #inlet1
  if(!is.na(inlaatnames$inlaat1)){
    inlets[inlaat == "inlaat1", name := inlaatnames$inlaat1]
    inlets[inlaat == "inlaat1", present := "yes"]
  } else if(is.na(inlaatnames$inlaat1) & nrow(inlaat1) > 0){
    inlets[inlaat == "inlaat1", name := "onbekend"]
    inlets[inlaat == "inlaat1", present := "yes"]
  } else{
    inlets[inlaat == "inlaat1", present := "no"]
  }

    #inlet2
  if(!is.na(inlaatnames$inlaat2)){
    inlets[inlaat == "inlaat2", name := inlaatnames$inlaat2]
    inlets[inlaat == "inlaat2", present := "yes"]
  } else if(is.na(inlaatnames$inlaat2) & nrow(inlaat2) > 0){
    inlets[inlaat == "inlaat2", name := "onbekend"]
    inlets[inlaat == "inlaat2", present := "yes"]
  } else{
    inlets[inlaat == "inlaat2", present := "no"]
  }

    #inlet3
  if(!is.na(inlaatnames$inlaat3)){
    inlets[inlaat == "inlaat3", name := inlaatnames$inlaat3]
    inlets[inlaat == "inlaat3", present := "yes"]
  } else if(is.na(inlaatnames$inlaat3) & nrow(inlaat3) > 0){
    inlets[inlaat == "inlaat3", name := "onbekend"]
    inlets[inlaat == "inlaat3", present := "yes"]
  } else{
    inlets[inlaat == "inlaat3", present := "no"]
  }

  #combine precipitation, evaporation, waterlevel and tidy
  dt1 <- merge.data.table(prec, peil, by = "date", all = TRUE)
  setnames(dt1, old = c("prec (mm)", "eref (mm)", "peil (mNAP)"), new = c("Neerslag1|Neerslag", "Verdamping1|Verdamping", "Peil1|peil"))

  #helper function for capitalization
  firstup <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

  #add name of inlets (but with NA since HDSR waterbalance already aggregated raw data to one inlet)
  inlets.present <- inlets[present == "yes"]
  inlets.names <- paste0(firstup(inlets.present$inlaat), "|", inlets.present$name)
  for(i in 1:length(inlets.names)){
    dt1[, inlets.names[i] := NA]
  }

  #add outlet if present
  if(nrow(inuit[!is.na(`uitlaat (m3)`)]) > 0){
    #add name of uitlaat (could be one out of two)
    uitlaatname <- paste0("Gemaal1|", gsub(" ", "", waterkwaliteit$location_names$meetlocatie1), " OF ", gsub(" ", "", waterkwaliteit$location_names$meetlocatie2))

    #assign
    dt2 <- merge.data.table(dt1, inuit[, c("date", "uitlaat (m3)")], by = "date", all = TRUE)

    #change names
    setnames(dt2, old = "uitlaat (m3)", new = uitlaatname)
  }

  #change name of date
  setnames(dt2, old = "date", new = "Datum")

  #save to output
  fwrite(dt2, file = paste0("output/", waterbalance, "/formatted/series.csv"))

}
