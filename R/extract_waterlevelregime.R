#' Extract waterlevel regime
#'
#'
#' @param filepath filepath of waterbalance excel document
#' @param waterbalance code of waterbalance (e.g. KRWO_04_Kockengen)
#'
#'
#' @export
extract_waterlevelregime <- function(filepath, waterbalance){
  #read
  peil <- suppressMessages(read_excel(filepath, sheet = "WAT", range = "B15:D33", col_names = FALSE, col_types = "list")) |> as.data.table()

  #extract
  result <- list()
  for(i in 1:dim(peil)[1]){
    #name
    param_name <- paste0(peil$...1[[i]], peil$...2[[i]])

    #value
    param_value <- peil$...3[[i]]

    #as.data.table
    param.dt <- data.table(name = param_name, value = as.character(param_value))

    #bind to list
    result[[i]] <- param.dt
  }
  #rbindlist
  result <- rbindlist(result)

  #add name manually (sometimes read_excel changes all to character)
  result$name <- c("Gemaalcapaciteit[m³/min]", "Inlaatcapaciteit[m³/min]", "Peilregime 1NA",
                   "StartdatumNA", "Aantal dagenNA", "EinddatumNA", "Gemaalpeil[m NAP]",
                   "Inlaatpeil[m NAP]", "Peilregime 2[1=ja, 0=nee]", "StartdatumNA",
                   "Aantal dagenNA", "EinddatumNA", "Gemaalpeil[m NAP]", "Inlaatpeil[m NAP]",
                   "Peilregime 3[1=ja, 0=nee]", "StartdatumNA", "EinddatumNA", "Gemaalpeil[m NAP]",
                   "Inlaatpeil[m NAP]")

  #tidy
  result[, name := gsub("\\[|\\]| |NA$", "", name)]
  result[, name := tolower(name)]
  result[, name := gsub("mnap$", " (mnap)", name)]
  result[, name := gsub("1=ja,0=nee", " (1=ja,0=nee)", name)]
  result[, name := gsub("³", "3", name)]
  result[, name := gsub("m3/min", " (m3/min)", name)]

  #extract peilregime1, peilregime2, peilregime3
  pr1 <- list(data = result[c(4,6)],
              gemaalpeil = result[7],
              inlaatpeil = result[8])
  pr2 <- list(active = result[9],
              data = result[c(10,12)],
              gemaalpeil = result[13],
              inlaatpeil = result[14])
  pr3 <- list(active = result[15],
              data = result[c(16,17)],
              gemaalpeil = result[18],
              inlaatpeil = result[19])

  #convert to peilregime
  if(pr2$active$value == 1){
    if(pr3$active$value == 1){
      #drie peilregimes
      peilregime <- data.table(startdag = c(pr1$data[name == "startdatum"]$value,
                                            pr2$data[name == "startdatum"]$value,
                                            pr3$data[name == "startdatum"]$value),
                               `gemaalpeil (mnap)` = c(pr1$gemaalpeil$value,
                                                       pr2$gemaalpeil$value,
                                                       pr3$gemaalpeil$value),
                               `inlaatpeil (mnap)` = c(pr1$inlaatpeil$value,
                                                       pr2$inlaatpeil$value,
                                                       pr3$inlaatpeil$value))
    } else{
      #twee peilregimes
      peilregime <- data.table(startdag = c(pr1$data[name == "startdatum"]$value,
                                            pr2$data[name == "startdatum"]$value),
                               `gemaalpeil (mnap)` = c(pr1$gemaalpeil$value,
                                                       pr2$gemaalpeil$value),
                               `inlaatpeil (mnap)` = c(pr1$inlaatpeil$value,
                                                       pr2$inlaatpeil$value))
    }
  } else{
    #1 peilregime
    peilregime <- data.table(startdag = pr1$data[name == "startdatum"]$value,
                             `gemaalpeil (mnap)` = pr1$gemaalpeil$value,
                             `inlaatpeil (mnap)` = pr1$inlaatpeil$value)
  }

  #save
  saveRDS(peilregime, file = paste0("output/", waterbalance, "/raw/peilregime.RDS"))
}
