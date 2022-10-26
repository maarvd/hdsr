#' Set folder structure for extracting excel data
#'
#'
#' @param datafolder folder where all excel waterbalances are located
#'
#'
#'
#' @export
set_folderstructure <- function(datafolder){
  #list excelfiles
  filelist <- list.files(datafolder, pattern = ".xls$|.xlsm$")

  #tidy
  filelist <- gsub(".xls$|.xlsm$", "", filelist)

  #create output folder
  if(!dir.exists("output")){dir.create("output")}

  #for each, create folder sturcture
  lapply(filelist, FUN = function(x){
    if(!dir.exists(paste0("output/", x))){dir.create(paste0("output/",x))}
    if(!dir.exists(paste0("output/",x, "/raw"))){dir.create(paste0("output/", x, "/raw"))}
    if(!dir.exists(paste0("output/",x, "/formatted"))){dir.create(paste0("output/", x, "/formatted"))}
    if(!dir.exists(paste0("output/",x, "/spatial"))){dir.create(paste0("output/", x, "/spatial"))}
  })
}
