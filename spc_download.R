library(httr)
#
# Change this to the data folder
setwd(getSrcDirectory()[1])
data_folder <- "data"
#
data_last_mod <- readLines(paste0(data_folder,"/last_updated.txt"))
spc_url <- "http://www.spc.noaa.gov/products/outlook/day1otlk-shp.zip"
last_mod <- (headers(HEAD(url=spc_url)))$`last-modified`
#
if(last_mod!=data_last_mod){
  time_now <- paste0("spc_",format(Sys.time(),format="%Y%m%d%H%M%S"))
  new_folder <- paste0(data_folder,"/",time_now)
  dir.create(new_folder)
  download.file(url=spc_url,paste0(new_folder,"/file.zip"))
  utils::unzip(paste0(new_folder,"/file.zip"),exdir = new_folder)
  if(file.exists(paste0(new_folder,"/day1otlk.info"))){
    file.remove(paste0(new_folder,"/file.zip"))
  }
  format_date <- lubridate::as_datetime(last_mod,format="%a, %d %b %Y %H:%M:%S")
  out_df <- data.frame(
    `Date_Time` = format_date,
    `Folder` = new_folder
  )
  write.table(out_df,paste0(data_folder,"/data.csv"),
              append=T,col.names=F,row.names = F,
              sep = ","
  )
  writeLines(last_mod,paste0(new_folder,"/out.txt"))
  writeLines(last_mod,paste0(data_folder,"/last_updated.txt"))
}
