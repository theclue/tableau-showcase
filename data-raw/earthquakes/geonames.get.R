require(plyr)
require(dplyr)
require(XML)
require(RCurl)

geonames.get <- function(username,
                         .progress = create_progress_bar()){
  
  loc <- 
    xmlToDataFrame(
      xmlParse(getURL(paste0("http://ws.geonames.org/countryInfo?username=", username), ssl.verifypeer=FALSE)
      ), stringsAsFactors = FALSE    )
  
  # Init the progress bar
  .progress$init(nrow(loc)+1)
  .progress$step()
  
  country.data <- do.call(rbind.fill, list(apply(loc[,1:2], 1, function(x){
    one.country <- 
      data.frame(t(sapply(
        xmlToList(xmlTreeParse(getURL(URLencode(paste0("http://ws.geonames.org/search?country=", x[1], "&name=", x[2], "&maxRows=1&username=", username)), ssl.verifypeer=FALSE)
        )$doc$children$geonames)$geoname
        ,c)), stringsAsFactors = FALSE)
    
    # Advance the progress bar
    if(inherits(try(.progress$step(), silent=T), "try-error")){
      .progress$init(length(ids)+1)
      .progress$step()
    }
    
    return(one.country)}
    
  ), stringsAsFactors = FALSE))
  
  .progress$term() 
  
  all.data <- data.frame(loc, country.data, stringsAsFactors = FALSE)

  all.data$latitude <- as.numeric(all.data$lat)
  all.data$longitude <- as.numeric(all.data$lng)
  all.data$west <- as.numeric(all.data$west)
  all.data$north <- as.numeric(all.data$north)
  all.data$east <- as.numeric(all.data$east)
  all.data$south <- as.numeric(all.data$south)
  
  all.data$areaInSqKm <- as.numeric(all.data$areaInSqKm)
  all.data$population <- as.numeric(all.data$population)
  
  all.data$lat <- NULL
  all.data$lng <- NULL
  all.data$countryCode.1 <- NULL
  all.data$countryName.1 <- NULL
  
  
  return(all.data)
  
}

countries <- geonames.get("theclue", .progress = create_progress_bar("text"))
save(countries, file="./data/countries.rdata")


