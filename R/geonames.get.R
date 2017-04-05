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
    url <- URLencode(paste0("http://ws.geonames.org/search?country=", x[1], "&name=", x[2], "&maxRows=1&username=", username))
    
    #print(url)
    
    one.country <- 
      data.frame(t(sapply(
        xmlToList(xmlTreeParse(getURL(URLencode(paste0("http://ws.geonames.org/search?country=", x[1], "&name=", x[2], "&maxRows=1&username=", username)), ssl.verifypeer=FALSE)
        )$doc$children$geonames)$geoname
        ,c)))
    
    # Advance the progress bar
    if(inherits(try(.progress$step(), silent=T), "try-error")){
      .progress$init(length(ids)+1)
      .progress$step()
    }
    
    return(one.country)}
    
  ), stringsAsFactors = FALSE))
 
  .progress$term() 
  
  return(data.frame(loc, country.data))
  
}

all.countries <- geonames.get("theclue", .progress = create_progress_bar("text"))


