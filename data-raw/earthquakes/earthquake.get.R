require(plyr)
require(dplyr)

earthquakes.get <- function(years,
                            parameters = list(
                              min.magnitude=4,
                              order.by="time-asc"
                            ),
                            .progress = create_progress_bar()){
  
  years.pagination.define <- 1
  
  years.v <- unique(unlist(strsplit(years, split = ",")))
  years.f <- rep(seq_len(ceiling(length(years.v) / years.pagination.define)),each = years.pagination.define,length.out = length(years.v))
  years.chunks <- split(years.v, f = years.f)
  
  if(length(years.chunks) > 1){
    
    # Init the progress bar
    .progress$init(length(years.chunks)+1)
    .progress$step()
    
    # Recursive calls for each chunk
    all.earthquakes <- do.call(rbind.fill,
                         lapply(years.chunks, function(single.chunk) {
                         earthquakes.get(years = single.chunk, parameters = parameters, .progress = .progress)
                         })
    )
    
    .progress$term()
    
    return(all.earthquakes)
    
  }
  
  else {
    
    url <- URLencode(paste0("https://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime=", min(years), "-01-01 00:00:00&endtime=", max(years), "-12-31 23:59:59&minmagnitude=", parameters$min.magnitude, "&orderby=", parameters$order.by))
    
    # Advance the progress bar
    if(inherits(try(.progress$step(), silent=T), "try-error")){
      .progress$init(length(ids)+1)
      .progress$step()
    }
    
    return(read.csv(url(url), stringsAsFactors = FALSE))
  }
  
}

earthquakes <- earthquakes.get(as.character(1996:2015), .progress = create_progress_bar("text"))
save(earthquakes, file="./data/earthquakes.rdata")
