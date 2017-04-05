require(openxlsx)
require(plyr)
require(dplyr)

scimago.jr.get <- function(years,
                            parameters = list(
                              category=1908,
                              area=1900
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
    all.papers <- do.call(rbind.fill,
                               lapply(years.chunks, function(single.chunk) {
                                 scimago.jr.get(years = single.chunk, parameters = parameters, .progress = .progress)
                               })
    )
    
    .progress$term()
    
    return(all.papers)
    
  }
  
  else {
    url <- URLencode(paste0("http://www.scimagojr.com/countryrank.php?category=", parameters$category, "&area=", parameters$area, "&year=", min(years), "&out=xls"))

        x <- read.xlsx(url)
    
    # Advance the progress bar
    if(inherits(try(.progress$step(), silent=T), "try-error")){
      .progress$init(length(ids)+1)
      .progress$step()
    }
        
    x$year <- min(years)
    
    return(x)
  }
  
}

all.papers <- scimago.jr.get(as.character(1996:2015), .progress = create_progress_bar("text"))
