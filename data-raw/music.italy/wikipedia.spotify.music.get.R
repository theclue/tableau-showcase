######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "rvest", "httr", "RCurl", "jsonlite", "futile.logger")
  p_load_gh("tiagomendesdantas/Rspotify")
}, warning=function(w){
  stop(conditionMessage(w))
})

##########
# SETUP  #
##########

base.url <- "https://it.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:%s&cmlimit=500&format=json"
wikipedia.cat <- c("Cantautori_italiani_del_XX_secolo",
                   "Cantautori_italiani_del_XXI_secolo",
                   "Cantanti_italiani_del_XX_secolo",
                   "Cantanti_italiani_del_XXI_secolo")


##########################
# CRAWL WIKIPEDIA PAGES  #
##########################
italian.artists <- unique(do.call(plyr::rbind.fill, lapply(wikipedia.cat, function(cat){
  itm <- data.frame()
  next.page <- sprintf(base.url, cat)
  repeat {
    new.page <- fromJSON(next.page, simplifyVector = TRUE, flatten = TRUE)
    itm <- plyr::rbind.fill(itm, new.page$query$categorymembers)
    cmcontinue <- new.page$continue$cmcontinue
    if(is.null(cmcontinue)) break()
    next.page <- paste(sprintf(base.url, cat), paste("cmcontinue", cmcontinue, sep = "="), sep = "&")
  }
  
  itm$title <- gsub("\\(.*?\\)", "", itm$title, perl = TRUE)
  itm$title <- gsub("Categoria:", "", itm$title, perl = TRUE)
  itm$title <- trimws(itm$title)
  
  flog.info(sprintf("Scraping %s: %d artists found", cat, nrow(itm)))
  
  return(itm)
}))) %>% select(-ns)

#######################
# CRAWL SPOTIFY DATA  #
#######################
spotify.keys <- spotifyOAuth("sna.spotify","","")
