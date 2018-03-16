library(rvest)
library(dplyr)
library(httr)
library(xml2)
library(RCurl)



page <- "https://index.rpg.net/display-search.phtml?key=category&value=Core+Rules&match=precise"

page.index <- read_html(page)

page.links <- unique(data.frame(link=page.index %>%
  html_nodes("div.boxframe > div.boxcontent") %>%
  html_nodes("div") %>%
  html_nodes("a") %>%
  html_attr("href"), stringsAsFactors = FALSE) %>%
  slice(grep("mainid=[0-9]+$", link)))

page.links <- as.data.frame(page.links)[,1]

# Remove already crawled games from the list
cached.files <- list.files(file.path(".", "temp"))
cached.ids <- gsub(".html", "", cached.files)

page.links <- setdiff(page.links, sprintf("https://index.rpg.net/display-entry.phtml?mainid=%s", cached.ids))

lapply(page.links, function(x){
  
  id <- getFormParams(x)['mainid']
  file <- file.path(".", "temp", paste(id, "html", sep="."))
  
  h <- read_html(httr::GET(as.character(x), add_headers("user-agent" = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.52 Safari/537.36 OPR/15.0.1147.100")))
  
  message(sprintf("Writing down game %s on %s", id, file))
  
  write_html(h, file, options = "format")
  Sys.sleep(5)

})