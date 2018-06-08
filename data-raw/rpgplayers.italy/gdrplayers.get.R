######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "lubridate", "rvest", "httr", "RCurl")
}, warning=function(w){
  stop(conditionMessage(w))
})

formatFbDate <- function(datestring, format="datetime") {
  
  date <- NULL
  
  datestring <- gsub("(.*)\\+0[\\d]:00", "\\1+0000", datestring, perl = TRUE)
  
  if (format=="datetime"){
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "Europe/Rome")    
  }
  if (format=="date"){
    date <- as.Date(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "Europe/Rome", origin="1970-01-01")   
  }
  return(date)
}

gdrplayers.prepare <- function(cache.dir, verbose = TRUE){
  
  cached.files <- list.files(cache.dir)

  gdrplayers <- do.call(plyr::rbind.fill, lapply(file.path(cache.dir, cached.files), function(x){
    
    page <- tryCatch({ 
      con <- file(x, "rb", blocking = FALSE)
      p <- read_html(con)
      close(con)
      p
    },
    error = function(e){
      close(con)
      stop(sprintf("Error parsing file %s: %s", x, conditionMessage(e)))
    })
    
    post.df <- tryCatch({
      
      post.link <- gsub("\\.\\/gdrplayers(.*)(\\.html)", "https://www.gdrplayers.it\\1", x, perl = TRUE)
      
      post.node <- page %>%  html_nodes(xpath = "//div[contains(@id, 'post-')]")
      
      post.id <- gsub("post-([0-9]+)", "\\1", 
                      post.node %>%
                        html_attr("id"),
                      perl = TRUE)
      
      post.title <-  as.character(post.node %>% 
                                    html_nodes(xpath = "div[contains(@class, 'title')]//h2[1]/text()")
      )
      
      author.name <- as.character(post.node %>% 
                                    html_nodes(xpath = "div[contains(@class, 'title')]//a[contains(@rel,'author')]//text()")
      )
      
      author.link <- post.node %>% html_nodes(xpath = "div[contains(@class, 'title')]//a[contains(@rel,'author')]") %>% html_attr("href")
      
      author.id <- gsub("^https?:\\/\\/www\\.gdrplayers\\.it\\/author\\/([^\\/]+)\\/$", "\\1",
                        author.link,
                        perl  = TRUE)
      
      post.content <- gsub("\r?\n|\r", " ", as.character(post.node %>% html_nodes(xpath = "div[contains(@class,'entry')]//p//text()")))
      post.content <- gsub("\t+", "", post.content)
      post.content <- paste(post.content, collapse="")
      
      category.tags <- paste(post.node %>% html_nodes(xpath = "p[contains(@class,'abstract postmetadata')]//a[contains(@rel,'category tag')]//text()"), collapse=", ")

      post.dt <- formatFbDate(page %>% html_nodes(xpath = "//head/meta[contains(@property, 'article:published_time')]") %>% html_attr("content"))
      
      location.literal <- paste(post.node %>% html_nodes(xpath = "div[contains(@class, 'title')]//a[contains(@rel,'tag')]") %>% html_text() ,
                                collapse=", ")
      
      comments.num <- as.integer(as.character(post.node %>%  html_nodes(xpath = "p[contains(@class,'abstract comments')]//a//text()")))
      
      post.df <- data.frame(post.id,
                            post.link = post.link,
                            author.id,
                            author.name,
                            author.link,
                            post.dt,
                            categories = ifelse(nchar(category.tags)==0, NA, category.tags),
                            locations = ifelse(nchar(location.literal)==0, NA, location.literal),
                            post.title = URLdecode(post.title),
                            post.content = URLdecode(post.content),
                            comments.num = ifelse(is_empty(comments.num), 0, comments.num),
                            stringsAsFactors = FALSE
      )
      
      message(paste("Parsing:", x, sep=" "))
      return(post.df)
      
    }, error = function(e) {
      message(paste("Skipping:", x, sep=" "))
      return(data.frame())
    })
    return(post.df)
  }))
  
}

gdrplayers.cache.page <- function(page, verbose = TRUE){
  
  gdrplayers.posts.links <- page %>% html_nodes(".post-excerpt") %>% html_node("h3 > a") %>% html_attr("href")
  
  cached.files <- list.files(file.path(".", "gdrplayers"))
  cached.ids <- gsub(".html", "", cached.files)
  
  page.links <- setdiff(gdrplayers.posts.links, sprintf("https://www.gdrplayers.it/%s/", cached.ids))
  
  lapply(page.links, function(x){
    
    post.df <- tryCatch({
      
      page <- read_html(httr::GET(as.character(x), add_headers("user-agent" = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.52 Safari/537.36 OPR/15.0.1147.100")))
      
      file <- file.path(".", "gdrplayers", paste(gsub("https:\\/\\/www\\.gdrplayers\\.it\\/(.+)?(?:\\/)", "\\1", x, perl = TRUE), "html", sep="."))
      write_html(page, file, options = "format")
      
      message(sprintf("Writing down %s", file))
      
      Sys.sleep(5)
      
      
    })
  })
  
}

########################
# CRAWL ALL THE PAGES  #
########################
next.page <- "https://www.gdrplayers.it/blog/"
gdrplayers <- data.frame()

repeat {
  message(paste("Scraping:", next.page, sep=" "))
  gdrplayers.home <- read_html(next.page)
  gdrplayers.cache.page(gdrplayers.home, verbose = TRUE)
  next.page <- gdrplayers.home %>% html_nodes(xpath = "//div[@id='navigation']//a[contains(.,'vecchie')]") %>% html_attr("href")
  if(nchar(next.page)==0) break()
}
  
########################
# GET ALL THE PAGES    #
########################
gdrplayers.cache.dir <- file.path(".", "gdrplayers")
gdrplayers <- gdrplayers.prepare(gdrplayers.cache.dir, verbose = TRUE)
