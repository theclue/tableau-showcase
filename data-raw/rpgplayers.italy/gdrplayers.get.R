######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "lubridate", "rvest")
}, warning=function(w){
  stop(conditionMessage(w))
})

gdrplayers.get.page <- function(page, verbose = TRUE){
  
  gdrplayers.posts.links <- page %>% html_nodes(".post-excerpt") %>% html_node("h3 > a") %>% html_attr("href")
  
  page1 <- do.call(plyr::rbind.fill, lapply(gdrplayers.posts.links, function(x){
    
    post.df <- tryCatch({
      
      page <- read_html(x)
      
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
      
      post.dt.literal <- gsub(".*(\\d{1,2}\\s[[:alpha:]]{3}\\s\\d{1,4}).*$", "\\1",
                              paste(post.node %>% 
                                      html_nodes(xpath = "div[contains(@class, 'title')]//p[contains(@class,'abstract')]//text()"),
                                    collapse="")
      )
      
      post.dt <- parse_date_time(post.dt.literal, "d b y", locale = "Italian_Italy.1252")
      
      location.literal <- paste(post.node %>% html_nodes(xpath = "div[contains(@class, 'title')]//a[contains(@rel,'tag')]") %>% html_text() ,
                                collapse=", ")
      
      comments.num <- as.integer(as.character(post.node %>%  html_nodes(xpath = "p[contains(@class,'abstract comments')]//a//text()")))
      
      post.df <- data.frame(post.id,
                            post.link = x,
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

########################
# GET ALL THE PAGES    #
########################
next.page <- "http://www.gdrplayers.it/blog/"
gdrplayers <- data.frame()

repeat {
  message(paste("Scraping:", next.page, sep=" "))
  gdrplayers.home <- read_html(next.page)
  gdrplayers <- plyr::rbind.fill(gdrplayers, gdrplayers.get.page(gdrplayers.home, verbose = TRUE))
  next.page <- gdrplayers.home %>% html_nodes(xpath = "//div[@id='navigation']//a[contains(.,'vecchie')]") %>% html_attr("href")
  if(nchar(next.page)==0) break()
}