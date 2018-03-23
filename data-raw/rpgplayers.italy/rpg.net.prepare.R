######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "lubridate", "rvest", "plyr")
}, warning=function(w){
  stop(conditionMessage(w))
})

cached.files <- list.files(file.path(".", "temp"))

rpg.list <- do.call(plyr::rbind.fill, lapply(cached.files, function(x){

  page <- tryCatch({ 
    con <- file(file.path(".", "temp", x), "rb", blocking = FALSE)
    read_html(con)
    },
    error = function(e){
      close(con)
      stop(sprintf("Error parsing file %s: %s", x, conditionMessage(e)))
    })
    
    id <- as.numeric(gsub(".html", "", basename(x)))

     table.box <- page %>% html_nodes("table.boxWide") %>% html_nodes("table.boxWide")
     
     # if(length(table.box)==0) table.box <- page %>% html_nodes("table.boxWide")
     
     title     <- str_replace_all(trimws(table.box %>% html_nodes(xpath = "tr[td[contains(.,'Title')]]/td[2]") %>% html_text()), "[\r\n]" , "")
     author    <- trimws(table.box %>% html_nodes(xpath = "tr[td[contains(.,'Author')]]/td[2]") %>% html_text())
     book.type <- trimws(table.box %>% html_nodes(xpath = "tr[td[contains(.,'Book Type')]]/td[2]") %>% html_text())
     genre     <- trimws(table.box %>% html_nodes(xpath = "tr[td[contains(.,'Genre')]]/td[2]") %>% html_text())
     setting   <- trimws(table.box %>% html_nodes(xpath = "tr[td[contains(.,'Setting')]]/td[2]/a[1]") %>% html_text())
     award     <- paste(trimws(table.box %>% html_nodes(xpath = "tr[td[contains(.,'Award')]]/td[2]") %>% html_nodes("a") %>% html_text()), collapse = ", ")
     
     message(sprintf("Parsing game %d: %s", id, title))

     edition.yyyy <- trimws(table.box %>% html_nodes(xpath = "tr[td[contains(.,'Edition') and contains(.,'Info')]]/td[2]") %>% html_text())
     year <- as.numeric(gsub("(.*)(\\(([0-9]+).*\\))", "\\3", edition.yyyy))
     edition <- trimws( gsub("(.*)(\\(([0-9]+).*\\))", "\\1", edition.yyyy))

    # Get Edition Data from Game Edition box if not available on the metadata box
    editions.box <- page %>% html_nodes("div#editions")

    editions.num <- as.numeric(editions.box %>% html_nodes(xpath = "b[1]") %>% html_text())
    year.details <- as.numeric(trimws(editions.box %>%
      html_nodes("div.boxLined") %>%
      html_nodes("table.boxWide") %>%
      html_nodes(xpath = "tr[2]/td[5]") %>% html_text()))

    edition.details <- trimws(editions.box %>%
                                        html_nodes("div.boxLined") %>%
                                        html_nodes("table.boxWide") %>%
                                        html_nodes(xpath = "tr[2]/td[4]") %>% html_text())


    system <- trimws(table.box %>% html_nodes(xpath = "tr[td[contains(.,'System')]]/td[2]") %>% html_text())

    summary <- trimws(page %>% html_nodes("div#summary > div.blockquote") %>% html_text())

    rating <- as.numeric(page %>% html_nodes("div.topTab") %>% html_nodes("a") %>% html_text())
    rank <- as.numeric(table.box %>% html_nodes(xpath = "tr[td[contains(.,'Book Type')]]/td[3]") %>% html_nodes("a") %>% html_text())

    img <- trimws(page %>% html_nodes(xpath = "//img[contains(@src,'pictures')]") %>% html_attr("src"))

    url <- sprintf("https://index.rpg.net/display-entry.phtml?mainid=%s", id)

    return(data.frame(id = id,
                      title = ifelse(!is_empty(title), title, NA),
                      authors = ifelse(!is_empty(author), author, NA),
                      book.type = ifelse(!is_empty(book.type), book.type, NA),
                      genres = ifelse(!is_empty(genre), genre, NA),
                      settings = ifelse(!is_empty(setting), setting, NA),
                      system = ifelse(!is_empty(system), system, NA),
                      awards = ifelse(!is_empty(award), award, NA),
                      edition = ifelse(!is_empty(edition), edition, ifelse(!is_empty(edition.details), edition.details, NA)),
                      year = ifelse(!is_empty(year), year, ifelse(!is_empty(year.details), year.details, NA)),
                      editions.num = ifelse(!is_empty(editions.num), editions.num, NA),
                      rating = ifelse(!is_empty(rating), rating, NA),
                      rank = ifelse(!is_empty(rank), rank, NA),
                      url = ifelse(!is_empty(url), url, NA),
                      img.url = ifelse(!is_empty(img), img, NA),
                      stringsAsFactors = FALSE))
   
}))
  