######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "rvest", "httr", "RCurl", "jsonlite", "futile.logger", "httpuv", "igraph")
  p_load_gh("tiagomendesdantas/Rspotify")
}, warning=function(w){
  stop(conditionMessage(w))
})

##########
# SETUP  #
##########

spotify_client_id = Sys.getenv("SPOTIFY_CLIENT_ID")
spotify_client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")

base.url <- "https://it.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:%s&cmlimit=500&format=json"
wikipedia.cat <- c("Cantautori_italiani_del_XX_secolo",
                   "Cantautori_italiani_del_XXI_secolo",
                   "Cantanti_italiani_del_XX_secolo",
                   "Cantanti_italiani_del_XXI_secolo",
                   "Gruppi_musicali_italiani")

content.query <- "https://it.wikipedia.org/w/api.php?action=parse&prop=text&page=%s&format=json"

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

  itm$title <- gsub("Categoria:", "", itm$title, perl = TRUE)

  itm$raw <- itm$title
  itm$raw <- trimws(itm$raw)
  
  itm$title <- gsub("\\(.*?\\)", "", itm$title, perl = TRUE)
  itm$title <- trimws(itm$title)
  
  flog.info(sprintf("Scraping %s: %d artists found", cat, nrow(itm)))
  
  return(itm)
}))) %>% select(-ns)

italian.artists <- italian.artists %>% filter(!grepl("Gruppi", title, ignore.case = TRUE))

# Get some synoptics
italian.artists.df <- do.call(plyr::rbind.fill, lapply(seq_len(NROW(italian.artists)), function(i, data){
  
  artist <- data[i,]$raw
  
  flog.info(sprintf("Parsing: %s", artist))
  
  a <- fromJSON(sprintf(content.query, URLencode(artist)), simplifyVector = TRUE, flatten = TRUE)
  
  if(!is.null(a[['parse']])){
    
    html <- read_html(a[["parse"]][["text"]][["*"]])
    
    genre <- trimws(html %>% html_nodes(xpath = "//table[contains(@summary, 'dati principali') and contains(@class, 'sinottico')]/tbody/tr[th[contains(.,'Genere')]]/td/a[1]")  %>% html_text())
    start.activity <- as.integer(gsub("^([\\d]{4}).*",
                                      "\\1",
                                      trimws(html %>% html_nodes(xpath = "//table[contains(@summary, 'dati principali') and contains(@class, 'sinottico')]/tbody/tr[th[contains(.,'Periodo di attività musicale')]]/td")  %>% html_text()),
                                      perl = TRUE))
   
  row <- data.frame(id = data[i,]$pageid,
                    name = data[i,]$title,
                    stringsAsFactors = FALSE)
  
  row$genre <- ifelse(length(genre > 0), genre, NA)
  row$start.activity <- ifelse(length(start.activity > 0), start.activity, NA)
  
  return(row)

  }
  
  return(NULL)
  
}, data = italian.artists))


#######################
# CRAWL SPOTIFY DATA  #
#######################
spotify.keys <- spotifyOAuth("sna.spotify", spotify_client_id, spotify_client_secret)

italian.artists.related <- do.call(plyr::rbind.fill, lapply(seq_len(NROW(italian.artists.df)), function(i, data){
  
  Sys.sleep(.5)
  
  artist <- data[i,]$name
  
  flog.info(sprintf("Get Spotify related artists for: %s", artist))
  
  related <- tryCatch(getRelated(artist, spotify.keys),
                      error = function(e) {
                        flog.error(sprintf("Error getting related artists for %s: %s", artist, e$message))
                        return(NULL)
                      })
  
  if(NROW(related) > 0){
    related$source = artist
    return(related)
  }
  
  return(NULL)
  
}, data = italian.artists.df))

italian.artists.spotify <- unique(italian.artists.related %>% select(-source))

italian.artists.df$name.tolow <- tolower(italian.artists.df$name)
italian.artists.spotify$name.tolow <- tolower(italian.artists.spotify$name)

italian.artists.lookup <- left_join(italian.artists.df, italian.artists.spotify %>% select(-name), by="name.tolow") %>%
  select(-type) %>%
  group_by(name, name.tolow, id.y) %>%
  summarise(genre = first(genre),
            wikipedia.id = min(id.x),
            popularity = min(popularity),
            followers = first(followers),
            start.activity = first(start.activity)) %>%
  mutate(spotify.id = as.character(id.y)) %>%
  select(wikipedia.id, name, genre, spotify.id, start.activity, popularity, followers, name.tolow) %>%
  ungroup()

# If an artist has not been appointed to be related to, it doesn't show on the Spotify dataset
# Thus, we need to crawl from the single-artist endpoint

italian.artists.missing <- italian.artists.lookup %>% filter(is.na(spotify.id))
italian.artists.spotify.missing <- do.call(plyr::rbind.fill, lapply(seq_len(NROW(italian.artists.missing)), function(i, data){
  
  Sys.sleep(.2)
  
  artist <- data[i,]$name.tolow
  
  flog.info(sprintf("Lookup missing data for: %s", data[i,]$name))
  
  related <- tryCatch(searchArtist(artist, spotify.keys),
                      error = function(e) {
                        flog.error(sprintf("Error getting related artists for %s: %s", artist, e$message))
                        return(NULL)
                      })
  
  if(NROW(related) > 0){
    related$name.tolow = artist
    return(related[1,])
  }
  
  return(NULL)
  
}, data = italian.artists.missing))

italian.artists.lookup.missing <- left_join(italian.artists.missing %>% select(-name, -spotify.id, -popularity, -followers), italian.artists.spotify.missing %>% mutate(name = display_name), by="name.tolow") %>%
  select(-type) %>%
  group_by(name, name.tolow, id) %>%
  summarise(genre = first(genre),
            wikipedia.id = min(wikipedia.id),
            popularity = min(popularity),
            followers = first(followers),
            start.activity = first(start.activity)) %>%
  mutate(spotify.id = as.character(id)) %>%
  select(wikipedia.id, name, genre, spotify.id, start.activity, popularity, followers, name.tolow) %>%
  filter(!is.na(spotify.id))
  ungroup()

########################
# ARTISTS DATASET      #
########################
italian.artists.clean <- unique(plyr::rbind.fill(italian.artists.lookup %>% filter(!is.na(spotify.id)), italian.artists.lookup.missing))
italian.artists.clean <- italian.artists.clean %>% filter(name %in% names(table(italian.artists.clean$name))[which(table(italian.artists.clean$name) == 1)]) %>%
  select(-name.tolow) %>%
  select(spotify.id, name, genre, wikipedia.id, start.activity, popularity, followers)
 
# Those crawled from Spotify's rerun are finally checked for related once more
italian.artists.missing.related <- do.call(plyr::rbind.fill, lapply(seq_len(NROW(italian.artists.lookup.missing)), function(i, data){
  
  Sys.sleep(.2)
  
  artist <- data[i,]$name
  
  flog.info(sprintf("Get Spotify related artists for missing record: %s", artist))
  
  related <- tryCatch(getRelated(artist, spotify.keys),
                      error = function(e) {
                        flog.error(sprintf("Error getting related artists for %s: %s", artist, e$message))
                        return(NULL)
                      })
  
  if(NROW(related) > 0){
    related$source = artist
    return(related)
  }
  
  return(NULL)
  
}, data = italian.artists.lookup.missing))

########################
# ARTISTS' CONNECTIONS #
########################
italian.artists.related.italian <- plyr::rbind.fill(italian.artists.related, italian.artists.missing.related) %>%
  filter(name %in% italian.artists.clean$name & source %in% italian.artists.clean$name) %>%
  left_join(italian.artists.clean %>% select(name, spotify.id), by=c("source"="name")) %>%
  mutate(target.name = as.character(name), target = as.character(id), source.name = source, source = spotify.id) %>%
  select(source, target, source.name, target.name)

italian.artists.related.italian <- unique(italian.artists.related.italian)

#######################
# NETWORK GENERATION  #
#######################

##########
# Directed Graph
# -
# Use the start.activity to set the influence. If a 'related' has started her music career paste the parent
# for at least 3 years it's safe to assume that it was influenced by whom.
# Other links are reversed and, if duplicated, are then removed

influence.threshold <- 5

influences.edges <- unique(left_join(italian.artists.related.italian,
                              italian.artists.clean %>% select("spotify.id", start.activity),
                              by=c("source" = "spotify.id")) %>%
  mutate(source.start = start.activity) %>%
  select(-start.activity) %>%
  left_join(italian.artists.clean %>% select(spotify.id, start.activity),
            by=c("target" = "spotify.id")) %>%
  mutate(target.start = start.activity) %>%
  select(-start.activity) %>%
  filter(abs(target.start - source.start) >= influence.threshold) %>%
  mutate(target.flip = ifelse(target.start <= source.start - influence.threshold, target, source)) %>%
  mutate(target.name.flip = ifelse(target.start <= source.start - influence.threshold, target.name, source.name)) %>%
  mutate(source.flip = ifelse(target.start <= source.start - influence.threshold, source, target)) %>%
  mutate(source.name.flip = ifelse(target.start <= source.start - influence.threshold, source.name, target.name)) %>%
  mutate(source = source.flip, source.name = source.name.flip, target = target.flip, target.name = target.name.flip) %>%
  select(source,target))

italian.music <- graph_from_data_frame(influences.edges,
                                       directed = TRUE,
                                       vertices = italian.artists.clean)

italian.music <- simplify(italian.music)

V(italian.music)$label <- V(italian.music)$name
V(italian.music)$spotify.id <- italian.artists.clean$spotify.id

#################
# OUTPUT FILES  #
#################
write.graph(italian.music, file = "../../data/italian-music/italian-music.graphml", format=c("graphml"))
write.csv(italian.artists.clean, file = "../../data/italian-music/italian.artists.csv", row.names = FALSE)
write.csv(influences.edges, file = "../../data/italian-music/italian.influences.csv", row.names = FALSE)

