###########################################
# gdrplayers.prepare.R                    #
# --------------------------------------- #
# Prepare italian RPG players for Viz     #
###########################################

#################
# Requirements  #
#################
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse",
               "tm",
               "RWeka",
               "topicmodels",
               "SnowballC",
               "tidytext",
               "scales",
               "igraph",
               "RTextTools",
               "koRpus",
               "lubridate",
               "rJava",
               "RWeka",
               "data.table",
               "xml2",
               "pbapply")

## Source useful functions on gist
invisible(lapply(c("https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions/optimal_k.R",
                   "https://gist.githubusercontent.com/theclue/a4741899431b06941c1f529d6aac4387/raw/f69d9b5a420e2c4707acad69f31c6e6a3c15e559/ggplot-multiplot.R",
                   "https://gist.githubusercontent.com/mrdwab/11380733/raw/83c8eeb156a49c653ed31c7cda67b36281173ee6/cSplit.R") ,devtools::source_url))

invisible(lapply(file.path(".", c("rpg.synonyms.R")), source))


unescape_html <- function(str){
  do.call(c, pblapply(str, function(x){
    xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>")))  
  }))
}


############################
# Initial Set-up
#####

.jinit(parameters="-Xmx128g")

# TODO it's needed on Linux, but it gets Windows processing slowly
options(mc.cores=1)


#############
# Load Data
#####

load(file.path("..", "..", "data", "rpg", "gdrplayers.rda"))
load(file.path("..", "..", "data", "rpg", "rpg.list.rda"))

# Integrating some italian stuff to rpg lists
rpg.list <- plyr::rbind.fill(rpg.list,
                             data.frame(title = c("Sine Requie",
                                                  "Anime e Sangue",
                                                  "Destino Oscuro",
                                                  "Cielo Cremisi",
                                                  "Project H.o.p.e.",
                                                  "L'Ultima Torcia",
                                                  "Antiche Leggende",
                                                  "L'era di Zargo",
                                                  "Nostalgia: la Flotta Nomade"),
                                        book.type = rep("Core Rules", 9),
                                        authors = c("Matteo Cortini, Leonardo Moretti",
                                                    "Matteo Cortini",
                                                    NA,
                                                    NA,
                                                    NA,
                                                    "Matteo Cortini",
                                                    "Carlo Barletta",
                                                    "Tito Leati",
                                                    "Tommaso De Benetti, Luca Vanin"),
                                        genres = c("Horror / Post-Apocalyptic",
                                                   "Action",
                                                   "Generic",
                                                   "Super Hero",
                                                   "Horror",
                                                   "Fantasy",
                                                   "Fantasy",
                                                   "Fantasy",
                                                   "Science Fiction"),
                                        settings = rep(NA, 9),
                                        system = c(rep(NA, 8), "Monad"),
                                        awards = c("Best of Show", rep(NA, 8)),
                                        edition = c("Asterion Press",
                                                    "Raven Distributions",
                                                    "Acchiappasogni",
                                                    "Acchiappasogni",
                                                    "Limana Humanita",
                                                    "Serpentarium Games",
                                                    "Brutaka Press",
                                                    NA,
                                                    "The World Anvil"),
                                        year = c(2003,
                                                 2012,
                                                 2012,
                                                 2012,
                                                 2011,
                                                 2016,
                                                 2017,
                                                 2017,
                                                 2017),
                                                 stringsAsFactors = FALSE))


italian.places <- read.csv(file = file.path("..", "..", "data", "common","italian.places.csv"),
                               sep = ";",
                               header = TRUE,
                               quote= "",
                               stringsAsFactors = FALSE)

rpg.abbreviations <- read.csv(file.path("..", "..", "data", "rpg", "rpg.abbreviations.csv"), stringsAsFactors = FALSE)

rpg.list.norm <- left_join(rpg.list, rpg.abbreviations, by = "id") %>%
  mutate(title.abbreviated = ifelse(is.na(title.abbreviated), tolower(gsub("[[:punct:] ]+", " ", title)), title.abbreviated))


############################
# Data Preprocessing
# ------------------
# Data is cleaned beforer being loaded
# into a corpus
#####


gdrplayers <- gdrplayers %>% filter(grepl("Ricerca giocatori", categories))

gdrplayers$post.title <- unescape_html(gdrplayers$post.title)
gdrplayers$post.content <- unescape_html(gdrplayers$post.content)

gdrplayers$role <- 
  ifelse((grepl("(ricerca|cerco|cerca|cerchiamo|cercando|cercasi|cerca|cercavo|vorrei trovare|ricerca)([^\\s]* ){1,4}(avventurier|giocator|element|component|person|player|gente)(.*)", paste(gdrplayers$post.title, gdrplayers$post.content, sep = ". "), ignore.case = TRUE, perl = TRUE) |
           grepl("(crea|creare|creando|forma|formare|formando|amplia|ampliare|ampliando)([^\\s]* ){1,4}grupp(.*)", paste(gdrplayers$post.title, gdrplayers$post.content, sep = ". "), ignore.case = TRUE, perl = TRUE) |
           grepl("(sono|gioco|offro|offresi)([^\\s]* ){1,3}(master|narratore|dm|storyteller|custode|dungeon master|gm|game master)([^\\s]* )(.*)", paste(gdrplayers$post.title, gdrplayers$post.content, sep = ". "), ignore.case = TRUE, perl = TRUE)),
         "Game Master",
         ifelse(grepl("(ricerca|cercasi|cerco|cerca|cerchiamo|cercando|cercavo|chiedo|chiedere|chiedendo|chiedevo|unire|unirti|unirvi|trovare)([^\\s]* ){1,4}grupp(.*)", paste(gdrplayers$post.title, gdrplayers$post.content, sep = ". "), ignore.case = TRUE, perl = TRUE) |
                  grepl("sono([^\\s]* ){1,3}(giocatore|giocatrice)([^\\s]* ){1,5}(cerco|cercando|unir)(.*)", paste(gdrplayers$post.title, gdrplayers$post.content, sep = ". "), ignore.case = TRUE, perl = TRUE) |
                  grepl("(ricerca|cercasi|cerco|cerchiamo|cercando|cerca|cercavo|vorrei trovare)([^\\s]* ){1,4}(party|master|narratore|storyteller|custode|gm|game master|dm|dungeon master|campagna)(.*)", paste(gdrplayers$post.title, gdrplayers$post.content, sep = ". "), ignore.case = TRUE, perl = TRUE) | 
                  grepl("aggregare", paste(gdrplayers$post.title, gdrplayers$post.content, sep = ". "), ignore.case = TRUE, perl = TRUE),                      
                "Player", NA )
  )

###########################
# Corpus Creation
# ---------------
# The corpus is created and cleaned, while
# relevant metadata are added to it
#####

# Load the corpus
# I decided to do text transformations outside the Corpus since i'm using a virtual one

games.vector <- paste(gdrplayers$post.title,
                      gdrplayers$post.content,
                      sep = " ")

games.vector <- tolower(games.vector)
games.vector <- resolveRpgSynonyms(games.vector)

gdrplayers.corpus <- VCorpus(VectorSource(games.vector))

# Clean up the text
gdrplayers.corpus <- tm_map(gdrplayers.corpus, stripWhitespace)

# Coercing to PlainTextDocument
#gdrplayers.corpus <-  tm_map(gdrplayers.corpus, PlainTextDocument)

# Metadata
DublinCore(gdrplayers.corpus, tag="identifier") <- gdrplayers$post.id
DublinCore(gdrplayers.corpus, tag="creator") <- gdrplayers$author.id
DublinCore(gdrplayers.corpus, tag="title") <- gdrplayers$post.title
DublinCore(gdrplayers.corpus, tag="date") <- gdrplayers$post.dt
DublinCore(gdrplayers.corpus, tag="source") <- gdrplayers$post.link
meta(gdrplayers.corpus, tag="role") <- gdrplayers$role
meta(gdrplayers.corpus, tag="replies") <- gdrplayers$comments.num
meta(gdrplayers.corpus, tag="place") <- gdrplayers$locations

############################
# Phase 3: Adjacence Matrix
# Matrix is built against a dictionary of known gdr names
#####

FourGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))

# term-document matrix
gdrplayers.tdm <- TermDocumentMatrix(gdrplayers.corpus, control = list(tokenize = FourGramTokenizer, dictionary = rpg.list.norm$title.abbreviated))

# document-term matrix
gdrplayers.dtm <- DocumentTermMatrix(gdrplayers.corpus, control = list(tokenize = FourGramTokenizer,  dictionary = rpg.list.norm$title.abbreviated))

# adjacence matrix
gdrplayers.tdm.matrix <- as.matrix(gdrplayers.tdm)
colnames(gdrplayers.tdm.matrix) <- meta(gdrplayers.corpus, type="indexed")$id

gdrplayers.adj.matrix <- gdrplayers.tdm.matrix
gdrplayers.adj.matrix[gdrplayers.tdm.matrix>=1] <- 1
gdrplayers.adj.matrix <- gdrplayers.adj.matrix %*% t(gdrplayers.adj.matrix)

#############################
# Phase 4: Attributes Fixing
#####

rpg.list.norm$main.genre <- gsub("^(.*)[\\s]+\\/[\\s](.*)$", "\\1", perl = TRUE, rpg.list$genres)
rpg.list.norm$sub.genre <- gsub("^(.*)[\\s]+\\/[\\s](.*)$", "\\2", perl = TRUE, rpg.list$genres)

######################################
# Phase 5: Citation-level Detail Cube
#####

gdrplayers.tableau <- as.data.frame(as.table(gdrplayers.tdm.matrix))
gdrplayers.tableau <- gdrplayers.tableau[which(gdrplayers.tableau$Freq > 0),]

gdrplayers.tableau <- left_join(gdrplayers.tableau,
                                gdrplayers,
                                by = c("Docs" = "post.id"))

# gdrplayers.tableau <- left_join(gdrplayers.tableau,
#                                 rpg.list.norm,
#                                 by = c("Terms" = "title.abbreviated"))

gdrplayers.tableau$Freq <- 1

# Get only the first place
gdrplayers.tableau$location <- gsub("([^,]+).*", "\\1", perl = TRUE, gdrplayers.tableau$location)

gdrplayers.tableau <- left_join(gdrplayers.tableau,
                                unique(italian.places[,c("Provincia", "Prov", "Regione", "Latitudine.Prov", "Longitudine.Prov")]),
                                by = c("location" = "Provincia")
                                ) %>% select(-post.content, -post.title)

write.csv(gdrplayers.tableau,
          file = file.path("..", "..", "data", "rpg","gdrplayers.tableau.csv"),
          row.names = FALSE)


#############################################
# RPG-level Aggregated Cube (with frequences)
#####

# Most frequent games

gdrplayers.tdm.df <- data.frame(apply(gdrplayers.tdm, 1, sum))
gdrplayers.freq <- data.frame(ST = row.names(gdrplayers.tdm.df), Freq = gdrplayers.tdm.df[, 1])
gdrplayers.freq <- gdrplayers.freq[order(gdrplayers.freq$Freq, decreasing = T), ]
row.names(gdrplayers.freq) <- NULL

# collapse vampire and some other stuff
# TODO fix at rpgsynonyms level

gdrplayers.freq$Rank <- 1:NROW(gdrplayers.freq)

# Gathering doc-term matrix
gdrplayers.dtm.df <- as.data.frame(as.matrix(gdrplayers.dtm))
gdrplayers.dtm.df$dt <- format(meta(gdrplayers.corpus, type="indexed")$date, "%Y")
gdrplayers.dtm.df$id <- meta(gdrplayers.corpus, type="indexed")$id
gdrplayers.denorm <- gather(gdrplayers.dtm.df, key = "game", value="value", -dt, -id) %>% 
  filter(value != 0) %>%
  mutate(value = 1) %>% select(-id) %>%
  dcast(game ~ dt, fun=sum)

gdrplayers.freq <- left_join(gdrplayers.freq, gdrplayers.denorm, by=c("ST"="game"))

write.csv(gdrplayers.freq,
          file = file.path("..", "..", "data", "rpg","gdrplayers.freq.csv"),
          row.names = FALSE)

write.csv(rpg.list.norm,
          file = file.path("..", "..", "data", "rpg","rpg.list.tableau.csv"),
          row.names = FALSE)