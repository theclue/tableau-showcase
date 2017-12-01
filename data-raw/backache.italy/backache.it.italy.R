###########################################
# Backache in Italy                       #
# --------------------------------------- #
# Analyzing forums and blogs              #
###########################################

#################
# Requirements  #
#################
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse",
               "tm",
               "RWeka",
               "topicmodels",
               "ldatuning",
               "SnowballC",
               "tidytext",
               "scales",
               "igraph",
               "RTextTools",
               "readxl",
               "data.table")

## Source useful functions on gist
invisible(lapply(c("https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions/optimal_k.R",
                   "https://gist.githubusercontent.com/theclue/a4741899431b06941c1f529d6aac4387/raw/f69d9b5a420e2c4707acad69f31c6e6a3c15e559/ggplot-multiplot.R") ,devtools::source_url))

###############
# STEP 0 
# Data Loading
####

# backache <- rbindlist(lapply(
#   list.files(path = "./data/", pattern = "*.xls"), function(x, path) {
#     read_xlsx(file.path(path, x))
#   }, path = "./data"), fill = TRUE)

# save(backache, file = "./data/backache.Rdata")

load(file = "./data/backache.Rdata")

#################################################
# POSTS MODELING                                #
# --------------------------------------------- #
# Topic Modeling with LDA/Gibbs sampling        #
#################################################

########
# STEP 1
# The corpus is created and cleaned, while
# relevant metadata are added to it
#####

removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))

# Load the corpus
backache.corpus <- Corpus(VectorSource(backache$Contents), list(language="it")) %>%
  tm_map(function(x) iconv(enc2utf8(x), sub = "byte")) %>%
  tm_map(function(x) iconv(x, to='UTF-8', sub='byte')) %>%
  tm_map(tolower) %>%
  tm_map(removeURL) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c("mal", "schiena", stopwords("italian"))) %>%
  tm_map(removeWords, c("rt", stopwords("english"))) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)

# Add some useful metadata
meta(backache.corpus, tag="identifier") <- backache$GUID
meta(backache.corpus, tag="date") <- backache$`Date (CET)`
meta(backache.corpus, tag="source") <- backache$Source

# Document-Terms Matrix
backache.dtm <- DocumentTermMatrix(backache.corpus)

# pruning really uncommon words
fivefreq <- findFreqTerms(backache.dtm, 5)
backache.dtm <- DocumentTermMatrix(backache.corpus, control=list(dictionary = fivefreq, minWordLength = 3))

# remove empty posts and create a subset of the corpus
ui = unique(backache.dtm$i)
backache.valid.dtm = backache.dtm[ui,]
backache.valid.corpus <- backache.corpus[ui]

############################
# STEP 2
# Topic Modeling
#####
k.max <- 50

backache.posts.valid <- backache.valid.dtm[which(rowSums(as.matrix(backache.valid.dtm) != 0)>=1),]

k.control <- list(burnin = 500, iter = 1000, keep = 100)
k <- optimal_k(backache.posts.valid, 50, control = k.control)

k <- ifelse(!exists("k"),k.max, ifelse(as.numeric(k)>k.max,k.max,as.numeric(k)))

#Set parameters for Gibbs sampling
gibbs.control <- list(burnin = 0,
                      iter = 2000,
                      thin = 500,
                      seed = list(2003,5,63,100001,765,287,899,101,49,3),
                      nstart = 10,
                      best = TRUE)

backache.ldaOut <-LDA(backache.valid.dtm,
                           k=k,
                           method="Gibbs",
                           control=gibbs.control)

save(backache.ldaOut, file = "./backache.ldaOut.Rdata")


# Best candidate topic column
#backache.main.topics <- as.matrix(topics(backache.ldaOut))
backache.main.topics <- data.frame(topic = topics(backache.ldaOut),
                                  GUID = meta(backache.valid.corpus, tag="identifier")) %>% mutate(document = as.character(row_number()))

# Top 25 tags for each topic
backache.topics.terms <- tidy(backache.ldaOut, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(25, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Top most representative document for each topic
backache.topics.documents <- tidy(backache.ldaOut, matrix = "gamma") %>%
  group_by(topic) %>%
  top_n(25, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma) %>%
  left_join(backache.main.topics %>% select(-topic), by = "document")

# Bind content to identifiers for visual inspections
backache.corpus.content <- left_join(data.frame(content = substr(content(backache.valid.corpus), 0, 90), GUID = meta(backache.valid.corpus, tag="identifier")),
                                    backache.topics.documents) %>% filter(!is.na(topic))


# Visual inspection
for(i in k:1){
  multiplot(
    backache.topics.terms %>%
      mutate(term = reorder(term, beta)) %>%
      filter(topic == i) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE, fill = "pink") +
      facet_wrap(~ topic, scales = "free") +
      coord_flip(),
    backache.corpus.content %>%
      mutate(document = reorder(document, gamma)) %>%
      filter(topic == i) %>%
      ggplot(aes(content, gamma, fill = factor(topic))) +
      geom_col(show.legend = FALSE, fill = "lightblue") +
      #facet_wrap(~ topic, scales = "free") +
      coord_flip()
  )
  }

backache.clean <- left_join(backache, backache.main.topics, by = c("GUID" = "identifier")) %>%
  select(-`Klout Score`, -Posts, -Followers, -Following, -Emotion)

write.csv2(backache.clean, file ="./data/backache.clean.csv")

topic.labels <- c("Diatriba backache/Panettone",
  "Happy Sharing",
  "Farcito da me", #aggiungere farcitura
  "Cenone di Capodanno",
  "Abbuffate Natalizie",
  "Primo backache della Stagione",
  "Valerio Scanu",
  "Meglio il Panettone",
  "Ricette",
  "Press/Promozionali",
  "Sarcamo (Politica)",
  "Lievitati Tradizionali", #TBS
  "Pubblicità",
  "Valerio Scanu (Haters)",
  "Natale in Arrivo",
  "Voglia di una fetta",
  "(Ritornare) Bambini",
  "Tante calorie",
  "Solidarietà",
  "Opinioni Personali e Ironia",
  "Aria di Natale",
  "Menù di Natale",
  "Irresistibile Voglia",
  "Diverse Usanze Italiane",
  "Polemiche Varie",
  "Casa",
  "backache \"senza\"",
  "News Locali",
  "Piaceri dell'Inverno",
  "Senso di Colpa",
  "Blog di Ricette",
  "Team backache",
  "Convivialità",
  "Non vedo l'ora di...",
  "Colazione",
  "Prodotti Italiani",
  "Melegatti, Bauli & Brand",
  "Media",
  "Concorsi",
  "Calcio",
  "Parenti",
  "Team Panettone",
  "Attendere il Natale",
  "Magia del Natale",
  "Social Haters",
  "Ricette Alternative",
  "Zucchero a Velo",
  "Dealers",
  "Ironia backache/Panettone",
  "Foodporn"
  
)

# Graph

backache.post <- posterior(backache.ldaOut)

backache.cor_mat <- cor(t(backache.post[["terms"]]))
backache.cor_mat[backache.cor_mat < .001 ] <- 0
diag(backache.cor_mat) <- 0

backache.graph <- graph.adjacency(backache.cor_mat, weighted=TRUE, mode="lower")
backache.graph <- delete.edges(backache.graph, E(backache.graph)[ weight < 0.15])

E(backache.graph)$edge.width <- E(backache.graph)$weight * 5
V(backache.graph)$label <- as.character(factor(V(backache.graph),
                                          labels=topic.labels))

V(backache.graph)$size <- colSums(backache.post[["topics"]])/200
V(backache.graph)$documents <- colSums(backache.post[["topics"]])/1

par(mar=c(0, 0, 3, 0))
set.seed(110)
plot.igraph(backache.graph, edge.width = E(backache.graph)$edge.width, 
            main = "Strength Between Topics Based On Word Probabilities",
            edge.color = "orange",
            vertex.color = "orange", 
            vertex.frame.color = NA,
            vertex.label.color = "grey10",
            vertex.label.cex=.8)

write_graph(backache.graph, file = "./data/backache.graphml", format = c("graphml"))

###################
# TOPICS OPT SIMULATION
###
k.control <- list(burnin = 500, iter = 500, seed = 77)
k.sim <- optimal_k(backache.valid.dtm, 30, control = k.control)

k.result <- FindTopicsNumber(
  backache.valid.dtm,
  topics = seq(from = 10, to = 150, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = k.control,
  mc.cores = parallel::detectCores(),
  verbose = TRUE
)
save(k.result, file = "./data/backache.k.result.Rdata")

FindTopicsNumber_plot(k.result)
