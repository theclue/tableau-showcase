###########################################
# Pandoro in Italy                        #
# --------------------------------------- #
# Analyzing in Twitter & Instagram        #
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

# pandoro <- rbindlist(lapply(
#   list.files(path = "./data/", pattern = "*.xls"), function(x, path) {
#     read_xlsx(file.path(path, x))
#   }, path = "./data"), fill = TRUE)
# 
# save(pandoro, file = "./data/pandoro.Rdata")

load(file = "./data/pandoro.Rdata")

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
pandoro.corpus <- Corpus(VectorSource(pandoro$Contents), list(language="it")) %>%
  tm_map(function(x) iconv(enc2utf8(x), sub = "byte")) %>%
  tm_map(function(x) iconv(x, to='UTF-8-MAC', sub='byte')) %>%
  tm_map(tolower) %>%
  tm_map(removeURL) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c("pandoro", stopwords("italian"))) %>%
  tm_map(removeWords, c("rt", stopwords("english"))) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)

# Add some useful metadata
meta(pandoro.corpus, tag="identifier") <- pandoro$GUID
meta(pandoro.corpus, tag="date") <- pandoro$`Date (CET)`
meta(pandoro.corpus, tag="source") <- pandoro$Source

# Document-Terms Matrix
pandoro.dtm <- DocumentTermMatrix(pandoro.corpus)

# pruning really uncommon words
fivefreq <- findFreqTerms(pandoro.dtm, 5)
pandoro.dtm <- DocumentTermMatrix(pandoro.corpus, control=list(dictionary = fivefreq, minWordLength = 3))

# remove empty posts and create a subset of the corpus
ui = unique(pandoro.dtm$i)
pandoro.valid.dtm = pandoro.dtm[ui,]
pandoro.valid.corpus <- pandoro.corpus[ui]

############################
# STEP 2
# Topic Modeling
#####
k.max <- 50

k <- ifelse(!exists("k"),k.max, ifelse(as.numeric(k)>k.max,k.max,as.numeric(k)))

#Set parameters for Gibbs sampling
gibbs.control <- list(burnin = 0,
                      iter = 2000,
                      thin = 500,
                      seed = list(2003,5,63,100001,765,287,899,101,49,3),
                      nstart = 10,
                      best = TRUE)

pandoro.ldaOut <-LDA(pandoro.valid.dtm,
                           k=k,
                           method="Gibbs",
                           control=gibbs.control)

save(pandoro.ldaOut, file = "./pandoro.ldaOut.Rdata")


# Best candidate topic column
#pandoro.main.topics <- as.matrix(topics(pandoro.ldaOut))
pandoro.main.topics <- data.frame(topic = topics(pandoro.ldaOut),
                                  GUID = meta(pandoro.valid.corpus, tag="identifier")) %>% mutate(document = as.character(row_number()))

# Top 25 tags for each topic
pandoro.topics.terms <- tidy(pandoro.ldaOut, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(25, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Top most representative document for each topic
pandoro.topics.documents <- tidy(pandoro.ldaOut, matrix = "gamma") %>%
  group_by(topic) %>%
  top_n(25, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma) %>%
  left_join(pandoro.main.topics %>% select(-topic), by = "document")

# Bind content to identifiers for visual inspections
pandoro.corpus.content <- left_join(data.frame(content = substr(content(pandoro.valid.corpus), 0, 90), GUID = meta(pandoro.valid.corpus, tag="identifier")),
                                    pandoro.topics.documents) %>% filter(!is.na(topic))


# Visual inspection
for(i in k:1){
  multiplot(
    pandoro.topics.terms %>%
      mutate(term = reorder(term, beta)) %>%
      filter(topic == i) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE, fill = "pink") +
      facet_wrap(~ topic, scales = "free") +
      coord_flip(),
    pandoro.corpus.content %>%
      mutate(document = reorder(document, gamma)) %>%
      filter(topic == i) %>%
      ggplot(aes(content, gamma, fill = factor(topic))) +
      geom_col(show.legend = FALSE, fill = "lightblue") +
      #facet_wrap(~ topic, scales = "free") +
      coord_flip()
  )
  }

pandoro.clean <- left_join(pandoro, pandoro.main.topics, by = c("GUID" = "identifier")) %>%
  select(-`Klout Score`, -Posts, -Followers, -Following, -Emotion)

write.csv2(pandoro.clean, file ="./data/pandoro.clean.csv")

topic.labels <- c("Diatriba Pandoro/Panettone",
  "Happy Sharing",
  "Farcito da me", #aggiungere farcitura
  "Cenone di Capodanno",
  "Abbuffate Natalizie",
  "Primo Pandoro della Stagione",
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
  "Pandoro \"senza\"",
  "News Locali",
  "Piaceri dell'Inverno",
  "Senso di Colpa",
  "Blog di Ricette",
  "Team Pandoro",
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
  "Ironia Pandoro/Panettone",
  "Foodporn"
  
)

# Graph

pandoro.post <- posterior(pandoro.ldaOut)

pandoro.cor_mat <- cor(t(pandoro.post[["terms"]]))
pandoro.cor_mat[pandoro.cor_mat < .001 ] <- 0
diag(pandoro.cor_mat) <- 0

pandoro.graph <- graph.adjacency(pandoro.cor_mat, weighted=TRUE, mode="lower")
pandoro.graph <- delete.edges(pandoro.graph, E(pandoro.graph)[ weight < 0.15])

E(pandoro.graph)$edge.width <- E(pandoro.graph)$weight * 5
V(pandoro.graph)$label <- as.character(factor(V(pandoro.graph),
                                          labels=topic.labels))

V(pandoro.graph)$size <- colSums(pandoro.post[["topics"]])/200
V(pandoro.graph)$documents <- colSums(pandoro.post[["topics"]])/1

par(mar=c(0, 0, 3, 0))
set.seed(110)
plot.igraph(pandoro.graph, edge.width = E(pandoro.graph)$edge.width, 
            main = "Strength Between Topics Based On Word Probabilities",
            edge.color = "orange",
            vertex.color = "orange", 
            vertex.frame.color = NA,
            vertex.label.color = "grey10",
            vertex.label.cex=.8)

write_graph(pandoro.graph, file = "./data/pandoro.graphml", format = c("graphml"))

###################
# TOPICS OPT SIMULATION
###
k.control <- list(burnin = 500, iter = 500, seed = 77)
k.sim <- optimal_k(pandoro.valid.dtm, 30, control = k.control)

k.result <- FindTopicsNumber(
  pandoro.valid.dtm,
  topics = seq(from = 10, to = 450, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = k.control,
  mc.cores = parallel::detectCores(),
  verbose = TRUE
)
save(k.result, file = "./data/pandoro.k.result.Rdata")

FindTopicsNumber_plot(k.result)
