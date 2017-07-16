###########################################
# enricomentanaLa7                        #
# --------------------------------------- #
# Analyzing social haters in Italy        #
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
               "e1071",
               "caret",
               "doMC",
               "facebook.S4")

## Source useful functions on gist
invisible(lapply(c("https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions/optimal_k.R",
                   "https://gist.githubusercontent.com/theclue/a4741899431b06941c1f529d6aac4387/raw/f69d9b5a420e2c4707acad69f31c6e6a3c15e559/ggplot-multiplot.R") ,devtools::source_url))

app.key <- Sys.getenv("FACEBOOK_CONNECTOR_APP", "")
app.secret <- Sys.getenv("FACEBOOK_CONNECTOR_SECRET", "")

fb_token <- fbOAuth(app.key, app.secret)

###############
# STEP 0 
# Data Loading
####

mentana.posts <- FacebookPagesCollection("enricomentanaLa7", fb_token) %>% FacebookPostsCollection(fields = c("id",
                                                                                                              "from.fields(id,name)",
                                                                                                              "message",
                                                                                                              "created_time",
                                                                                                              "type",
                                                                                                              "link",
                                                                                                              "name",
                                                                                                              "icon"),
                                                                                                   n = Inf)


mentana.comments.1 <- mentana.posts[1:100]   %>% FacebookCommentsCollection(n = Inf, .progress = plyr::progress_text())
mentana.comments.2 <- mentana.posts[101:200] %>% FacebookCommentsCollection(n = Inf, .progress = plyr::progress_text())
mentana.comments.3 <- mentana.posts[201:250] %>% FacebookCommentsCollection(n = Inf, .progress = plyr::progress_text())
mentana.comments.4 <- mentana.posts[251:350] %>% FacebookCommentsCollection(n = Inf, .progress = plyr::progress_text())
mentana.comments.5 <- mentana.posts[351:450] %>% FacebookCommentsCollection(n = Inf, .progress = plyr::progress_text())

mentana.comments.1.replies <- mentana.comments.1 %>% FacebookCommentsCollection(n = Inf, .progress = plyr::progress_text())
mentana.comments.2.replies <- mentana.comments.2 %>% FacebookCommentsCollection(n = Inf, .progress = plyr::progress_text())
mentana.comments.3.replies <- mentana.comments.3 %>% FacebookCommentsCollection(n = Inf, .progress = plyr::progress_text())

mentana.comments <- c(mentana.comments.1, mentana.comments.2, mentana.comments.3, mentana.comments.4, mentana.comments.5)
metana.comments.focus <- c(mentana.comments.1, mentana.comments.2, mentana.comments.3)
metana.comments.focus.replies <- c(mentana.comments.1.replies, mentana.comments.2.replies, mentana.comments.3.replies)

mentana.posts.df <- as.data.frame(mentana.posts)

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

# Load the corpus
mentana.posts.corpus <- Corpus(VectorSource(mentana.posts.df$message), list(language="it"))
mentana.posts.corpus <- tm_map(mentana.posts.corpus, tolower)
mentana.posts.corpus <- tm_map(mentana.posts.corpus, removeWords, stopwords("italian"))
mentana.posts.corpus <- tm_map(mentana.posts.corpus, stripWhitespace)
mentana.posts.corpus <- tm_map(mentana.posts.corpus, removePunctuation)
mentana.posts.corpus <- tm_map(mentana.posts.corpus, removeNumbers)
#mentana.posts.corpus <- tm_map(mentana.posts.corpus, stemDocument, language="italian")

# Metadata
DublinCore(mentana.posts.corpus, tag="identifier") <- mentana.posts.df$id
DublinCore(mentana.posts.corpus, tag="date") <- mentana.posts.df$created_time
DublinCore(mentana.posts.corpus, tag="source") <- mentana.posts.df$link
meta(mentana.posts.corpus, tag="likes") <- mentana.posts.corpus$likes.summary.total_count
meta(mentana.posts.corpus, tag="comments") <- mentana.posts.corpus$comments.summary.total_count
meta(mentana.posts.corpus, tag="type") <- mentana.posts.corpus$type

############################
# STEP 2
# Adjacence Matrices
# Matrix is built against 3-grams stemmed italian dictionary
#####

# term-document matrix
mentana.posts.tdm <- TermDocumentMatrix(mentana.posts.corpus)

# document-term matrix
mentana.posts.dtm <- DocumentTermMatrix(mentana.posts.corpus)

# adjacence matrix
mentana.posts.tdm.matrix <- as.matrix(mentana.posts.tdm)
colnames(mentana.posts.tdm.matrix) <- meta(mentana.posts.corpus, type="indexed")$id

mentana.posts.adj.matrix <- mentana.posts.tdm.matrix
mentana.posts.adj.matrix[mentana.posts.tdm.matrix>=1] <- 1
mentana.posts.adj.matrix <- mentana.posts.adj.matrix %*% t(mentana.posts.adj.matrix)

############################
# STEP 3
# Topic Modeling
#####
k.control <- list(burnin = 500, iter = 1000, keep = 100)

mentana.posts.valid <- mentana.posts.dtm[which(rowSums(as.matrix(mentana.posts.dtm) != 0)>=1),]

k <- optimal_k(mentana.posts.valid, 50, control = k.control)
k <- ifelse(!exists("k"),15, ifelse(k>15,15,k))

#Set parameters for Gibbs sampling
gibbs.control <- list(burnin = 2000,
                      iter = 200,
                      thin = 200,
                      seed = list(2003,5,63,100001,765,287,899,101,49,3),
                      nstart = 10,
                      best = TRUE)

#Run LDA using Gibbs sampling
mentana.posts.ldaOut <-LDA(mentana.posts.valid,
                 k=as.numeric(k),
                 method="Gibbs",
                 control=gibbs.control)

# Best candidate topic column
mentana.posts.ldaOut.main.topics <- as.matrix(topics(mentana.posts.ldaOut))

# Top 25 tags for each topic
mentana.posts.ldaOut.top.terms <- tidy(mentana.posts.ldaOut, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(25, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Top most representative document for each topic
mentana.posts.ldaOut.top.documents <- tidy(mentana.posts.ldaOut, matrix = "gamma") %>%
  group_by(topic) %>%
  top_n(25, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

# Visual inspection
for(i in 1:k){
  multiplot(mentana.posts.ldaOut.top.terms %>%
    mutate(term = reorder(term, beta)) %>%
    filter(topic == i) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE, fill = "pink") +
    facet_wrap(~ topic, scales = "free") +
    coord_flip())}
  
# After manual inspection...
topic.labels <- c("Personal Opinions", #
                  "Movimento 5 Stelle", #
                  "N/A", #
                  "Press", #
                  "Islamic Terrorism", #
                  "Haters & Social Networks", #
                  "Geopolitics", #
                  "Italian Politics",
                  "Earthquake of Amatrice",
                  "Freedom & Civil Rights",
                  "Mentana's Marathons (Elections Day)", #
                  "Law & Justice",
                  "Referendum in Italy",
                  "Labor", #
                  "Matteo Renzi") #

mentana.posts.inspect <- mentana.posts.df[which(rowSums(as.matrix(mentana.posts.dtm) != 0)>=1),]$id

mentana.posts.topics <- data.frame(id = mentana.posts.inspect, topic = mentana.posts.ldaOut.main.topics)

mentana.posts.topics <- (left_join(mentana.posts.df, mentana.posts.topics, by = 'id')) %>%
                          mutate(topic = ifelse(!is.na(topic), topic, 3))

mentana.posts.topics$topic <- factor(mentana.posts.topics$topic, labels=topic.labels)

View(mentana.posts.topics %>% filter(topic=="Labor") %>% select(message))


# Plot the distribution of topics
mentana.posts.topics %>%
  group_by(topic) %>%
  dplyr::summarise(count = n()) %>%
  filter(topic != "N/A") %>%
  mutate(percent.total = count / sum(count)) %>%
  arrange(-count) %>% 
  ggplot(aes(reorder(topic, -percent.total), percent.total)) +
  geom_bar(stat="identity", fill="darkgreen", alpha=.4) +
  geom_text(aes(reorder(topic, -percent.total), label=percent(percent.total), percent.total), angle=90, size=3, hjust=-.1) +
  ylab("Percent of Total Posts") + xlab("Topics") + scale_y_continuous(labels=percent, expand = c(.02, .01)) +
  ggtitle(paste("Distribution of Topics in Enrico Mentana's Posts")) +
  theme(axis.text.x = element_text(angle = 90, size=9), axis.text.y = element_text(size=9))

# Plot relations
mentana.posts.post <- posterior(mentana.posts.ldaOut)

mentana.posts.cor_mat <- cor(t(mentana.posts.post[["terms"]]))
mentana.posts.cor_mat[mentana.posts.cor_mat < .001 ] <- 0
diag(mentana.posts.cor_mat) <- 0

mentana.posts.graph <- graph.adjacency(mentana.posts.cor_mat, weighted=TRUE, mode="lower")
mentana.posts.graph <- delete.edges(mentana.posts.graph, E(mentana.posts.graph)[ weight < 0.14])

E(mentana.posts.graph)$edge.width <- E(mentana.posts.graph)$weight * 8
V(mentana.posts.graph)$label <- as.character(factor(V(mentana.posts.graph),
                                          labels=topic.labels))

V(mentana.posts.graph)$size <- colSums(mentana.posts.post[["topics"]])/15

par(mar=c(0, 0, 3, 0))
set.seed(110)
plot.igraph(mentana.posts.graph, edge.width = E(mentana.posts.graph)$edge.width, 
            main = "Strength Between Topics Based On Word Probabilities",
            edge.color = "orange",
            vertex.color = "orange", 
            vertex.frame.color = NA,
            vertex.label.color = "grey10",
            vertex.label.cex=.8)

############################
# STEP 4
# Blast Detection :)
#####

comments <- as.data.frame(mentana.comments.focus)
comments$parent.post <- mentana.comments.focus@parent

comments.replies <- as.data.frame(mentana.comments.focus.replies)
comments.replies$parent.comment <- mentana.comments.focus.replies@parent

blasts <- comments.replies %>%
  filter(from.name == "Enrico Mentana") %>%
  select(message, created_time, likes.summary.total_count, parent.comment) %>%
  rename(id = parent.comment) %>%
  mutate(blast = factor("BLAST!"))

blast.candidates <- left_join(comments,
                              blasts,
                              by = "id") %>%
  select(-comments.summary.order, -comments.summary.can_comment, -likes.summary.can_like, -likes.summary.has_liked) %>%
  left_join(mentana.posts.topics %>% select(id, topic), by = c("parent.post" = "id"))

# Load the corpus
blast.candidates.corpus <- Corpus(VectorSource(blast.candidates$message.x), list(language="it")) %>%
  tm_map(function(x) iconv(enc2utf8(x), sub = "byte")) %>%
  tm_map(function(x) iconv(x, to='UTF-8-MAC', sub='byte')) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords("italian")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stemDocument, language="italian")

# documents-terms matrix
blasts.dtm <- DocumentTermMatrix(blast.candidates.corpus)

# outcome
blasts.id <- which()

# pruning really uncommon words
fivefreq <- findFreqTerms(blasts.dtm, 5)
blasts.dtm <- DocumentTermMatrix(blast.candidates.corpus, control=list(dictionary = fivefreq))

# partitioning the data
predict.id <- which(blast.candidates$blast == "BLAST!")
model.id <- which(is.na(blast.candidates$blast))

blast.threshold <- as.integer(length(predict.id)*25/100)
#blast.candidates.predict <- blast.candidates.predict[sample(nrow(blast.candidates.predict)),]

test.id <- predict.id[1:blast.threshold]
train.id <- predict.id[(blast.threshold+1):length(predict.id)]

blasts.dtm.train <- blasts.dtm[train.id,]
blasts.dtm.test <- blasts.dtm[test.id,]
blasts.dtm.model <- blasts.dtm[model.id,]

# Train the classifier
system.time(classifier <- naiveBayes(blasts.dtm.train, blast.candidates.train$blast, laplace = 1))

# Use the NB classifier we built to make predictions on the test set.
system.time(pred <- predict(classifier, newdata = blasts.test.NB))

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = df.test$class )