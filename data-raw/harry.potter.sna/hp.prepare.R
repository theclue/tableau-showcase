######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "stringr", "stringi", "googleLanguageR", "wordnet", "futile.logger", "Hmisc", "igraph", "sna", "poweRlaw", "intergraph")
  p_load_gh("quanteda/spacyr")
}, warning=function(w){
  stop(conditionMessage(w))
})


########
# Setup
#
sapply(file.path("..", "..", "R", c("multiplot.R",
                                    "splitAt.R",
                                    "merge.dialogues.R",
                                    "find.explicit.speakers.R")),
       source,
       .GlobalEnv)

cloud.credentials <- file.path("..", "..", "credentials", Sys.getenv("GCLOUD_DEV_CREDENTIALS"))

gl_auth(cloud.credentials)

#spacy_initialize(model = "en_core_web_lg")

# wordnet
setDict(Sys.getenv(file.path("WNHOME"), "dict"))

#################################
# Data Loading and text cleanup
# -
# 
##
harrypotter.one <- readLines("../../data/harry-potter/harrypotter.01.txt")
harrypotter.aliases <- read.csv2("../../data/harry-potter/harrypotter.aliases.csv", stringsAsFactors = FALSE)
harrypotter.houses <- read.csv2("../../data/harry-potter/harrypotter.houses.csv", stringsAsFactors = FALSE)

# TODO: clean this chapter-delimiters regex
harrypotter.one[which(grepl("\\*", harrypotter.one, ignore.case = TRUE, perl = TRUE))] <- NA


hp.one.sentences.nlp <- gl_nlp(harrypotter.one, language = "en", nlp_type = "analyzeSentiment")


hp.one.sentences <- do.call(c, lapply(hp.one.sentences.nlp[["sentences"]], function(x){
  if(is.null(x)) return(NA)
  return(x$content)
}))

# merge dialogue lines together
hp.one.sentences.delim <- merge.dialogues(hp.one.sentences)

################################
# DIALOGUE METRICS DISTRIBUTION
# -
# Calculation of the distribution of dialogue lengths (consecutive lines of dialogue)
# and dialogue spacings (consecutive lines of text without dialogues)
##

# I'm going to split by chapter to calculate dialogue metrics with more precision
hp.one.chapters <- lapply(splitAt(hp.one.sentences.delim,
                                  which(is.na(hp.one.sentences.delim))
), function(chapter){
  na.omit(chapter)
})

hp.one.dialogue.len <- do.call(rbind, lapply(hp.one.chapters, function(chapter){
  chapter.dialogue.lines <- ifelse(str_count(chapter, "\"") > 0, TRUE, FALSE)
  chapter.dialogue.rle <- rle(chapter.dialogue.lines)
  
  dialogue.len <- as.data.frame(table(chapter.dialogue.rle), stringsAsFactors = FALSE) %>% 
    mutate(lengths = as.integer(lengths)) %>%
    filter(values == "TRUE" & Freq > 0) %>%
    select(-values)
})) %>%
  group_by(`lengths`) %>%
  summarize_all(sum) %>%
  arrange(lengths)

hp.one.dialogue.spacing <- do.call(rbind, lapply(hp.one.chapters, function(chapter){
  chapter.dialogue.lines <- ifelse(str_count(chapter, "\"") > 0, TRUE, FALSE)
  chapter.dialogue.rle <- rle(chapter.dialogue.lines)
  
  dialogue.len <- as.data.frame(table(chapter.dialogue.rle), stringsAsFactors = FALSE) %>% 
    mutate(lengths = as.integer(lengths)) %>%
    filter(values == "FALSE" & Freq > 0) %>%
    select(-values)
})) %>%
  group_by(`lengths`) %>%
  summarize_all(sum)

#####################################
# CONVERSATION THRESHOLD CALCULATION
# -
# This allow to detect when a conversation must start and end
# with (ideally) an automatic process. If two dialogues are
# separated by a number of lines which is greater than the
# computed threshold number, then they own to different conversations.
#
# This empirical rule is got from {Waumass, Nicodeme, Bersini}:
#
# "The threshold is the highest possible value of spacing, such that its frequency
# is higher than both 10 and double the frequency of the spacing one unit above, and such
# that all lower values of spacing have higher frequencies. However, if there is a higher value
# of spacing with a frequency above 100, this value is used as a threshold instead.
##
threshold <- (function(d.spacing){
  
  candidate.thresholds <- d.spacing[d.spacing$Freq > 10 & 2*d.spacing$Freq > lead(d.spacing$Freq,1),]
  candidate.thresholds <- candidate.thresholds[1:min(which(diff(candidate.thresholds$lengths)>1)),]
  
  threshold.span <- 0
  repeat{
    best.candidate <- candidate.thresholds[NROW(candidate.thresholds) - threshold.span,]$Freq
    if(do.call(all,lapply(candidate.thresholds[1:(NROW(candidate.thresholds)- threshold.span - 1),]$Freq, function(t){
      return(t > best.candidate)
    }))){
      
      lo.freq.candidate <- candidate.thresholds[NROW(candidate.thresholds) - threshold.span,]$lengths
      hi.freq.candidates <- d.spacing %>% filter(Freq > 100)
      if(nrow(hi.freq.candidates > 0) & max(hi.freq.candidates$lengths) > lo.freq.candidate) return(max(hi.freq.candidates$lengths))
      return(lo.freq.candidate)
    } else { 
      threshold.span <- threshold.span
    }
  }
  
})(hp.one.dialogue.spacing)

flog.info(sprintf("Dialogue separated by more than %d lines of narrative will be treated as separate conversations", threshold))

#####################################
# CONVERSATIONS ENUMERATION
# -
# Here I'm going to set the boundaries of the conversation, grouping the dialogue lines that
# fit into the interval delimited by the threshold value.
# I'm also adding all the narrative from the end to the previous conversation to the beginning
# of the current one, as it provides meaningful context when trying to identify the speakers.
# Then, the narrative following the dialogues is added to the context as well, if they are not
# assigned to their own conversation yet.
# Finally, I split by conversation, which is more useful than chapters in NLP (more on later)
##

hp.one.conversations <- do.call(rbind, lapply(seq_along(hp.one.chapters), function(i, chapters){
  
  chapter <- chapters[[i]]
  
  quote <- grepl("\"", chapter)
  
  chapter.conversations <- data.frame(quote = quote, conversation=NA_integer_)
  
  # expand the conversation boundaries
  chapter.conversations$conversation[quote] <- cumsum(c(1, diff(which(quote)) > threshold))
  
  # add the narrative before
  chapter.conversations <- fill(chapter.conversations, conversation, .direction = "up")
  chapter.conversations <- fill(chapter.conversations, conversation, .direction = "down")
  chapter.conversations$conversation <- ifelse(!is.na(chapter.conversations$conversation), paste(stri_pad_left(i,3,0), stri_pad_left(chapter.conversations$conversation, 2, 0), sep = "."), NA)
  chapter.conversations$content <- chapter
  
  return(chapter.conversations[,c("content", "conversation")])
  
}, chapters = hp.one.chapters)) %>%
  filter(!is.na(conversation))

hp.one.conversations <- split(hp.one.conversations, hp.one.conversations$conversation)

#####################################
# GOOGLE NATURAL LANGUAGE PROCESSING
# -
# 
##

hp.one.pos <- lapply(hp.one.conversations, function(c){
  
  gl_nlp(c$content, language = "en")
  
})

#####################################
# SPEAKER IDENTIFICATION
# -
# Here I'm going try to identify, for each conversation, who are actually speaking.
# This is useful since someone could refer to a character inside a dialogue, but
# this doesn't mean that the character is physically present. To do that, I use
# relations and lemmas as got from part-of-speech tagging, wordnet
# to expand a set of seed words which could be related to a dialogue (ex. say, state, answer...)
# and a couple of features which would able to better detect those.
#
# These are several situations that could to identify who is involved in a conversation:
#
# 1- Explicit Speaker: '"I suppose so," said Mrs Dursley stiffly.'
# 2- Use of vocative in dialogues: '"Fancy seeing you here, Professor McGonagall."'
# 3- Implicit Speaker
# 4- Anaphoric Speaker
##
dialogue.verbs.seed <- c("say", "speak", "talk", "ask", "reply", "answer",
                         "add", "continue", "go on", "cry", "sigh",
                         "murmur", "comment", "nod", "whisper",
                         "assent", "confirm", "sob", "falter", "suggest", "screech", "scream", "shriek")

implicit.dialogue.verbs.seed <- c("say", "speak", "talk", "ask", "reply", "answer",
                                  "cry", "sigh", "murmur", "comment", "nod",
                                  "assent", "sob", "suggest", "screech", "scream", "shriek")


# I really don't need wordnet for this task, but it's so funny to use ;)
if(initDict()) {
  verbs.dictionary <- left_join(
    (data.frame(verb = unique(do.call(c, lapply(dialogue.verbs.seed, function(v){
      getSynonyms(getIndexTerms("VERB", 1, getTermFilter("ExactMatchFilter", v, TRUE))[[1]])
    }))), stringsAsFactors = FALSE) %>%
      mutate("direct.dialogue" = TRUE)),
    (data.frame(verb = unique(do.call(c, lapply(implicit.dialogue.verbs.seed, function(v){
      getSynonyms(getIndexTerms("VERB", 1, getTermFilter("ExactMatchFilter", v, TRUE))[[1]])
    }))), stringsAsFactors = FALSE) %>%
      mutate("implicit.dialogue" = TRUE))
  ) %>%
    mutate("implicit.dialogue" = ifelse(is.na(implicit.dialogue), FALSE, implicit.dialogue))
  
  verbs.dictionary <- verbs.dictionary[(verbs.dictionary$verb %nin% c("do")),]
  
}

############################################
# Speaker Detection pt.1: Explicit Speakers
# -
# To detect explicit speakers, part-of-speech tags are used.
# Specifically, I'm going to extract all the proper names who act as nsubj for one of those
# dialogue verbs found above.
# 
# During the process, a single speaker per line is assumed at most, which is almost safe
# when analyzing western literature (or even a translation to western language).
#

#explicit.speakers <- find.explicit.speakers(hp.one.pos['054.04'])
explicit.speakers <- find.explicit.speakers(hp.one.pos)

speakers.df <- do.call(plyr::rbind.fill, explicit.speakers) %>% filter(!is.na(conversation)) %>% inner_join(harrypotter.aliases) %>% select("conversation", "name", "dialogues")
speakers.df <- unique(speakers.df)

# Finally, push orphans to previous conversation

conversation.sumup <- speakers.df %>% select(conversation) %>% mutate(n=1) %>% group_by(conversation) %>% summarise(n = sum(n))
conversation.sumup$true.conversation <- ifelse(conversation.sumup$n==1, NA, conversation.sumup$conversation)
conversation.sumup <- fill(conversation.sumup, true.conversation, .direction = "down")

speakers.df <- speakers.df %>% left_join(conversation.sumup, by="conversation") %>%
  select("conversation", "true.conversation", "name", "dialogues")

speakers.df$true.conversation <- ifelse(is.na(speakers.df$true.conversation), speakers.df$conversation, speakers.df$true.conversation)

speakers.df <- unique(speakers.df %>% select(-conversation))

######################
# Garbage collection
##
rm(hp.one.chapters,
   hp.one.sentences,
   hp.one.sentences.delim,
   hp.one.sentences.nlp,
   conversation.sumup, 
   explicit.speakers,
   implicit.dialogue.verbs.seed,
   dialogue.verbs.seed,
   verbs.dictionary)

#######################
# NETWORK GENERATION  #
#######################

# Static Network
edge.lists <- do.call(plyr::rbind.fill, lapply(split(speakers.df, as.factor(speakers.df$true.conversation)), function(t){
  
  if(nrow(t) <= 1) return(NULL)
  
  t1 <- (expand.grid(t$name, t$name) %>% filter(Var1 != Var2))
  delRows = NULL # the rows to be removed
  for(i in 1:nrow(t1)){
    j = which(t1$Var1 == t1$Var2[i] & t1$Var2 == t1$Var1[i])
    j = j [j > i]
    if (length(j) > 0){
      delRows = c(delRows, j)
    }
  }
  t1 = t1[-delRows,]
  colnames(t1) <- c("from", "to")
  t1$true.conversation <- unique(t$true.conversation)
  return(t1)
  
})) %>% group_by(from, to) %>% summarise(weight = n())


hp.graph <- graph_from_data_frame(edge.lists, directed = FALSE, vertices = harrypotter.houses[harrypotter.houses$name %in% speakers.df$name,])
hp.graph <- igraph::simplify(hp.graph)

V(hp.graph)$label <- V(hp.graph)$name

#################
# OUTPUT FILES  #
#################
write.graph(hp.graph, file = "../../data/harry-potter/harrypotter.01.graphml", format=c("graphml"))
write.csv(speakers.df, file = "../../data/harry-potter/harrypotter.01.speakers.csv", row.names = FALSE)

#######################
# GARBAGE COLLECTION  #
#######################


#spacy_finalize()
