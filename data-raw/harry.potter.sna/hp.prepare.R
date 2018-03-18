######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "stringr")
  p_load_gh("quanteda/spacyr")
}, warning=function(w){
  stop(conditionMessage(w))
})

harrypotter.one <- readLines("../../data/harry-potter/harrypotter.01.txt")

# detect uneven double quotes
which((str_count(harrypotter.one, "\"") %% 2) != 0)

################

harrypotter.dialog.lines <- str_count(harrypotter.one, "\"")


#######
# NLP
##
spacy_initialize(model = "en_core_web_lg")

txt <- c(d1 = "spaCy excels at large-scale information extraction tasks.",
         d2 = "Mr. Smith goes to North Carolina.")
spacy_parse(txt)

doc <- spacy_parse(harrypotter.one, dependency = TRUE)

spacy_parse(harrypotter.one[1:4], dependency = TRUE, lemma = FALSE, pos = FALSE)


entity_consolidate(txt)

spacy_finalize()
