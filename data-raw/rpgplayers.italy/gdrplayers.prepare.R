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
               "data.table")

## Source useful functions on gist
invisible(lapply(c("https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions/optimal_k.R",
                   "https://gist.githubusercontent.com/theclue/a4741899431b06941c1f529d6aac4387/raw/f69d9b5a420e2c4707acad69f31c6e6a3c15e559/ggplot-multiplot.R",
                   "https://gist.githubusercontent.com/mrdwab/11380733/raw/83c8eeb156a49c653ed31c7cda67b36281173ee6/cSplit.R") ,devtools::source_url))


############################
# Initial Set-up
#####

.jinit(parameters="-Xmx128g")

# TODO it's needed on Linux, but it gets Windows processing slowly
options(mc.cores=1)


###############################
# Load Data
# ---------
#####

gdr.players <- load(file.path("..", "..", "data", "rpg", "gdrplayers.rda"))

rpg.list <- rbind(
  read.csv(file = file.path("..", "..", "data", "rpg","rpg.list.italians.csv"), sep="|", header=T, quote="", stringsAsFactors=FALSE), 
  read.csv(file = file.path("..", "..", "data", "rpg", "rpg.list.csv"), sep="|", header=T, quote="", stringsAsFactors=FALSE)
)

rpg.list.italians.attributes <- rbind(
  rpg.loadData("rpg.list.attributes.italians.csv.zip", overwrite=FALSE, sep=",", header=T, quote="\"", stringsAsFactors=FALSE),
  rpg.loadData("rpg.list.attributes.csv.zip", overwrite=FALSE, sep=",", header=T, quote="\"", stringsAsFactors=FALSE)
)

italian.places <- rpg.loadData(file="italian.places.csv.zip", overwrite=FALSE, sep=";", header=T, quote="", stringsAsFactors=FALSE)

colnames(rpg.list.italians.attributes)[1] <- "Link"
rpg.list.italians.attributes <- merge(
  x=rpg.list.italians[,c("Link", "TitoloNorm")], 
  y=rpg.list.italians.attributes[,!names(rpg.list.italians.attributes) %in% "Titolo"], 
  by="Link", all.x=TRUE)[,!names(rpg.list.italians.attributes) %in% "Link"]

