resolveRpgSynonyms <- function(x) UseMethod("resolveRpgSynonyms", x)

resolveRpgSynonyms.PlainTextDocument <- resolveRpgSynonyms.character <- function(games){
  
  i.c <- TRUE
  
  do.call(c, pblapply(games, function(x){
  
  # separatori comuni
  x <- gsub("/", " ", x, ignore.case = i.c)
  x <- gsub("@", " ", x, ignore.case = i.c)
  x <- gsub("-", " ", x, ignore.case = i.c)
  
  # narrativism
  x <- gsub("can(i|o|e) nella vigna", " dogs in the vineyard ", x, ignore.case = i.c)
  x <- gsub("anime & sangue", " anime e sangue ", x, ignore.case = i.c)
  x <- gsub("project h(\\.)?o(\\.)?p(\\.)?e(\\.)?", " project h o p e ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("avventure in prima serata", " primetime adventures ", x, ignore.case = i.c)
  x <- gsub("aips", " primetime adventures ", x, ignore.case = i.c)
  x <- gsub("mia vita col padrone", " my life with master ", x, ignore.case = i.c)
  x <- gsub("solipsist[i]?", " solipsists ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("esoterroristi", " esoterrorists ", x, ignore.case = i.c)
  x <- gsub("cuori di mostro", " monsterhearts ", x, ignore.case = i.c)
  x <- gsub("penny per i miei pensieri", " a penny for my thoughts ", x, ignore.case = i.c)
  x <- gsub("di cosa hai paura", " fear itself ", x, ignore.case = i.c)
  x <- gsub("sporchi segreti", " dirty secrets ", x, ignore.case = i.c)
  x <- gsub("non cedere al sonno", " don't rest your head ", x, ignore.case = i.c)
  x <- gsub("gusto del delitto", " taste for murder ", x, ignore.case = i.c)
  x <- gsub("spirito del secolo", " spirit of the century ", x, ignore.case = i.c)
  x <- gsub("savage world", " savage worlds ", x, ignore.case = i.c, perl = TRUE)
  
  # warammer  
  x <- gsub("martelli da guerra", " warhammer ", x, ignore.case = i.c)
  x <- gsub("mdg", " warhammer ", x, ignore.case = i.c)
  x <- gsub("warhammer 40(000|[\\s]?k|[\\s]?mila)", " dark heresy ", x, ignore.case = i.c)
  x <- gsub("wfb", " warhmmer ", x, ignore.case = i.c)
  x <- gsub("wfrp", " warhammer ", x, ignore.case = i.c)

  # game of thrones
  x <- gsub("agot", " a game of thrones ", x, ignore.case = i.c)
  x <- gsub("a game of throne", " a game of thrones ", x, ignore.case = i.c)
  x <- gsub("trono di spade", " a game of thrones ", x, ignore.case = i.c)

  # buffy
  x <- gsub("buffy the vampire slayer", " buffy ", x, ignore.case = i.c)
  x <- gsub("buffy l'ammazzavampiri", " buffy ", x, ignore.case = i.c)
  
  # d&d  
  x <- gsub("original dungeon[s]?([\\s]?(e|and|&)[\\s]?)dragon[s]", " od&d ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("odnd", " od&d ", x, ignore.case = i.c)
  x <- gsub("od'n'd", " od&d ", x, ignore.case = i.c)
  x <- gsub("advanced dungeon[s]?([\\s]?(e|and|&)[\\s]?)dragon[s]", " ad&d ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("adnd", " ad&d ", x, ignore.case = i.c)
  x <- gsub("ad'n'd", " ad&d ", x, ignore.case = i.c)
  x <- gsub("dungeon[s]?([\\s]?(e|and|&)[\\s]?)dragon[s]", " d&d ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("dnd", " d&d ", x, ignore.case = i.c)
  x <- gsub("d'n'd", " d&d ", x, ignore.case = i.c)
  x <- ifelse(grepl("5[\\s]?ed", x, perl = TRUE) & !grepl("d&d", x), gsub("5ed", " d&d ", x), x)
  x <- ifelse(grepl("3\\.5", x, perl = TRUE) & !grepl("d&d", x), gsub("3\\.5", " d&d ", x, perl = TRUE), x)
  
  x <- gsub("quinta edizione", " d&d ", x, ignore.case = i.c)

  # coc
  x <- gsub("(chtulhu|cthulu)", "cthulhu", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("richiamo di", "call of", x, ignore.case = i.c)
  x <- gsub("tracce di", "trail of", x, ignore.case = i.c)
  x <- gsub("trails of cthulhu", " trail of cthulhu ", x, ignore.case = i.c)
  x <- gsub("coc", " call of cthulhu ", x, ignore.case = i.c)
  x <- ifelse(grepl("cthulhu", x) & !grepl("trail of", x) & !grepl("call of cthulhu", x), gsub("cthulhu", " call of cthulhu ", x), x)

  # l5r
  x <- gsub("l5a", " legend of the five rings ", x, ignore.case = i.c)
  x <- gsub("leggenda dei (5|cinque) anelli", " legends of the five rings ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("l5r", " legend of the five rings ", x, ignore.case = i.c)
  x <- gsub("legend of the (5|five) rings", " legend of the five rings", x, ignore.case = i.c, perl = TRUE)

  # m&m
  x <- gsub("mutant[s]?([\\s]?(e|and|&)[\\s]?)mastermind[s]", " mutants & masterminds ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("m(&|n)m", " mutants & masterminds ", x, ignore.case = i.c, perl = TRUE)

  # whitewold
  x <- gsub("licantropi", " werewolf ", x, ignore.case = i.c)

  x <- gsub("vampir(i|es)", "vampire", x, ignore.case = i.c)
  x <- gsub("(masquerade|requiem)", " vampire the \\1 ", x, perl = TRUE)
  x <- gsub("(vampir(?:e|i))[:]?[\\s]?(?:the|la|il)?[\\s](requiem|masquerade)", " vampire the \\2 ", x, perl = TRUE, ignore.case = i.c)
  x <- ifelse(grepl("vampire", x) & grepl("dark ages", x), gsub("vampire", " dark ages vampire ", x), x)
  x <- ifelse(grepl("vampire", x) & !grepl("(masquerade|requiem|buffy|discotheque|dark ages|city)", x, perl = TRUE), gsub("vampire", " vampire the masquerade ", x), x)

  x <- gsub("mondo di tenebra", " world of darkness ", x, ignore.case = i.c)
  x <- gsub("[(n|o)]?wod", " world of darkness ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("[(n|v)]?mdt", " world of darkness ", x, ignore.case = i.c)
  
  # starwars
  x <- gsub("sw", " star wars ", x, ignore.case = i.c)
  x <- gsub("guerre stellari", " star wars ", x, ignore.case = i.c)
  
  x <- gsub("cyberpunk[\\s](2020)?", " cyberpunk 2020 ", x, perl = TRUE, ignore.case = i.c)
  
  x <- gsub("3.16", " 3:16 ", x, ignore.case = i.c)
  x <- gsub("tmnt", " teenage mutants ninja turtles ", x, ignore.case = i.c)
  x <- gsub("tng", " star trek: the next generation ", x, ignore.case = i.c)
  x <- gsub("tsc", " mage ", x, ignore.case = i.c)
  x <- gsub("besm", " big eyes small mouth ", x, ignore.case = i.c)
  x <- gsub("æther", " aeter ", x, ignore.case = i.c)
  x <- gsub("megatraveller", " traveller ", x, ignore.case = i.c)
  x <- gsub("dogs of war", " dogs of w*a*r ", x, ignore.case = i.c)
  x <- gsub("hounds of god", " hounds of g.o.d ", x, ignore.case = i.c)
  x <- gsub("bunnies and burrows", " bunnies & burrows ", x, ignore.case = i.c)
  x <- gsub("harnmaster", " hârnmaster ", x, ignore.case = i.c)
  x <- gsub("tunnel[s]?([\\s]?(e|and|&)[\\s]?)troll[s]", " tunnels & trolls ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("blood and smoke", " blood & smoke ", x, ignore.case = i.c)
  x <- gsub("psirun", " psi*run ", x, ignore.case = i.c)
  x <- gsub("(21|twentyone) gun[s]?", " 21 guns ", x, ignore.case = i.c, perl = TRUE)
  x <- gsub("unico anello", " one ring ", x, ignore.case = i.c)
  x <- gsub("tredicesima era", " 13th age ", x, ignore.case = i.c)
  x <- gsub("(seven[th]?|7[th]?) sea", " 7th sea ", x, ignore.case = i.c, perl = TRUE)
  
  x
  }))
}  