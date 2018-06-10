merge.dialogues <- function(novel.sentences){
  
  dialogue.start <- which((str_count(novel.sentences, "\"") %% 2) != 0)
  
  if(is_empty(dialogue.start)) return(novel.sentences)
  
  quotes.fill <- data.frame(dialogue.start) %>%
    mutate(n = row_number())
  
  quotes.fill$dialogue.end <- ifelse((quotes.fill$n %% 2) != 0, lead(quotes.fill$dialogue.start, 1), NA)
  quotes.fill$dialogue.next <- ifelse((quotes.fill$n %% 2) != 0, lead(quotes.fill$dialogue.start, 2, default = NROW(novel.sentences)), NA)
  quotes.fill$dialogue.before <- ifelse((quotes.fill$n %% 2) != 0, lag(quotes.fill$dialogue.start, 2, default = 0), NA)
  
  
  quotes.fill <- quotes.fill %>% filter(!is.na(dialogue.end)) %>%
    select(-n)
  
  quotes.gaps <- do.call(rbind, lapply(split(quotes.fill, seq(nrow(quotes.fill))), function(x) { 
    
    prologue <- NULL
    
    dialogue.hold <- seq(to = (x$dialogue.next - 1), from = (x$dialogue.end + 1))
    dialogue.prologue <- seq(to = (x$dialogue.start - 1), from = (x$dialogue.before + 1))
    
    if(x$dialogue.before == 0 & x$dialogue.start > 0) prologue <- data.frame(dialogue.start = dialogue.prologue, dialogue.end = dialogue.prologue, stringsAsFactors = FALSE)
    
    if((x$dialogue.end + 1) >= x$dialogue.next) return(rbind(prologue, x[,c("dialogue.start", "dialogue.end")]))
    
    
    return(rbind(prologue, x[,c("dialogue.start", "dialogue.end")], data.frame(dialogue.start = dialogue.hold, dialogue.end = dialogue.hold, stringsAsFactors = FALSE)))
  })
  )
  
  novel.sentences.delim <- do.call(c, lapply(split(quotes.gaps, seq(nrow(quotes.gaps))), function(c, novel) {
    paste(novel[c$dialogue.start:c$dialogue.end], collapse = " ")
  }, novel = novel.sentences))
  
  # Somewhere, NA were parsed to "NA". I'm reintroducing NA values here as I need to properly
  # split by chapter later on.
  novel.sentences.delim[which(grepl("^NA$", novel.sentences.delim, perl = TRUE))] <- NA
  
  return(novel.sentences.delim)
  
}