find.explicit.speakers <- function(novel.pos) {
  exp <- lapply(seq_along(novel.pos), function(i, pos){
    
    p <- pos[[i]]
    n <- names(pos[i])
    
    candidate.speakers <- do.call(plyr::rbind.fill, lapply(seq_along(p[['tokens']]), function(ti, tokenset, sentenceset, entityset){
      
      tokens <- tokenset[[ti]]
      sentences <- sentenceset[[ti]]
      entities <- entityset[[ti]]
      index <- paste(n, ti, sep = ".")
      
      quotes.marks <- which(tokens$value == "\"")
      has.quotes <- length(quotes.marks) > 0
      
      flog.trace(sprintf("[%s - %d] >> %s", n, ti, paste(sentences$content, collapse = " ")))
      
      if(has.quotes){
        
        # I don't want dialogue utterances to be parsed as ppl "talking" inside dialogues are
        # probably not present in the room.
        
        exclude <- data.frame(start = quotes.marks[c(TRUE, FALSE)],
                              end = quotes.marks[c(FALSE, TRUE)])
        
        tokens.remove <- -do.call(c, lapply(1:nrow(exclude), function(x) exclude$start[x]:exclude$end[x]))
        
        tokens.q <- tokens[tokens.remove,]
        verbs <- (verbs.dictionary %>% filter(direct.dialogue == TRUE))$verb
        
        if(length(tokens.remove) == nrow(tokens)){
          flog.debug(sprintf("[%s - %d] Line has utterances, but no candidates", n, ti))
          return(data.frame(index = index,
                            quote = TRUE,
                            value = NA,
                            proper = NA,
                            gender = NA,
                            type = NA,
                            wikipedia_url = NA,
                            speaker_type = "implicit",
                            stringsAsFactors = FALSE))
        }
        
      } else {
        
        # If the sentence ends with a question mark, it's probably a dubitative, so FOR NOW it's wiped
        # TODO: a better recognition method
        
        tokens.q <- tokens
        
        if(tokens.q[nrow(tokens.q),]$value == "?"){
          flog.debug(sprintf("[%s - %d] Line ends with a question mark, so it's probably a dubitative.", n, ti))
          return(data.frame(index = index,
                            quote = has.quotes,
                            value = NA,
                            proper = NA,
                            gender = NA,
                            type = NA,
                            wikipedia_url = NA,
                            speaker_type = "ambiguous",
                            stringsAsFactors = FALSE))
        }
        
        # If the line has no dialogues, the entire sentence is parsed
        # but spotting a smaller list of verbs.
        
        verbs <- (verbs.dictionary %>% filter(implicit.dialogue == TRUE))$verb
        
      }
      
      target.verbs.positions <- as.integer(row.names(tokens.q[which(tokens.q$value %in% verbs),]))
      
      # Spanning the sentence to find verbs from the verbs list.
      # Only those verbs syntactically referring to the root particle (or the root particle itself) are reported
      
      conj.ref.root <- (tokens.q %>% filter(tag == "VERB" & (label == "CONJ" | label == "ROOT" | label == "DEP") & row.names(.) %in% target.verbs.positions))$headTokenIndex
      rcmod.ref <- (tokens.q %>% mutate(idx = (as.integer(row.names(.)) - 1)) %>% filter((label == "RCMOD" | label == "ADVCL") & row.names(.) %in% target.verbs.positions))$idx
      
      possibly.speakings <- tokens.q %>% 
        filter((label == "NSUBJ" | label == "DOBJ") & (headTokenIndex %in% c(conj.ref.root, rcmod.ref))) %>%
        mutate(index = paste(n, ti, sep = ".")) 
      
      if(!is.null(entities)){  
        possibly.speakings <- left_join(possibly.speakings, entities, by = c("content" = "name", "beginOffset" = "beginOffset")) %>%
          mutate(wikipedia_url = ifelse(("wikipedia_url" %in% names(.)), wikipedia_url, NA))
      } else {
        possibly.speakings <- possibly.speakings %>%
          mutate(wikipedia_url = NA, type = NA)
      }
      
      explicit.speakers <- possibly.speakings %>%
        filter(tag == "NOUN" & proper == "PROPER")
      
      anaphoric.speakers <- possibly.speakings %>%
        filter((tag == "PRON" | tag == "NOUN") & proper == "PROPER_UNKNOWN")
      
      if(nrow(explicit.speakers) > 0){
        
        flog.debug(sprintf("[%s - %d] Line has explicit candidates: %s", n, ti, paste(explicit.speakers$value, collapse = ", ")))
        
        return(explicit.speakers %>%
                 mutate("quote" = rep(has.quotes, nrow(explicit.speakers))) %>%
                 mutate("speaker_type" = rep("explicit", nrow(explicit.speakers))) %>%
                 select("index", "quote", "value", "proper", "gender", "type", "wikipedia_url", "speaker_type")
        )
        
      } else {
        if(length(target.verbs.positions) > 0){
          if(nrow(anaphoric.speakers) >= 1){
            flog.debug(sprintf("[%s - %d] Line has implicit candidates: %s", n, ti, paste(anaphoric.speakers$value, collapse = ", ")))
            return(anaphoric.speakers %>%
                     mutate("quote" = rep(has.quotes, nrow(anaphoric.speakers))) %>%
                     mutate("speaker_type" = rep("anaphoric", nrow(anaphoric.speakers))) %>%
                     select("index", "quote", "value", "proper", "gender", "type", "wikipedia_url", "speaker_type")
            )
          }
          
          flog.debug(sprintf("[%s - %d] Line has candidates, but POS was not able to isolate them", n, ti))
          return(data.frame(index = index,
                            quote = has.quotes,
                            value = NA,
                            proper = NA,
                            gender = NA,
                            type = NA,
                            wikipedia_url = NA,
                            speaker_type = "ambiguous",
                            stringsAsFactors = FALSE))
          
        } else {
          return(data.frame(index = index,
                            quote = TRUE,
                            value = NA,
                            proper = NA,
                            gender = NA,
                            type = NA,
                            wikipedia_url = NA,
                            speaker_type = "none",
                            stringsAsFactors = FALSE))
          
        }
      }
    }, tokenset = p[['tokens']], sentenceset = p[['sentences']], entityset = p[['entities']])
    )
    
    # If there's a candidate in the line following an implicit utterance, and the latter is not a dialogue, the candidate
    # is spent on the current line
    
    ####
    # LOCAL ALIASES TABLE
    # -
    # These aliases are valid only in the scope
    # of the current conversation
    
    # 1. If somewhere else in the conversation there is a title being associated to a named entity, then
    # all the implicit references to that title in the same conversation are locally associated to that entity
    
    titles.lut <- (do.call(plyr::rbind.fill, lapply(seq_along(p[['tokens']]), function(ti, tokenset, sentenceset, entityset){
      
      tokens <- tokenset[[ti]]
      sentences <- sentenceset[[ti]]
      entities <- entityset[[ti]]
      index <- paste(n, ti, sep = ".")
      
      if(is.null(tokens) | is.null(entities)) return(NULL)
      
      rcmod.ref <- tokens %>% 
        mutate(idx = (as.integer(row.names(.)) + 1)) %>%
        filter(label == "TITLE")
      
      x <- left_join(tokens, rcmod.ref, by=c("headTokenIndex"="idx")) %>%
        left_join(entities, by=c("value.x"="name")) %>%
        filter(proper.x == "PROPER" & !is.na(content.y)) %>%
        #mutate(alias=ifelse(grepl("(Mr|Mrs|Miss|Madam|Sir)", content.y, ignore.case = TRUE), paste(content.y, value.x, sep = " "))) %>%
        mutate(alias=content.y, value=value.x, type) %>%
        mutate(wikipedia_url = {if("wikipedia_url" %in% names(.)) wikipedia_url else NA}) %>%
        select(alias, value, type, wikipedia_url)
      
    }, tokenset = p[['tokens']], sentenceset = p[['sentences']], entityset = p[['entities']])
    )) %>%
      mutate(n=1) %>%
      group_by(alias, value, type, wikipedia_url) %>%
      summarise(n = sum(n)) %>%
      arrange(alias, desc(n))
    
    if(nrow(titles.lut) > 0){
      titles.lut <- do.call(plyr::rbind.fill, lapply(split(titles.lut, titles.lut$alias), data.table::first)) %>% select(-n)
    }
    
    
    titles.lut <- titles.lut %>%
      ungroup() %>%
      mutate(op = grepl("(Mr|Mrs|Miss|Madam|Sir)", alias, ignore.case = TRUE)) %>%
      mutate(title.value=ifelse(op, paste(alias, value, sep = " "), alias)) %>%
      mutate(alias = ifelse(op, value, alias), value = ifelse(op, title.value, value)) %>%
      select(-title.value, -op)
    
    if(nrow(titles.lut) > 0){
      titles.lut <- do.call(plyr::rbind.fill, lapply(split(titles.lut, titles.lut$alias), data.table::first))
    }
    
    # 2. Create a temporary lookup table of the only entities that have a valid wikipedia_url source. These are going to be
    # used for elements with same value but no wikipedia_url. The assumption here is that on the same conversation
    # we're probably referring to the same entity.
    
    
    # speakers.lut <- unique(plyr::rbind.fill(candidate.speakers %>%
    #                                           filter(!is.na(wikipedia_url)) %>%
    #                                           mutate(value = as.character(value)) %>%
    #                                           mutate(alias = value) %>%
    #                                           select("alias", "value", "type", "wikipedia_url"),
    #                                           titles.lut)
    #                       )
    
    speakers.lut <- titles.lut
    
    speakers.lut$alias <- as.character(speakers.lut$alias)
    
    # Lag
    candidate.speakers$lag <- ifelse(candidate.speakers$speaker_type == "implicit", lag(ifelse(!candidate.speakers$quote & candidate.speakers$speaker_type != "none", candidate.speakers$value, NA)), NA)
    candidate.speakers$lag.type <- ifelse(candidate.speakers$speaker_type == "implicit", lag(ifelse(!candidate.speakers$quote & candidate.speakers$speaker_type != "none", candidate.speakers$type, NA)), NA)
    candidate.speakers$value <- ifelse(is.na(candidate.speakers$value) & !is.na(candidate.speakers$lag), candidate.speakers$lag, candidate.speakers$value)
    candidate.speakers$type <- ifelse(is.na(candidate.speakers$type) & !is.na(candidate.speakers$lag.type), candidate.speakers$lag.type, candidate.speakers$type)
    
    # Lead
    candidate.speakers$lead <- ifelse(candidate.speakers$speaker_type == "implicit", lead(ifelse(!candidate.speakers$quote & candidate.speakers$speaker_type != "none", candidate.speakers$value, NA)), NA)
    candidate.speakers$lead.type <- ifelse(candidate.speakers$speaker_type == "implicit", lead(ifelse(!candidate.speakers$quote & candidate.speakers$speaker_type != "none", candidate.speakers$type, NA)), NA)
    candidate.speakers$value <- ifelse(is.na(candidate.speakers$value) & !is.na(candidate.speakers$lead), candidate.speakers$lead, candidate.speakers$value)
    candidate.speakers$type <- ifelse(is.na(candidate.speakers$type) & !is.na(candidate.speakers$lead.type), candidate.speakers$lead.type, candidate.speakers$type)
    
    candidate.speakers$value <- as.character(candidate.speakers$value)
    
    # TODO: for now I'm just getting the explicit candidates
    
    flog.debug(sprintf("[%s - X] Resolving local aliases", n))
    
    candidate.speakers <- left_join(candidate.speakers, speakers.lut, by=c("value"="alias"))
    candidate.speakers$value <- ifelse(!is.na(candidate.speakers$value.y), candidate.speakers$value.y, candidate.speakers$value)
    candidate.speakers$type <- ifelse(!is.na(candidate.speakers$type.y), candidate.speakers$type.y, candidate.speakers$type.x)
    candidate.speakers$wikipedia_url <- ifelse(!is.na(candidate.speakers$wikipedia_url.y), candidate.speakers$wikipedia_url.y, candidate.speakers$wikipedia_url.x)
    
    candidate.speakers <- candidate.speakers %>% select(-value.y, -type.y, -type.x, -wikipedia_url.x, -wikipedia_url.y)
    
    candidate.speakers$conversation <- n
    candidates <- candidate.speakers %>%
      filter(proper == "PROPER") %>%
      select("conversation", "value")
    
    # 2. Create a temporary lookup table of the only entities that have a valid wikipedia_url source. These are going to be
    # used for elements with same value but no wikipedia_url. The assumption here is that on the same conversation
    # we're probably referring to the same entity.
    
    wikipedia.lut <- candidate.speakers %>%
      filter(!is.na(wikipedia_url)) %>%
      select("value", "wikipedia_url") %>%
      mutate(n=1) %>%
      group_by(value, wikipedia_url) %>%
      summarise(n = sum(n)) %>%
      arrange(value, desc(n))
    
    if(nrow(wikipedia.lut) > 0){
      wikipedia.lut <- do.call(plyr::rbind.fill, lapply(split(wikipedia.lut, wikipedia.lut$value), data.table::first)) %>% select(-n)
    }
    
    candidates <- candidates %>% group_by(conversation, value) %>% summarise(dialogues = n()) # %>% left_join(wikipedia.lut, by = "value")
    
    if(is_empty(candidates$value)) {flog.info(sprintf("[%s] No speakers were selected in this chunk", n))}
    else {flog.info(sprintf("[%s] Selected speakers: %s", n, paste(candidates$value, collapse = ", ")))}
    
    return(candidates)
    
  }, pos = novel.pos)
  
  return(exp)
  
}