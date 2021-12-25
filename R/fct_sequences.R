
# =========================================================================
# --- Check that sequence is valid --------------------------------------------
# =========================================================================


sequence_check <- function(sequence){
  sequence <- str_to_upper(sequence)
  sequence <- str_replace_all(sequence, "[\r\n]" , "")
  ((nchar(sequence) >= 130) &
   (str_detect(sequence, "[^ACGTRYSWKMBDHVN]", negate = TRUE)))
}

# =========================================================================
# --- Match all ASVs (pattern) to query sequences -----------------------------
# =========================================================================

# Compute % of ID for each ASV versus query sequence and order by decreasing PID

match_asv <- function(fasta.df, query){

  query <- str_to_upper(query)
  query <- str_replace_all(query, "[\r\n]" , "")
  
  query <-  Biostrings::DNAString(query)

  pattern <- Biostrings::DNAStringSet(fasta.df$sequence)
  names(pattern) <- fasta.df$asv_code

  asv_align <- Biostrings::pairwiseAlignment(pattern = pattern, subject = query, type = "global-local")

  scores <- data.frame(score=Biostrings::score(asv_align), pid = Biostrings::pid(asv_align) )
  
  df <- bind_cols(fasta.df, scores) %>%
    arrange(desc(pid)) %>%
    mutate(pid = round(pid, 2)) %>%  # Only 2 decimals
    select(asv_code, pid, kingdom:species, sequence, sum_reads_asv)
  
  # print(df)
  
  return(df)
}

# =========================================================================
# --- Match all ASVs (pattern) to query sequences using blaster-----------
# =========================================================================

# Compute % of ID for each ASV versus query sequence and order by decreasing PID

blaster_asv <- function(fasta.df, query, 
                        minIdentity = 0.80,
                        maxAccepts = 100){
  
  query <-  data.frame(Id = "query", Seq =query)
  
  db <- fasta.df %>% 
    select(Id = asv_code,
           Seq = sequence)
  
  df <- blaster::blast(query, db, 
                       minIdentity = minIdentity,
                       maxAccepts = maxAccepts) 
  # print(df)

  if(nrow(df) > 0) {
    df <- df %>% 
      select(pid = Identity, asv_code = TargetId, 
             mismatches = NumMismatches, gaps = NumGaps,
             query_start = QueryMatchStart, 
             query_end = QueryMatchEnd, 
             asv_start = TargetMatchStart, 
             asv_end = TargetMatchEnd) %>%
      mutate(pid = 100*pid) %>% 
      arrange(desc(pid)) %>%
      inner_join(fasta.df) %>% 
      select(asv_code, pid, mismatches, gaps, 
             query_start, query_end, asv_start, asv_end,  
             kingdom:species, sequence, sum_reads_asv)
  } else {
    df <- NULL
  }
  # print(df)
  
  return(df)
}



