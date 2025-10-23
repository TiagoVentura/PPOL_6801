
# ---------------------------
# LLM Annotation Harness (R)
# ---------------------------
# deps
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringi)
  library(jsonlite)
  library(lubridate)
  library(tibble)
})

# =====================================================
# Annotation Harness with Trade / Immigration / Jobs
# =====================================================

library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(tibble)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------
# Controlled Keyword Lists with Frame Categories
# ---------------------------

TRADE_KEYWORDS <- list(
  economic = c("exports","export","global markets","free trade",
               "competitiveness","trade agreement","trade deal",
               "open markets","access to markets"),
  jobs = c("job loss","outsourcing","offshoring","unfair trade",
           "lost jobs","tariff"),
  sovereignty_law = c("currency manipulation","dumping","protect",
                      "protectionist","sovereignty"),
  fairness = c("fair trade","level playing field","cheating","unfair partner")
)

IMMIGRATION_KEYWORDS <- list(
  economic = c("take jobs","steal jobs","cheap labor","burden","strain",
               "taxpayer","contribute","economic growth"),
  security = c("border crisis","border wall","invasion","cartel",
               "drug trafficking","fentanyl","ms-13","terrorism","gang",
               "public safety","secure the border"),
  cultural = c("assimilation","integration failure","foreign culture",
               "diluting","un-American","clash of cultures"),
  humanitarian = c("asylum","refugee","dreamers","daca","family unity",
                   "compassion","humanitarian","safe haven",
                   "pathway to citizenship"),
  identity = c("replacement","demographic shift","majority/minority",
               "losing control","heritage"),
  law_order = c("illegal aliens","illegal immigration","deport",
                "line-jumpers","cut in line","sovereignty",
                "respect the law","sanctuary cities"),
  political = c("open borders agenda","electoral advantage",
                "political takeover","globalist plot")
)

JOBS_KEYWORDS <- list(
  positive_economic = c("jobs","new jobs","hiring","job growth","opportunity",
                        "higher wages","pay raise","workforce development"),
  negative_economic = c("unemployment","job loss","economic decline","lost jobs",
                        "layoffs","outsourcing","underemployment","wages down")
)

# ---------------------------
# Schema (multi-frame stance support)
# ---------------------------

schema <- '{
  "doc_id": "%s",
  "trade": {
    "is_trade": boolean,
    "stances": [
      {
        "is_positive": boolean,
        "frame_categories": string[],
        "keywords": string[],
        "evidence": [{"text": string, "start_char": integer, "end_char": integer}]
      }
    ]
  },
  "immigration": {
    "is_immigration": boolean,
    "stances": [
      {
        "is_positive": boolean,
        "frame_categories": string[],
        "keywords": string[],
        "evidence": [{"text": string, "start_char": integer, "end_char": integer}]
      }
    ]
  },
  "jobs": {
    "is_jobs": boolean,
    "stances": [
      {
        "is_positive": boolean,
        "frame_categories": string[],
        "keywords": string[],
        "evidence": [{"text": string, "start_char": integer, "end_char": integer}]
      }
    ]
  },
  "notes": string,
  "confidence": number
}'

# ---------------------------
# Build Prompts
# ---------------------------

build_system_prompt <- function() {
  paste(
    "You are an expert political communication coder. Your task is to annotate 
    U.S. congressional newsletters for three talking points (Trade, Immigration, Jobs). 

General rules:
- Use ONLY the provided controlled keyword lists for each topic. 
- Multi-label allowed: A newsletter can contain multiple stances per topic, and each stance may involve multiple frames.
- For each stance, return:
  • is_positive: stance polarity (true for pro; false for anti)
  • frame_categories: one or more categories from the controlled lists
  • keywords: exact matches from controlled lists
  • evidence: verbatim substrings with 0-index [start, end) offsets
- Evidence must be explicit; if uncertain, default to no stance.
- Return only valid JSON matching the schema.",
    sep = ""
  )
}

format_keyword_list <- function(lst) {
  paste0("{",
         paste(
           sapply(names(lst), function(frame) {
             sprintf('"%s":[%s]', frame,
                     paste(sprintf('"%s"', lst[[frame]]), collapse = ","))
           }),
           collapse = ","),
         "}")
}

build_user_prompt <- function(
    doc_id, member, party, chamber, district_or_state,
    doc_date, text
) {
  doc_date <- as.character(as_date(doc_date))
  
  glue <- sprintf(
    'TASK: Annotate the following congressional newsletter using the “three-topic” framework.

METADATA:
- Member: %s (%s)
- Chamber: %s
- District/State: %s
- Newsletter date: %s

CONTROLLED LISTS:
- TradeKeywords: %s
- ImmigrationKeywords: %s
- JobKeywords: %s

TEXT:
%s

OUTPUT SCHEMA (JSON only):
%s

INSTRUCTIONS:
- Obey definitions and edge cases from the System message.
- Evidence spans must be verbatim substrings from TEXT with 0-index [start, end) offsets.
- For each stance, assign one or more frame_categories from the mapping above.
- If a field is false, return empty arrays/strings for its subfields.
- Return only valid JSON matching the schema. No extra keys.',
    member, party, chamber, district_or_state, doc_date,
    format_keyword_list(TRADE_KEYWORDS),
    format_keyword_list(IMMIGRATION_KEYWORDS),
    format_keyword_list(JOBS_KEYWORDS),
    text,
    schema
  )
  
  glue
}

# ---------------------------
# Chunking helper
# ---------------------------
chunk_text <- function(text, max_chars = 2000) {
  n <- nchar(text)
  if (n <= max_chars) return(list(list(text = text, start = 0)))
  starts <- seq(1, n, by = max_chars)
  lapply(starts, function(s) {
    e <- min(s + max_chars - 1, n)
    list(text = substr(text, s, e), start = s - 1)
  })
}


# ---------------------------
# LLM call (pluggable)
# --- OPTIONAL: Example OpenAI adapter (uncomment and fill your model/env) ---
Sys.setenv(OPENAI_API_KEY="ENTER API KEY HERE")
# install.packages('httr2')
library(httr2)
call_llm <- function(system_prompt, user_prompt, model = Sys.getenv("OPENAI_MODEL","gpt-4o-mini"), seed = 7) {
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0,
      response_format = list(type = "json_object"),
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      ),
      seed = seed
    ))
  resp <- httr2::req_perform(req)
  js <- httr2::resp_body_string(resp)
  out <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  out$choices[[1]]$message$content
}
# call_llm <- function(sys_prompt, user_prompt) {
#   if (grepl("secure the border", user_prompt)) {
#     return('{
#       "doc_id": "dummy1",
#       "trade": {"is_trade": false, "stances": []},
#       "immigration": {
#         "is_immigration": true,
#         "stances": [
#           {
#             "is_positive": false,
#             "frame_categories": ["security"],
#             "keywords": ["illegal immigration","secure the border"],
#             "evidence": [
#               {"text": "illegal immigration", "start_char": 35, "end_char": 52},
#               {"text": "secure the border", "start_char": 11, "end_char": 27}
#             ]
#           },
#           {
#             "is_positive": true,
#             "frame_categories": ["humanitarian"],
#             "keywords": ["pathway to citizenship","dreamers"],
#             "evidence": [
#               {"text": "pathway to citizenship", "start_char": 63, "end_char": 86},
#               {"text": "Dreamers", "start_char": 91, "end_char": 99}
#             ]
#           }
#         ]
#       },
#       "jobs": {"is_jobs": false, "stances": []},
#       "notes": "mixed stance",
#       "confidence": 0.9
#     }')
#   } else if (grepl("Unfair trade", user_prompt)) {
#     return('{
#       "doc_id": "dummy2",
#       "trade": {
#         "is_trade": true,
#         "stances": [
#           {
#             "is_positive": false,
#             "frame_categories": ["jobs","fairness"],
#             "keywords": ["unfair trade","outsourcing"],
#             "evidence": [
#               {"text": "unfair trade", "start_char": 0, "end_char": 12},
#               {"text": "outsourcing", "start_char": 60, "end_char": 71}
#             ]
#           }
#         ]
#       },
#       "immigration": {"is_immigration": false, "stances": []},
#       "jobs": {"is_jobs": false, "stances": []},
#       "notes": "anti-trade protectionist",
#       "confidence": 0.85
#     }')
#   } else {
#     return('{
#       "doc_id": "dummy3",
#       "trade": {"is_trade": false, "stances": []},
#       "immigration": {"is_immigration": false, "stances": []},
#       "jobs": {
#         "is_jobs": true,
#         "stances": [
#           {
#             "is_positive": true,
#             "frame_categories": ["positive_economic"],
#             "keywords": ["jobs","wages"],
#             "evidence": [
#               {"text": "jobs", "start_char": 40, "end_char": 44},
#               {"text": "wages", "start_char": 75, "end_char": 80}
#             ]
#           }
#         ]
#       },
#       "notes": "pro-jobs",
#       "confidence": 0.92
#     }')
#   }
# }

# ---------------------------
# Annotation Validator
# ---------------------------
validate_annotation <- function(ann, full_text, base_offset = 0) {
  issues <- c()
  check_span <- function(e) {
    if (is.null(e$text)) return(NULL)
    if (is.null(e$start_char) || is.null(e$end_char) ||
        is.na(e$start_char) || is.na(e$end_char)) {
      return("missing offsets")
    }
    if (e$start_char < 0 || e$end_char > nchar(full_text)) {
      return("offset out of range")
    }
    substring <- substr(full_text, e$start_char + 1, min(e$end_char, nchar(full_text)))
    if (!grepl(e$text, substring, fixed = TRUE)) {
      return(sprintf("mismatch: expected '%s', got '%s'", substring, e$text))
    }
    return(NULL)
  }
  
  for (topic in c("trade","immigration","jobs")) {
    stances <- ann[[topic]]$stances %||% list()
    for (s in stances) {
      evs <- s$evidence %||% list()
      for (e in evs) {
        prob <- check_span(e)
        if (!is.null(prob)) {
          issues <- c(issues, sprintf("%s evidence: %s", topic, prob))
        }
      }
    }
  }
  
  list(ok = length(issues) == 0, issues = issues)
}

# ---------------------------
# Frame Mapper
# ---------------------------
flatten_keywords <- function(lst, topic) {
  do.call(rbind, lapply(names(lst), function(frame) {
    data.frame(
      topic = topic,
      frame = frame,
      keyword = lst[[frame]],
      stringsAsFactors = FALSE
    )
  }))
}

TRADE_DF       <- flatten_keywords(TRADE_KEYWORDS, "trade")
IMMIGRATION_DF <- flatten_keywords(IMMIGRATION_KEYWORDS, "immigration")
JOBS_DF        <- flatten_keywords(JOBS_KEYWORDS, "jobs")
ALL_KEYWORDS   <- rbind(TRADE_DF, IMMIGRATION_DF, JOBS_DF)

map_frames <- function(annotation_json) {
  ann <- fromJSON(annotation_json, simplifyVector = FALSE)  # don’t auto-simplify
  
  process_topic <- function(topic_obj, topic_name) {
    if (isFALSE(topic_obj[[paste0("is_", topic_name)]])) return(topic_obj)
    topic_obj$stances <- lapply(topic_obj$stances, function(st) {
      # Force keywords into character vector
      kws <- st$keywords %||% character()
      kws <- unlist(kws, use.names = FALSE)
      kws <- as.character(kws)
      kws <- tolower(kws)
      
      matched_frames <- ALL_KEYWORDS %>%
        filter(topic == topic_name, tolower(keyword) %in% kws) %>%
        pull(frame) %>%
        unique()
      
      st$frame_categories <- matched_frames
      st$keywords <- kws
      st
    })
    topic_obj
  }
  
  ann$trade       <- process_topic(ann$trade, "trade")
  ann$immigration <- process_topic(ann$immigration, "immigration")
  ann$jobs        <- process_topic(ann$jobs, "jobs")
  
  ann
}

# ---------------------------
# Merge Chunk-level Annotations
# ---------------------------
merge_annotations <- function(chunk_list) {
  if (length(chunk_list) == 0) {
    return(list(
      doc_id = NA,
      trade = list(is_trade = FALSE, stances = list()),
      immigration = list(is_immigration = FALSE, stances = list()),
      jobs = list(is_jobs = FALSE, stances = list()),
      notes = "empty chunk list",
      confidence = NA
    ))
  }
  
  merged <- chunk_list[[1]]
  if (length(chunk_list) > 1) {
    for (i in 2:length(chunk_list)) {
      for (topic in c("trade","immigration","jobs")) {
        merged[[topic]]$stances <- c(
          merged[[topic]]$stances %||% list(),
          chunk_list[[i]][[topic]]$stances %||% list()
        )
      }
    }
  }
  merged
}

# ---------------------------
# Main: annotate one newsletter
# ---------------------------
annotate_newsletter <- function(
    doc_id, member, party, chamber, district_or_state,
    doc_date, text,
    retry = 3, sleep_secs = 1
) {
  sys <- build_system_prompt()
  chunks <- chunk_text(text)
  out_per_chunk <- list()
  
  for (i in seq_along(chunks)) {
    ch <- chunks[[i]]
    user <- build_user_prompt(
      paste0(doc_id, "_chunk", i),
      member, party, chamber, district_or_state,
      doc_date, ch$text
    )
    
    resp_json <- NULL
    for (attempt in seq_len(retry)) {
      resp <- try(call_llm(sys, user), silent = TRUE)
      if (!inherits(resp, "try-error")) { resp_json <- resp; break }
      Sys.sleep(sleep_secs * attempt)
    }
    if (is.null(resp_json)) {
      stop(sprintf("LLM call failed after %d attempts for %s (chunk %d)", retry, doc_id, i))
    }
    
    ann <- try(jsonlite::fromJSON(resp_json, simplifyVector = FALSE), silent = TRUE)
    if (inherits(ann, "try-error")) {
      stop(sprintf("Invalid JSON returned for %s (chunk %d): %s", doc_id, i, substr(resp_json, 1, 200)))
    }
    
    val <- validate_annotation(ann, ch$text, base_offset = ch$start)
    if (!val$ok) warning(sprintf("Validation issues for %s chunk %d: %s", doc_id, i, paste(val$issues, collapse = " | ")))
    
    ann <- map_frames(toJSON(ann, auto_unbox = TRUE, null = "null"))
    out_per_chunk <- append(out_per_chunk, list(ann))
  }
  
  merged <- merge_annotations(out_per_chunk)
  merged$doc_id <- doc_id
  merged
}

# ---------------------------
# Batch runner
# ---------------------------
annotate_batch <- function(df, ...) {
  pmap_dfr(df, function(doc_id, member, party, chamber, district_or_state,
                        doc_date, text, ...) {
    ann <- annotate_newsletter(doc_id, member, party, chamber, district_or_state,
                               doc_date, text, ...)
    tibble(
      doc_id = ann$doc_id,
      is_trade = ann$trade$is_trade %||% FALSE,
      is_immigration = ann$immigration$is_immigration %||% FALSE,
      is_jobs = ann$jobs$is_jobs %||% FALSE,
      confidence = as.numeric(ann$confidence %||% NA_real_),
      json = ann %>% toJSON(auto_unbox = TRUE, null = "null") %>% as.character()
    )
  })
}

# ---------------------------
# Corpus batch processor (resume-safe, chunked)
# ---------------------------
annotate_corpus_in_chunks <- function(df,
                                      batch_size = 500,
                                      out_file = "annotations.csv") {
  n <- nrow(df)
  batches <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  
  if (file.exists(out_file)) {
    results <- read.csv(out_file, stringsAsFactors = FALSE)
    done_ids <- results$doc_id
  } else {
    results <- NULL
    done_ids <- character()
  }
  
  for (i in seq_along(batches)) {
    idx <- batches[[i]]
    batch <- df[idx, ]
    
    batch <- batch[!batch$doc_id %in% done_ids, ]
    if (nrow(batch) == 0L) next
    
    message(sprintf("Processing batch %d / %d (%d rows)...",
                    i, length(batches), nrow(batch)))
    
    res <- annotate_batch(df = batch)
    
    if (is.null(results) && !file.exists(out_file)) {
      write_csv(res, out_file)
      readr::write_lines(res$json,gsub('\\.csv','.jsonl',out_file),)
    } else {
      write_csv(res, out_file,append = TRUE)
      readr::write_lines(res$json,gsub('\\.csv','.jsonl',out_file),append = T)
    }
    
    results <- rbind(results, res)
    done_ids <- results$doc_id
  }
  
  invisible(results)
}


# Testing it out
# =====================================================
# Dummy Dataset for Testing
# =====================================================

library(tibble)
library(lubridate)

df_dummy <- tibble::tibble(
  doc_id = c("D001", "D002", "D003"),
  member = c("Jane Doe", "John Smith", "Alice Johnson"),
  party = c("D", "R", "D"),
  chamber = c("House", "House", "Senate"),
  district_or_state = c("CA-12", "TX-07", "NY"),
  doc_date = as_date(c("2025-03-01", "2025-03-02", "2025-03-03")),
  text = c(
    # Immigration: mixed stance (security + humanitarian)
    "We must secure the border against illegal immigration, but also provide a pathway to citizenship for Dreamers.",
    
    # Trade: protectionist
    "Unfair trade practices from China have cost American workers their jobs. We must stand up against dumping and outsourcing.",
    
    # Jobs: pro jobs, wage growth
    "Our new infrastructure plan will create nearly 1.5 million jobs over five years and raise wages for hardworking families."
  )
)

print(df_dummy)
require(tidyverse)
annotate_corpus_in_chunks(df = df_dummy,
                          out_file = './data/testing_stance_cheaper.csv')
