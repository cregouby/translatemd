#' translate an md file via LLM
#' 
#' `translate_llm()` will split the markdown file into chunks of around 100 lines
#' and make the llm proceed it through the instruction in prompt
#' 
#' @param md  md file name
#' 
#' @examples
#' parsed <- lightparser::split_to_tbl("my.qmd")
#' parsed |>
#'   unnest(cols = text) |>
#'   mutate(text_es = map(text,translate))
#' @export
#' @importFrom dplyr mutate lag
#' @importFrom purrr map_dbl

translate_llm <- function(md, prompt = prompt(from = "English", to = "French")){
  # lightparse file & accumulate section size
  md_lp <- lightparser::split_to_tbl(md) |>
    mutate(nline = map_dbl(text, length) + map_dbl(code, length) -1,
                           n_section = cumsum(!is.na(heading)),
                           cum_line = cumsum(nline)
  ) |> tibble::rowid_to_column()
  
  # split into chunks
  ideal_lines <- 100L
  chunk_start <- c(1L)
  for (chunk_id in seq_len(max(md_lp$cum_line) %/% ideal_lines)) {
    prev_cum_line <- md_lp |> filter(rowid == chunk_start[chunk_id]) |> pull(cum_line)
    chunk_start[chunk_id + 1] <- md_lp |>
      filter(!is.na(heading_level), cum_line <= ideal_lines + prev_cum_line ) |>
      slice_tail(n = 1) |>
      pull(rowid)
    
    if (chunk_start[chunk_id + 1] == chunk_start[chunk_id]) {
      # next chink is too large for ideal_lines
      # we take next heading candidate
      chunk_start[chunk_id + 1] <- md_lp |>
        filter(!is.na(heading_level), cum_line > prev_cum_line ) |>
        slice_head(n = 1) |>
        pull(rowid)
    }
    
  }

  # translate each chunk
  md_reassemble <- map2_chr(chunk_start, lead(chunk_start,default =  max(md_lp$rowid)),
                           ~lightparser::combine_tbl_to_file(md_lp[.x:.y,])
  )
  
  ollama_api_get <- function(model, prompt) {
    get <- httr::POST(url = "http://localhost:11434/api/generate",
                    httr::add_headers(model = model, prompt = prompt, stream = FALSE)
                    )
    get
  }
  md_translated <- future::map(md_reassemble, 
                               ~ollama_api_get(model = "llama3.1:latest", propmt = glue::glue(prompt, .x))
                               )
  }

#' create a translation prompt
#'
#' @param from plain text language to translate from
#' @param to plain text language to translate into
#'
#' @return the translation prompt
#' @export
prompt <- function(from, to) {
  glue::glue("Please translate the {from} text of the following rmarkdown vignette into {to}.",
             "Do not translate nor modify the YAML header block within `---`.",
             "Do not translate nor modify the code chunks, but only the code comments.",
             "Do not try to summarize but just translate.")
}