#' Select Language
#' 
#' for example "Helsinki-NLP/opus-mt-en-es"
#' @export
#' @param lang translation model to use like "Helsinki-NLP/opus-mt-en-es"
#' @importFrom reticulate import
select_lang <- function(lang){
  translator <- transformers$pipeline(task = "translation", model=lang)
}



