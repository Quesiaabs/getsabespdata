#' Pega os Dados Pluviometricos da Sabesp
#'
#' @param data_coleta Data de coleta dos dados no formato "YYYY-MM-DD".
#'
#' @returns dataset com os dados pluviometricos da Sabesp.
#' @export
#'
#' @examples
#' #pega_manaciais_sabesb(data_coleta = Sys.Date())
#'
#' # pega os o ultimo mes
#' #data_coleta = Sys.Date()-1:30
#'
#' # pega para a ultima semana
#' # data_coleta = Sys.Date()-1:7
pega_manaciais_sabesp <- function(data_coleta = Sys.Date()) {
  url <- stringr::str_glue(
    'https://mananciais.sabesp.com.br/api/Mananciais/ResumoSistemas/{data_coleta}')

  pega_dados <- function(url_funcao){
    jsonlite::fromJSON(url_funcao) %>%
      purrr::pluck("ReturnObj", "sistemas") %>%
      dplyr::tibble() %>%
      janitor::clean_names()
  }

  purrr::map(
    url,
    ~pega_dados(.x)
  ) %>%
    purrr::list_rbind()
}
