#' read gas-exchange data from LI6400XT
#'
#'@param file path
#'
#'@importFrom magrittr %>%
#'@importFrom magrittr %$%
#'@export
read_licor <- function(file){

  raw_data <-
    suppressWarnings(readr::read_csv(file, col_names = FALSE, col_types = readr::cols()))

  data_rows <- grep(raw_data$X1, pattern = "^[0-9]+\t")
  change_rows <- grep(raw_data$X1, pattern = "^[0-9]{2}:")
  pam_rows <- grep(raw_data$X1, pattern = "=")

  info_rows <- which(!(seq_along(raw_data$X1) %in% unique(c(data_rows, change_rows, pam_rows))))

  info <-
    raw_data[info_rows,]

  date <-
    info[2 , 1, drop = TRUE] %>%
    stringr::str_sub(start = 5, end = 15) %>%
    lubridate::mdy()

  variables <-
    utils::tail(info, 1)$X1 %>%
    stringr::str_split(pattern = '\\"\\t"') %>%
    .[[1]] %>%
    stringr::str_replace(pattern = "\\?", replacement = "")

  tidy_data <-
    raw_data[data_rows,] %>%
    tidyr::separate(col = X1, into = variables, sep = "\\t") %>%
    dplyr::mutate_at(c(-1, -2), as.numeric) %>%
    dplyr::mutate(file = paste0(basename(file)),
                  time = as.POSIXct(date + lubridate::hms(stringr::str_replace_all(HHMMSS, '\\\\|"', ""))),
                  .before = 1,
                  HHMMSS = NULL)

  attributes(tidy_data)$remark <-
    raw_data[change_rows[1], "X1", drop = TRUE] %>%
    stringr::str_sub(start = 10, end = -1)

  attributes(tidy_data)$controls <-
    raw_data[change_rows[-1], ] %>%
    tidyr::extract(X1, into = c("time", "log"), regex = "(.+:[0-9]+) (.+)") %>%
    dplyr::mutate(time = lubridate::ymd_hms(paste0(date, " ", time)),
                  file = paste0(basename(file)))


  return(tidy_data)
}

#' extract control procedure from change logs
#'
#'@param data output of `read_licor`
#'@param track_variable variable tracked
#'@param name_variable name of variable in the output tibble
#'
#'@importFrom magrittr %>%
#'@importFrom magrittr %$%
#'@importFrom rlang !!
#'@importFrom rlang :=
#'@export
track_changes <-
  function(data, track_variable = "ParIn", name_variable = "PPFD"){

    changes <-
      attributes(data)$controls %>%
      dplyr::filter(stringr::str_detect(log, track_variable)) %>%
      dplyr::mutate(log = stringr::str_extract(log, ":.*-> [0-9.]*")) %>%
      tidyr::separate(log, into = c("variable", "value"), sep = "->") %>%
      dplyr::mutate(variable = NULL, value = as.numeric(value))

    output <-
      data %>%
      dplyr::mutate(value = NA_real_)
    for(i in 1:(NROW(changes)-1)){
      output <-
        output %>%
        dplyr::mutate(value = dplyr::if_else(time > changes$time[i], changes$value[i], value))
    }

    output %>%
      dplyr::rename(!!(name_variable) := value) %>%
      return()
  }
