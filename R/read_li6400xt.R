#' read gas-exchange data from LI6400XT
#'
#'@param file path
#'@param inherit_pam logical; if `TRUE` xxx
#'
#'@importFrom magrittr %>%
#'@export
read_licor <- function(file, inherit_pam = FALSE){

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


  ### replace chlorophyll fluorescence data

  pam_data <-
    tidy_data %>%
    dplyr::distinct(Fo, Fm, `Fo'`, `Fm'`, `Fv/Fm`, `Fv'/Fm'`, PhiPS2, qP, qN, NPQ, .keep_all = TRUE) %>%
    dplyr::select(time, Fo, Fm, `Fo'`, `Fm'`, `Fv/Fm`, `Fv'/Fm'`, PhiPS2, qP, qN, NPQ, ETR)

  # remove first row if not inherit
  if(!inherit_pam){ pam_data <- dplyr::slice(pam_data, -1) }

  # closest time matching
  insert_index <- pam_data$time %>% map_int(~ abs(. - tidy_data$time) %>% which.min)

  time_matched_pam_data <-
    pam_data %>%
    dplyr::mutate(pam_data, Obs = as.character(insert_index), time = NULL)

  result <-
    tidy_data %>%
    dplyr::select(-c(Fo, Fm, `Fo'`, `Fm'`, `Fv/Fm`, `Fv'/Fm'`, PhiPS2, qP, qN, NPQ, ETR)) %>%
    dplyr::left_join(time_matched_pam_data, by = "Obs")

  attributes(result)$remark <-
    raw_data[change_rows[1], "X1", drop = TRUE] %>%
    stringr::str_sub(start = 10, end = -1)

  attributes(result)$controls <-
    raw_data[change_rows[-1], ] %>%
    tidyr::extract(X1, into = c("time", "log"), regex = "(.+:[0-9]+) (.+)") %>%
    dplyr::mutate(time = lubridate::ymd_hms(paste0(date, " ", time)),
                  file = paste0(basename(file)))


  return(result)
}

#' extract control procedure from change logs
#'
#'@param data output of `read_licor`
#'@param track tracked operation
#'@param variable name of new variable
#'
#'@importFrom magrittr %>%
#'@importFrom rlang !!
#'@importFrom rlang :=
#'@export
track_changes <-
  function(data, track = "LCF", variable = "PPFD"){

    changes <-
      attributes(data)$controls %>%
      dplyr::filter(stringr::str_detect(log, track)) %>%
      dplyr::mutate(log = stringr::str_replace(log, "Off", ": -> Off"),
                    log = stringr::str_extract(log, ":.*-> [0-9.]*")) %>%
      tidyr::separate(log, into = c("variable", "value"), sep = "->") %>%
      dplyr::mutate(variable = NULL, value = as.numeric(value))


    output <-
      data %>%
      dplyr::mutate(value = NA_real_)
    for(i in 1:(NROW(changes))){
      output <-
        output %>%
        dplyr::mutate(value = dplyr::if_else(time > changes$time[i], changes$value[i], value))
    }

    output %>%
      dplyr::rename(!!(variable) := value) %>%
      return()
  }
