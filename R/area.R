#' calculate leaf area sandwitched by the chamber
#'
#'@param data gas-exchange data
#'@param width_mm leaf width in mm
#'@param model chamber used for the measurement
#'@importFrom magrittr %>%
#'@export
correct_leaf_area <-
  function(data, width_mm = 15, model = "6400-40"){

    # 6400-07 Needle Chamber 2 x 6 cm
    if(model == "6400-07|Needle"){
      chamber_area <- 12
      area <- pmin(width_mm/10 * 6, chamber_area)
    }

    # 6400-02B LED Light Source 2 x 3 cm
    if(model == "6400-02B|LED"){
      chamber_area <- 6
      area <- pmin(width_mm/10 * 3, chamber_area)
    }

    # 6400-40 Leaf Chamber Fluorometer 2 cm2 circle
    if(model == "6400-40|PAM"){
      chamber_area <- 2
      r <- sqrt(chamber_area/pi)
      l <- width_mm / 10
      if(l > 2 * r){
        area <- chamber_area
      } else {
        theta <- acos(l/(2*r))
        area <- chamber_area - 4 * (r^2 * theta / 2  - r^2 * cos(theta) * sin(theta) / (4 * l))
      }
    }

    return(area)
  }

#' recalculate gas exchange parameters by correcting leaf area
#'
#'@param data gas-exchange data
#'@param corrected_area corrected leaf area in cm2
#'@param default_area default leaf area in cm2
#'@importFrom magrittr %>%
#'@export
recalculate <-
  function(data, corrected_area, default_area){
    data %>%
      dplyr::mutate(Area = corrected_area,
                    Photo = Photo * default_area / Area,
                    Cond = Cond * default_area / Area,
                    Trmmol = Trmmol * default_area / Area)
  }
