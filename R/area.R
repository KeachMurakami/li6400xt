#' calculate leaf area sandwitched by the chamber
#'
#'@param data gas-exchange data
#'@param width_mm leaf width in mm
#'@param model chamber used for the measurement
#'@importFrom magrittr %>%
#'@export
correct_leaf_area <-
  function(data, width_mm, model = "6400-40"){

    default_area <- unique(data$Area)

    # 6400-07 Needle Chamber 2 x 6 cm
    if(model == "6400-07"){
      area <- pmin(width_mm/10 * 6, default_area)
    }

    # 6400-02B LED Light Source 2 x 3 cm
    if(model == "6400-02B"){
      area <- pmin(width_mm/10 * 3, default_area)
    }

    # 6400-40 Leaf Chamber Fluorometer 2 cm2 circle
    if(model == "6400-40"){
      r <- sqrt(default_area/pi)
      l <- width_mm / 10
      if(l > 2 * r){
        area <- default_area
      } else {
        theta <- acos(l/(2*r))
        area <- default_area - 4 * (r^2 * theta / 2  - r^2 * cos(theta) * sin(theta) / (4 * l))
      }
    }

    data %>%
      dplyr::mutate(Area = area,
                    Photo = Photo * default_area / Area,
                    Cond = Cond * default_area / Area,
                    Trmmol = Trmmol * default_area / Area) %>%
      return()
  }
