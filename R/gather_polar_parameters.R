#' @name gather_polar_parameters
#' @title Collect and process polar parameters
#' @author Nicolas Mangin
#' @description Module collecting the paraleters used to produce a polar distorsion.
#' @param path Character. Path to the folder containing parameter files.
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom purrr map
#' @importFrom scholR document_data
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom utils read.csv
#' @export

gather_polar_parameters <- function(path = "C:/Users/nicol/Pictures/Art/Polar"){
  
  data <- NULL
  name <- NULL
  drwcol <- NULL
  
  polar_parameters <- tibble::tibble(
    name = base::list.files(path, pattern = ".csv$"),
    file = base::list.files(path, pattern = ".csv$", full.names = TRUE)
  ) |>
    dplyr::mutate(
      name = stringr::str_remove_all(name, "^polar_|.csv$"),
      data = purrr::map(file, utils::read.csv)
    ) |>
    dplyr::mutate(data = purrr::map(data, function(x){
      if(base::length(x) < 17){
        x |>
          dplyr::rename(plt = drwcol) |>
          dplyr::mutate(
            cx = 1,
            ex = 1,
            cy = 0,
            ey = 1,
            ntl = 300,
            rotation = 0
          )
      } else if(base::length(x) < 22){
        x |>
          dplyr::rename(plt = drwcol) |>
          dplyr::mutate(
            cx = 1,
            ex = 1,
            cy = 0,
            ey = 1,
            ntl = 300
          )
      } else {
        x
      }
    })) |>
    tidyr::unnest(data)
  
  #base::save(polar_parameters, file = base::paste0(path, "/polar_parameters.RData"))
  #scholR::document_data(polar_parameters, "polar_parameters", path)
  
  return(polar_parameters)
}
