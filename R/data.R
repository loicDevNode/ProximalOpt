#' Load the synthetic Douala housing dataset
#'
#' The dataset contains 500 simulated housing observations. It is educational
#' and does not represent verified market transactions or actual market prices.
#' @return A data frame.
#' @export
load_housing_data <- function() {
  path <- system.file("extdata", "05_donnees_logements.csv",
                      package = "ProximalOpt")
  if (!nzchar(path)) {
    stop("The packaged housing dataset could not be found.", call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}
