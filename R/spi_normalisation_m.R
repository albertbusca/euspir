#' Dataframe Normalisation EU-SPI (2020)
#' @description
#' Normalise data frames according to the EU-SPI (2020) methodology.
#'
#' @param type Variable that accepts the following values:
#' - "regional" to normalise values for the NUTS-2 EU regions;
#' - "national" to normalise values for the EU state members.
#' @param normdata Normalisation data needed according to the EU-SPI (2020) methodology.
#' By default, it uses the official normalisation data provided by the European
#' Commission. See details to learn how to use custom normalisation data.
#' @param rawdata Data frame of values that need to be normalised. By default,
#' it uses the official raw indicator data provided by the European Commission.
#'
#' @details
#' ## Custom Normalisation Data
#'
#' The custom normalisation data needs to be a data frame containing 4 columns, as such:
#' - First column: (character) Should have the indicator name, one for each row.
#' - Second column: (logical) Should be TRUE if the indicator is inverted (negative relation
#' to the index) or FALSE otherwise (positive relation to the index).
#' - Third column: (numeric) Should have the utopian values for each indicator.
#' - Fourth column: (numeric) Should have the dystopian values for each indicator.
#'
#' ## Custom Raw Data
#'
#' The function expects the raw data to have one indicator per column and one row per
#' observation. All columns should be numeric. At the start of the dataframe, however,
#' the firsts columns can be character for labeling purposes -like the NUTS IDs or the
#' region names- and they will be preserved after the normalisation process.
#'
#' @examples
#' #With the official data
#' spi_normalisation_m(type = "national")
#'
#' #With custom data
#' normalisation_data <- data.frame(indicators = c("Indicator 1", "Indicator 2", "Indicator 3"),
#'                                  inverted = c(TRUE, FALSE, TRUE),
#'                                  utopian = c(0,100,24),
#'                                  dystopian = c(100,0,72))
#' raw_data <- data.frame(regions = c("Region A", "Region B"),
#'                        indicator1 = sample(0:100,2),
#'                        indicator2 = sample(0:100,2),
#'                        indicator3 = sample(24:72,2))
#' spi_normalisation_m(type = "regional", normdata = normalisation_data, rawdata = raw_data)
#'
#' @export

spi_normalisation_m <- function(type, normdata = NULL, rawdata = NULL) {
  #Handling given data for potential errors
  if (!is.character(type)) {stop(paste0("Error: 'type' must be a character vector, not a ", typeof(type), "."))}
  if (!(type %in% c("regional", "national"))) {stop(paste0("Error: 'type' must be either 'regional' or 'national', not ", type, "."))}
  if (!is.null(normdata)) {if(!is.data.frame(normdata)) {stop(paste0("Error: 'normdata' must be a dataframe, not a ", typeof(normdata), "."))}}
  if (!is.null(rawdata)) {if(!is.data.frame(rawdata)) {stop(paste0("Error: 'rawdata' must be a dataframe, not a ", typeof(rawdata), "."))}}
  type <- tolower(type)

  #Retrieve raw data when needed
  if (is.null(rawdata)) {rawdata <- spi_rawdata_down(type)}
  print(rawdata)
  #Retrieve normalisation data when needed
  if (is.null(normdata)) {normdata <- spi_rawdata_down("normalisation")}

  #Apply normalisation data to each indicator (column)
  norm_values <- rawdata[1:(which(vapply(rawdata, is.numeric, logical(1)))[1]-1)]
  i = 1
  for (column in which(sapply(rawdata, is.numeric))[1]:ncol(rawdata)) {
    norm_values <- cbind(norm_values, mapply(spi_normalisation,
                                                      rawdata[column],
                                                      inverted = normdata[i,2],
                                                      uto_val = normdata[i,3],
                                                      dis_val = normdata[i,4]))
    i = i + 1
  }

  #Return the normalised values
  return(norm_values)
}
