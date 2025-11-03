#' Aggregation of indicators within the EU-SPI (2020)
#' @description
#' Aggregate a set of indicators as seen in the EU-SPI (2020) methodology.
#'
#' @param type Variable that can take the values "national" when the values to
#' aggregate are at the NUTS-0 level or "regional" when the values to aggregate are
#' at the NUTS-2 level.
#' @param data Data frame with the values of each indicator (column), for each
#' region or country (rows). By default (data = NULL) it takes the official
#' data from the European Commission.
#' @param component_name Name of the component(s) to be obtained as the final result
#' of the aggregation. Alternatively, and by default, setting it to "all" uses the
#' names of the twelve official components on the EU-SPI (2020).
#' @param component_index List of the column indeces of each component within data.
#' Each item of the list should be a numeric vector stating the indeces of each
#' component's indicators. By default it will assume that the index follows the
#' official data when data is not provided or that all the indicators are to be
#' aggregated to only one component when data is provided.
#' @param append Logical argument. By default (TRUE) retains the values of all
#' indicators alongside the aggregated values of each component. When set to FALSE,
#' only the aggregated values are kept.
#'
#' @examples
#' #With the official data
#' regional_components <- spi_indicator_sum("regional")
#' national_components <- spi_indicator_sum("national")
#'
#' #With custom data
#' data <- data.frame("Regions" = c("Region A", "Region B", "Region C"),
#'                    "Indicator 1" = sample(1:100,3),
#'                    "Indicator 2" = sample(1:100,3),
#'                    "Indicator 3" = sample(1:100,3))
#' spi_indicator_sum("regional", data,"Component 1")
#'
#' @export

spi_indicator_sum <- function(type, data = NULL, component_name = "all", component_index = NULL, append = TRUE) {
  #Handle the data given in case of potential errors
  if(!is.character(type)) {stop(paste0("Error: 'type' must be a character vector, not a ", typeof(type), "."))}
  if(!is.null(data)) {if(!is.data.frame(data)) {stop(paste0("Error: 'data', must be a dataframe, not a ", typeof(data), "."))}}
  if(!is.character(component_name)) {stop(paste0("Error: 'component_name' must be a character vector, not a ", typeof(component_name), "."))}
  type <- tolower(type)

  #Retrieve the data if needed
  if (is.null(data)) {
    data <- spi_normalisation_m(type)
    component_index <- list(c(1:4),c(5:8),c(9:12),c(13:16),c(17:19),c(20:23),c(24:29),
                       c(30:33),c(34:39),c(40:44),c(45:51),c(52:55))
    is_character <-  which(sapply(data, is.character))
    component_index <- lapply(component_index, function(x) x + is_character[length(is_character)])
    repetition <- 1:12
    }
  else {
    if (is.null(component_index)) {component_index <- list(which(sapply(data, is.numeric)))}
    repetition <- length(component_index)
  }

  #Set the name of the component
  if (any(component_name == "all")) {
    component_name <- c("Nutrition and Basic Medical Care",
                        "Water and Sanitation",
                        "Shelter",
                        "Personal Security",
                        "Access to Basic Knowledge",
                        "Access to ICT",
                        "Health and Wellness",
                        "Environmental Quality",
                        "Personal Rights",
                        "Personal Freedom and Choice",
                        "Tolerance and Inclusion",
                        "Access to Advanced Education")}

  #Apply the aggregation formula
  is_character <- which(sapply(data, is.character))
  result <- data[is_character]

  for (rep in repetition) {
    rep_result <- data[component_index[[rep]]]
    rep_result[component_name[rep]] <- rowMeans(rep_result, na.rm = TRUE)
    if (append == TRUE) {result <- cbind(result, rep_result)}
    else {result <- cbind(result, rep_result[component_name[rep]])}
  }
  return(result)
}
