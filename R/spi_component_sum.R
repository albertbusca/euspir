#' Component or dimension aggregation within EU-SPI (2020)
#' @description
#' Aggregate components or dimensions following the EU-SPI (2020) methodology.
#'
#' @param type Variable that can take "regional dimension" to aggregate to the three
#' dimensions at the NUTS-2 level, "national dimension" to aggregate to the three dimensions
#' at the NUTS-0 (country) level, "regional spi" to aggregate to the EU-SPI index at the
#' NUTS-2 level and "national spi" to aggregate to the EU-SPI at the NUTS-0 level.
#' Alternatively, it can also take the value "custom" for custom data.
#' @param data Data frame of the components or dimensions to aggregate. The values
#' should be numerical. If not provided, it will take the corresponding official data
#' according to the type parameter.
#' @param name Name of the dimension or index that is to be retrieved
#' as the result of the aggregation. If not provided, it will take the corresponding
#' official data according to the type parameter. See details for using custom data.
#' @param b β value, a constant that can be adjusted to change the level of
#' compensability of the index. From β = 1 for the arithmetic mean to β = 0 for
#' the geometric mean. The default value is β = 0.5 as per the EU-SPI (2020) methodology.
#' @param index A list of numeric vectors. Only needed when type is custom. Each item
#' should represent the index of the components of each dimension. If not provided,
#' it will take the corresponding official indices for the official data or, if type
#' is custom, it will assume that everything must be aggregated to a single index.
#' @param append Logical argument. By default (TRUE) it will retain the original
#' values alongside the calculated aggregated index. Alternatively, if set to FALSE
#' it will only keep the aggregated values.
#'
#' @details
#' When using custom data, type must be set to custom. Data must be provided.
#'
#' ## Aggregating to a global index
#' When aggregating all variables to only one global indicator, the index parameter
#' can be omitted. It will be assumed that all numeric columns in 'data' should be
#' aggregated. The name variable is optional, the global index will be named
#' "Aggregated Index", or it can be changed using the parameter.
#'
#' ## Aggregating to various dimensions
#' When aggregating groups of variables to more than one dimension, the index parameter
#' must be stated. The name variable is optional. Each dimension will be named "Dimension [n]",
#' or it can be changed using the parameter.
#'
#' @examples
#' # With official data
#' spi_component_sum("regional spi") # For the regional EU-SPI
#' spi_component_sum("national dimension") # For the national dimensions
#'
#' # With custom data
#' #data <- data.frame("Regions" <- c("Region A", "Region B", "Region C"),
#' #                   "Component1" <- sample(1:100, 3),
#' #                   "Component2" <- sample(1:100, 3),
#' #                   "Component3" <- sample(1:100, 3))
#' #spi_component_sum(data, "Dimension1")
#'
#' @export

#This function aggregates components following the EU-SPI (2020) methodology
#This function aggregates dimensions following the EU-SPI (2020) methodology
spi_component_sum <- function(type, data = NULL, name = "Aggregated Index", b=0.5, index = NULL, append = TRUE){
  #Handle given data in case of potential errors
  if(!is.character(type)) {stop(paste0("Error: 'type' must be a character string, not a ", typeof(type), "."))}
  if(!is.null(data) && !is.data.frame(data)) {stop(paste0("Error: 'data' must be a dataframe, not a ", typeof(data), "."))}
  if(!is.character(name)) {stop(paste0("Error: 'name' must be a character string, not a ", typeof(type), "."))}
  if(!is.numeric(b)) {stop(paste0("Error: 'b' must be a number, not a ", typeof(type), "."))}
  type <- tolower(type)

  #Retrieve data if necessary
  if (is.null(data)) {
    if (type == "regional dimension") {data <- weighted_spi("all", append = FALSE)}
    if (type == "regional spi") {data <- spi_component_sum("regional dimension", append = FALSE)}
    if (type == "national dimension") {data <- spi_indicator_sum("national", append = FALSE)}
    if (type == "national spi") {data <- spi_component_sum("national dimension", append = FALSE)}
  }

  #Set the dimension(s) name
  if (name == "all") {
    if (type == "regional dimension" | type == "national dimension") {
      name <- c("Basic Human Needs",
                         "Foundations of Well-Being",
                         "Opportunity")
    }
    if (type == "regional spi" | type == "national spi") {name <- "EU-SPI"}
  }
  else if (type == "regional dimension" | type == "national dimension") {
    name <- c("Basic Human Needs",
                        "Foundations of Well-Being",
                        "Opportunity")
  }
  else if (type == "basic human needs") { name <- "Basic Human Needs" }
  else if (type == "foundations of well-being") { name <- "Foundations of Well-Being"}
  else if (type == "opportunity") { name <- "Opportunity" }
  else if (type == "custom") {
    if (!is.null(index)) { for(n in 1:length(index)) {
      name[n] <- paste("Dimension", n)
    }}
  }

  #Set the dimension(s) index
  if (is.null(index)) {
    if (type == "regional dimension") {index <- list(c(4:7),c(8:11),c(12:15))}
    if (type == "regional spi") {index <- list(c(4:6))}
    if (type == "national dimension") {index <- list(c(3:6),c(7:10),c(11:14))}
    if (type == "national spi") {index <- list(c(3:5))}
    if (type == "custom") {index <- which(sapply(data, is.numeric))}
  }

  result <- data[sapply(data, is.character)]

  for(n in 1:length(index)) {
    #First step: Retrieve data for the aggregation
    agg_data <- cbind(data[which(sapply(data, is.character))],
                      data[index[[n]]])

    #Second step: elevate the values to b
    elevate_to_b <- agg_data[which(sapply(agg_data, is.numeric))[1]:ncol(agg_data)]^b

    #Third step: Sum by row (region)
    sum_row <- rowSums(elevate_to_b, na.rm = TRUE)

    #Fourth step: 1 divided by the number of indicators
    one_indicator_n <- 1/(rowSums(!is.na(agg_data)) - sum(sapply(agg_data, is.character)))

    #Fifth step: sum_row * one_indicator_n
    fifth_step <- sum_row*one_indicator_n

    #Sixth step: elevate the values to (1/b)
    agg_data[name[n]] <- fifth_step^(1/b)

    #Return the dimension(s)
    agg_data <- agg_data[sapply(agg_data, is.numeric)]
    if(append == TRUE){result <- cbind(result, agg_data)}
    else{result <- cbind(result, agg_data[ncol(agg_data)])}
  }

  #Retrieve the aggregated dimension
  return(result)
}
