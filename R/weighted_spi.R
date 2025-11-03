#' Calculation of the weighted components within EU-SPI (2020)
#' @description
#' Calculate the weighted value of a given component with the population data of
#' the NUTS-2 regions, according to the EU-SPI (2020) methodology.
#' @param country NUTS-0 code of the country to weight. For more than one country,
#' it should be a string vector of the NUTS-0 codes to be used. It can also take
#' the value "all" so that it is applied to all EU countries at once.
#' @param unweighted_values Data frame with the values to weight. By default it
#' uses the official EU-SPI (2020) data. See details to learn how to use custom
#' data.
#' @param national_value Data frame with the national values to use. By default it
#' uses the official EU-SPI (2020) data. See details to learn how to use custom
#' data.
#'
#' @details
#' ## Custom unweighted values
#' - First column(s): At least one of the first columns should be called "NUTS_ID",
#' this column should have the NUTS-2 code for each region to weight.
#' - The following columns should be numeric and each one should have one component
#' to be weighted. Each row should represent one region.
#'
#' ## Custom national values
#' - First column(s): At least one of the first columns should be called "NUTS_ID",
#' and have the NUTS-0 code for each country.
#' - The following columns should be numeric and each one should have the national
#' values of one component. Each row should represent one country.
#'
#' @examples
#' # With official data
#' ## For one country
#' weighted_spi("AT") # Weighted values for Austria
#' ## For several countries
#' weighted_spi(c("BE","ES")) # Weighted values for Belgium and Spain
#' ## For all countries
#' weighted_spi("all")
#'
#' # With custom data
#' data <- data.frame("NUTS_ID" = c("AT11", "AT12", "AT13", "AT21",
#'                                  "AT22", "AT31", "AT32", "AT33", "AT34"),
#'                    "Component1" = sample(1:100,9),
#'                    "Component2" = sample(1:100,9),
#'                    "Component3" = sample(1:100,9))
#' national_val <- data.frame("NUTS_ID" = "AT",
#'                            "Component1" = 52,
#'                            "Component2" = 49,
#'                            "Component3" = 55)
#' weighted_spi("AT", data, national_val)
#'
#' @importFrom eurostat get_eurostat
#' @importFrom stats aggregate weighted.mean
#'
#' @export

weighted_spi <- function(country, unweighted_values = NULL, national_value = NULL, append = TRUE){
  #Handling of given data in case of potential errors
  if (!is.null(unweighted_values)) {if(!is.data.frame(unweighted_values)) {
    stop(paste0("Error: 'unweighted_values' must be a data frame, but it was of type: ", typeof(unweighted_values), "."))}}
  if (!is.null(unweighted_values)) {if(!is.data.frame(national_value)) {
    stop(paste0("Error: 'national_value' must be a data frame, but it was of type: ", typeof(national_value), "."))}}
  if (!is.character(country)) {stop(paste0("Error: 'country' must be a string, but it was of type: ", typeof(national_value), "."))}
  country <- tolower(country)
  if (!any(country == "all")) {stopifnot(nchar(country[1]) == 2)
    country <- toupper(country)}

  #Retrieving the data if needed
  if (is.null(unweighted_values)) {
    if(append == TRUE){unweighted_values <- spi_indicator_sum("regional")}
    else{unweighted_values <- spi_indicator_sum("regional", append = FALSE)}
    colnames(unweighted_values)[2] <- "NUTS_ID"
    }
  if (is.null(national_value)) {
    if(append == TRUE){national_value <- spi_indicator_sum("national")}
    else{national_value <- spi_indicator_sum("national", append = FALSE)}
    colnames(national_value)[1] <- "NUTS_ID"
    }

  #Creation of the data base of population and countries
  population <- eurostat::get_eurostat(id = "tgs00096", filters = list(time = c(2017:2019)))[c(5,7)]
  if (any(country == "all")) {
    population <- subset(population, !grepl("AL|CH|HR02|HR05|HR06|IS|XX|ME|MK|LI|NO|RS", population$geo))
    population <- subset(population, !grepl("AL|CH|HR02|HR05|HR06|IS|XX|ME|MK|LI|NO|RS", population$geo))
    population <- na.omit(population)
    population <- population[order(population$geo),][1:720,]
    }
  else {
    country <- paste(country, collapse = "|")
    population <- subset(population, grepl(paste0("^", country), population$geo))
    population <- subset(population, !grepl("XX", population$geo))
    }
  population <- aggregate(values ~ geo, data = population, mean)
  countries <- unique(substr(population$geo, start = 1, stop = 2))

  # Application of the formula to each component
  weighted_data <- unweighted_values[which(sapply(unweighted_values, is.character))]
  is_num <- which(sapply(unweighted_values, is.numeric))
  diff_num <- as.numeric(which(sapply(national_value, is.numeric))[1] - is_num[1])

  for (component in (is_num)) {
    #A pplication of the formula to each corresponding country
    result <- c()
    for (c in countries) {
      weights <- population[grepl(paste0("^", c), population$geo),]
      unweighted <- unweighted_values[grepl(paste0("^", c), unweighted_values$NUTS_ID), component]
      national <- national_value[grepl(c, national_value$NUTS_ID), (component+diff_num)]

      #Application of the formula to each corresponding region
      reg_result <- c()
      for (r in 1:nrow(weights)) {
        w_mean <- weighted.mean(unweighted, weights$values, na.rm = TRUE)
        reg_result <- c(reg_result, (national + unweighted[r] - w_mean))
      }
      result <- c(result, reg_result)
    }
    weighted_data <- cbind(weighted_data, result)
  }
  colnames(weighted_data) <- colnames(unweighted_values)
  return(weighted_data)
}
