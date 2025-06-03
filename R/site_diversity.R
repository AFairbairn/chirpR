#' Create a by-site summary with diversity metrics
#'
#' This function creates site-level summaries of ecological data, calculating
#' vocal activity rates if needed and computing diversity metrics including Hill numbers.
#' The function can work with pre-calculated abundance data or calculate vocal activity
#' rates internally.
#'
#' @param df A data frame containing the ecological data
#' @param abundance_col Character string specifying the column name containing abundance
#'   data to summarize. If NULL (default), vocal activity rate will be calculated internally
#' @param species_col Character string specifying the column name containing species names.
#'   Default is "scientific_name"
#' @param site_col Character string specifying the column name containing site identifiers.
#'   Default is "site"
#' @param abundance_sum_stat Character string specifying the summary statistic to apply to the
#'   abundance data. Default is "sum". Other options include "mean", "median", "max", "min"
#' @param ... Additional arguments passed to vocal_activity_rate() when abundance_col is NULL.
#'   This should include time_col, date_col, and any other parameters needed for VAR calculation
#'
#' @return A data frame with site-level summaries including:
#'   \item{site}{Site identifier}
#'   \item{abundance_summary}{Summary statistic of abundance data for each site}
#'   \item{species_richness}{Number of species per site}
#'   \item{shannon_diversity}{Shannon diversity index}
#'   \item{simpson_diversity}{Simpson diversity index}
#'   \item{richness}{Hill number q=0 (species richness)}
#'   \item{shannon}{Hill number q=1 (exp of Shannon index)}
#'   \item{simpson}{Hill number q=2 (inverse Simpson)}
#'
#' @details
#' The function works in several steps:
#' 1. If no abundance_col is provided, it calculates vocal activity rate using vocal_activity_rate()
#'    (time_col, date_col, and other VAR parameters should be passed via ...)
#' 2. Creates a species × site matrix with the specified summary statistic
#' 3. Calculates diversity metrics using the vegan package
#' 4. Calculates Hill numbers with intuitive names (richness, shannon, simpson)
#'
#' Hill numbers provide a unified framework for measuring diversity:
#' - q=0 (richness): Species richness (number of species)
#' - q=1 (shannon): Exponential of Shannon entropy
#' - q=2 (simpson): Inverse Simpson concentration
#'
#' @examples
#' \dontrun{
#' # Using pre-calculated abundance data
#' result <- site_diversity(data, abundance_col = "count")
#'
#' # Calculate vocal activity rate internally
#' result <- site_diversity(data, abundance_col = NULL,
#'                            time_col = "timestamp", date_col = "date")
#'
#' # Custom summary statistic
#' result <- site_diversity(data, abundance_col = "detections",
#'                            abundance_sum_stat = "mean")
#' }
#'
#' @importFrom vegan diversity
#' @export
site_diversity <- function(df,
                              abundance_col = NULL,
                              species_col = "scientific_name",
                              site_col = "site",
                              abundance_sum_stat = "sum",
                              ...) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  # Required columns depend on whether abundance_col is provided
  if (!is.null(abundance_col)) {
    required_cols <- c(species_col, site_col, abundance_col)
  } else {
    # When abundance_col is NULL, we only require species and site cols
    # time_col, date_col etc. should be passed via ... to vocal_activity_rate()
    required_cols <- c(species_col, site_col)
  }

  missing_cols <- required_cols[!required_cols %in% names(df)]
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Step 1: Calculate vocal activity rate if abundance_col is NULL
  if (is.null(abundance_col)) {
    df <- vocal_activity_rate(df,
                              species_col = species_col,
                              site_col = site_col,
                              ...)
    abundance_col <- "var"
  }

  # Step 2: Create species × site matrix using base R
  # First, create site-species-abundance summary
  site_species_summary <- aggregate(
    df[[abundance_col]],
    by = list(site = df[[site_col]], species = df[[species_col]]),
    FUN = match.fun(abundance_sum_stat),
    na.rm = TRUE
  )
  names(site_species_summary)[3] <- "abundance_value"

  # Create wide matrix
  sites <- unique(site_species_summary$site)
  species <- unique(site_species_summary$species)

  # Initialize matrix with zeros
  count_matrix <- matrix(0, nrow = length(sites), ncol = length(species))
  rownames(count_matrix) <- sites
  colnames(count_matrix) <- species

  # Fill matrix with values
  for (i in seq_len(nrow(site_species_summary))) {
    site_name <- as.character(site_species_summary$site[i])
    species_name <- as.character(site_species_summary$species[i])
    count_matrix[site_name, species_name] <- site_species_summary$abundance_value[i]
  }

  # Step 3: Calculate site-level abundance summary
  abundance_summary <- aggregate(
    df[[abundance_col]],
    by = list(site = df[[site_col]]),
    FUN = match.fun(abundance_sum_stat),
    na.rm = TRUE
  )
  names(abundance_summary)[2] <- "abundance_summary"

  # Ensure sites are in same order as matrix
  abundance_summary <- abundance_summary[match(rownames(count_matrix), abundance_summary$site), ]

  # Step 4: Calculate diversity metrics
  species_richness <- rowSums(count_matrix > 0)
  shannon_diversity <- vegan::diversity(count_matrix, index = "shannon")
  simpson_diversity <- vegan::diversity(count_matrix, index = "simpson")
  q2 <- vegan::diversity(count_matrix, index = "invsimpson")

  # Step 5: Create result data frame with all Hill numbers
  result <- data.frame(
    site = rownames(count_matrix),
    setNames(list(abundance_summary$abundance_summary), abundance_col),
    richness = species_richness,
    shannon = shannon_diversity,
    simpson = simpson_diversity,
    q1 = exp(shannon_diversity),
    q2 = q2,
    stringsAsFactors = FALSE
  )

  # Ensure row names are reset
  rownames(result) <- NULL

  return(result)
}
