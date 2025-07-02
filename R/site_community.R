#' Create Site-Species Community Matrix
#'
#' This function creates a site-species community matrix from ecological data,
#' with options for abundance data or presence-absence data. The resulting matrix
#' has sites as rows and species as columns, making it suitable for ordination
#' analyses such as NMDS.
#'
#' @param df A data frame containing ecological survey data with site, species,
#'   and abundance information.
#' @param site_col Character string specifying the column name containing site
#'   identifiers. Default is "site".
#' @param species_col Character string specifying the column name containing
#'   species identifiers. Default is "scientific_name".
#' @param abundance_col Character string specifying the column name containing
#'   abundance or count data. Default is "var".
#' @param presence_absence Logical value indicating whether to return
#'   presence-absence data (TRUE) or abundance data (FALSE). When TRUE, any
#'   abundance value > 0 is converted to 1, otherwise 0. Default is FALSE.
#'
#' @return A numeric matrix with sites as rows and species as columns. Row names
#'   are site identifiers and column names are species identifiers. For abundance
#'   data, values represent the abundance/count values. For presence-absence data,
#'   values are 1 (present) or 0 (absent).
#'
#' @details
#' The function handles duplicate site-species combinations by summing abundances
#' (when presence_absence = FALSE) or taking the maximum presence value (when
#' presence_absence = TRUE). Missing combinations are filled with zeros.
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   site = c("A", "A", "B", "B", "C"),
#'   scientific_name = c("Species1", "Species2", "Species1", "Species3", "Species2"),
#'   var = c(5, 3, 2, 1, 0)
#' )
#'
#' # Create abundance matrix
#' abundance_matrix <- site_community(sample_data)
#'
#' # Create presence-absence matrix
#' pa_matrix <- site_community(sample_data, presence_absence = TRUE)
#'
#' # Use custom column names
#' custom_data <- data.frame(
#'   location = c("Site1", "Site2"),
#'   species_name = c("Bird1", "Bird2"),
#'   count = c(10, 5)
#' )
#' custom_matrix <- site_community(custom_data,
#'                                site_col = "location",
#'                                species_col = "species_name",
#'                                abundance_col = "count")
#'
#' @export
site_community <- function(df,
                           site_col = "site",
                           species_col = "scientific_name",
                           abundance_col = "var",
                           presence_absence = FALSE) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame")
  }

  if (nrow(df) == 0) {
    stop("Input data frame is empty")
  }

  # Check if required columns exist
  required_cols <- c(site_col, species_col, abundance_col)
  missing_cols <- required_cols[!required_cols %in% names(df)]

  if (length(missing_cols) > 0) {
    stop("Missing columns in dataframe: ", paste(missing_cols, collapse = ", "))
  }

  # Check if abundance column is numeric
  if (!is.numeric(df[[abundance_col]])) {
    stop("Abundance column '", abundance_col, "' must be numeric")
  }

  # Extract relevant columns
  site_data <- df[[site_col]]
  species_data <- df[[species_col]]
  abundance_data <- df[[abundance_col]]

  # Get unique sites and species
  unique_sites <- sort(unique(site_data))
  unique_species <- sort(unique(species_data))

  # Initialize matrix with zeros
  community_matrix <- matrix(0,
                             nrow = length(unique_sites),
                             ncol = length(unique_species),
                             dimnames = list(unique_sites, unique_species))

  # Fill the matrix
  for (i in seq_len(nrow(df))) {
    site <- site_data[i]
    species <- species_data[i]
    abundance <- abundance_data[i]

    if (presence_absence) {
      # For presence-absence: any abundance > 0 becomes 1
      value <- ifelse(abundance > 0, 1, 0)
      # Take maximum in case of duplicates (1 if any presence)
      community_matrix[site, species] <- max(community_matrix[site, species], value)
    } else {
      # For abundances: sum values in case of duplicates
      community_matrix[site, species] <- community_matrix[site, species] + abundance
    }
  }

  return(community_matrix)
}
