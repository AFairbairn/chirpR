% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/site_diversity.R
\name{site_diversity}
\alias{site_diversity}
\title{Create a by-site summary with diversity metrics}
\usage{
site_diversity(
  df,
  abundance_col = NULL,
  species_col = "scientific_name",
  site_col = "site",
  abundance_sum_stat = "sum",
  ...
)
}
\arguments{
\item{df}{A data frame containing the ecological data}

\item{abundance_col}{Character string specifying the column name containing abundance
data to summarize. If NULL (default), vocal activity rate will be calculated internally}

\item{species_col}{Character string specifying the column name containing species names.
Default is "scientific_name"}

\item{site_col}{Character string specifying the column name containing site identifiers.
Default is "site"}

\item{abundance_sum_stat}{Character string specifying the summary statistic to apply to the
abundance data. Default is "sum". Other options include "mean", "median", "max", "min"}

\item{...}{Additional arguments passed to vocal_activity_rate() when abundance_col is NULL.
This should include time_col, date_col, and any other parameters needed for VAR calculation}
}
\value{
A data frame with site-level summaries including:
\item{site}{Site identifier}
\item{abundance_col or "var"}{Summary statistic of abundance data for each site. Column name
matches the input abundance_col parameter, or "var" if vocal activity rate was calculated internally}
\item{richness}{Species richness (Hill number q=0) - number of species per site}
\item{shannon}{Shannon diversity index (H') - natural logarithm based}
\item{simpson}{Simpson diversity index (1-D) - probability that two randomly selected
individuals belong to different species}
\item{q1}{Hill number q=1 - exponential of Shannon diversity (exp(H')), representing
the "effective number of common species"}
\item{q2}{Hill number q=2 - inverse Simpson diversity (1/D), representing the
"effective number of dominant species"}
}
\description{
This function creates site-level summaries of ecological data, calculating
vocal activity rates if needed and computing diversity metrics including Hill numbers.
The function can work with pre-calculated abundance data or calculate vocal activity
rates internally.
}
\details{
The function works in several steps:
\enumerate{
\item If no abundance_col is provided, it calculates vocal activity rate using vocal_activity_rate()
(time_col, date_col, and other VAR parameters should be passed via ...)
\item Creates a species × site matrix with the specified summary statistic
\item Calculates diversity metrics using the vegan package
\item Calculates Hill numbers for unified diversity measurement
}

Hill numbers provide a unified framework for measuring diversity with different
sensitivities to species frequencies:
\itemize{
\item q=0 (richness): Species richness - counts all species equally regardless of abundance
\item q=1 (shannon/q1): Exponential of Shannon entropy - weights species by their frequency,
giving the effective number of "common" species
\item q=2 (q2): Inverse Simpson concentration - emphasizes dominant species,
giving the effective number of "very abundant" species
}

The traditional Shannon and Simpson indices are also provided:
\itemize{
\item Shannon index (H'): Measures uncertainty in species identity of a randomly chosen individual
\item Simpson index (1-D): Probability that two randomly chosen individuals are different species
}
}
\examples{
\dontrun{
# Using pre-calculated abundance data
result <- site_diversity(data, abundance_col = "count")

# Calculate vocal activity rate internally
result <- site_diversity(data, abundance_col = NULL,
                           time_col = "timestamp", date_col = "date")

# Custom summary statistic
result <- site_diversity(data, abundance_col = "detections",
                           abundance_sum_stat = "mean")
}

}
