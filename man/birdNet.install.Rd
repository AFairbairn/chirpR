% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birdNet_install.R
\name{birdNet.install}
\alias{birdNet.install}
\title{Gets and installs the latest version of BirdNet from github}
\usage{
birdNet.install(path, ...)
}
\arguments{
\item{path}{Path to save BirdNet, defaults to package directory.}

\item{...}{Other parameters to be passed to download.file()}
}
\description{
birdNet.install downloads the latest version of BirdNet from github and installs
it and the necessary python libraries using a virtual environment in the package directory,
unless otherwise specified. Directory ~/BirdNet-Analyzer/BirdNet-Analyzer-main.
When specifying a home directory, this same directory must be used as path in
analyze().
}
\examples{
\dontrun{
birdNet.install()
birdNet.install(path="C:/projectFolder/")}
}
