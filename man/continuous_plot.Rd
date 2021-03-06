% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/continuous_plot.R
\name{continuous_plot}
\alias{continuous_plot}
\title{continuous_plot}
\usage{
continuous_plot(
  data,
  vector,
  plot,
  color = 1,
  binwidth = 0.5,
  title = "continuous plot",
  xlabel = NULL,
  ylabel = NULL,
  freq = TRUE,
  vertical = TRUE
)
}
\description{
This function visualizes and summarizes continuous data.
}
\examples{

continuous_plot(iris, iris$Sepal.Length, "hist", binwidth = 0.1, freq=FALSE, title="histogram")
}
