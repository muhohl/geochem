% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spider_plot.R
\name{spider_plot}
\alias{spider_plot}
\title{Spider Plot faceted}
\usage{
spider_plot(
  data,
  elements,
  group,
  levels = elements,
  ribbon = TRUE,
  ncol = NULL,
  ...
)
}
\arguments{
\item{data}{Dataframe}

\item{elements}{Select the elements to include in the plot as vector.}

\item{group}{Select the grouping of the data set as character variable. This needs to be the
name of a column in the data frame.}

\item{levels}{Select sequence of elements.}

\item{ribbon}{If TRUE the fourth and first quantile are summarised to a ribbon, if FALSE
single lines for those samples are plotted.}

\item{ncol}{Number of facet columns.}

\item{...}{Arguments passed on to ggplot2::theme().}
}
\description{
Produces spider plots highlighting different groups per facet and compares them
to the remaining groups.
}
