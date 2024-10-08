% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xmod_double_mapping.R
\name{xmod_double_mapping}
\alias{xmod_double_mapping}
\title{XMOD Double Maps}
\usage{
xmod_double_mapping(
  xmod_dfs_list,
  cutoff = 0.2,
  sel_min,
  sel_min_wrap = "Selected",
  gangue_min_wrap = "Gangue",
  color_sel_min = ggplot2::scale_color_viridis_d,
  color_gangue_min = ggplot2::scale_color_viridis_d,
  my_sel_colors = FALSE,
  my_gangue_colors = FALSE,
  name_append = NULL,
  directory = "./XMOD_Plots",
  filetype = ".png"
)
}
\arguments{
\item{xmod_dfs_list}{A list containing all the XMOD dataframes to plot.}

\item{cutoff}{Minimum percentage a mineral must occur in the map with remaining minerals.}

\item{sel_min}{Vector containing the selected minerals.}

\item{sel_min_wrap}{Name of the selected minerals. Defaults to Ore.}

\item{gangue_min_wrap}{Name of the remaining minerals. Defaults to Gangue.}

\item{color_sel_min}{Color scale for the selected minerals. Default is the viridis color scale.
If color provided to my_sel_colors this one needs not to be changed.}

\item{color_gangue_min}{Color scale for the remaining minerals. Default is the plasma virdis color scale.
If color provided to my_gangue_colors this one needs not to be changed.}

\item{my_sel_colors}{Custom color values for the selected minerals.}

\item{my_gangue_colors}{Custom color values for the remaining minerals.}

\item{name_append}{Suffix to append to the plot file.}

\item{directory}{Directory created within the current working environment in which the plots
are saved. !Warning! This might override existing data.}

\item{filetype}{Filetype ending. Default is ".png".}
}
\description{
Function that plots two XMOD maps, one with the selected minerals
and one with the remaining minerals. This allows to highlight paragenetic
relationships.
}
