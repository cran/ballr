% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nba-per-game-adv-stats.R
\name{NBAPerGameAdvStatistics}
\alias{NBAPerGameAdvStatistics}
\title{NBA Player Advanced Statistics For a Given Season}
\usage{
NBAPerGameAdvStatistics(season = 2018)
}
\arguments{
\item{season}{A numeric year}
}
\value{
An object of class tbl_df
}
\description{
This function returns all player advanced statistics for each
player from an NBA season on basketball-reference.com.  See an example
table at:
https://www.basketball-reference.com/leagues/NBA_2018_advanced.html
}
\examples{
\dontrun{
library(magrittr)

players <- NBAPerGameAdvStatistics(season = 2018)
players

players \%>\%
  dplyr::filter(pos \%in\% c("SF")) \%>\%
  dplyr::select(player, link) \%>\%
  dplyr::distinct()
}

}
