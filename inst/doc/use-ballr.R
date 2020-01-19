## ----setup, message = FALSE, warning = FALSE-----------------------------
library(ballr)

## ---- message = F, warning = F-------------------------------------------
library(magrittr)
library(ggplot2)
library(janitor)
library(scales)

## ---- message = F, warning = F-------------------------------------------
standings <- NBAStandingsByDate() # "YEAR-MO-DY"
standings

## ------------------------------------------------------------------------
standings <- NBAStandingsByDate("2015-12-31")
standings

## ------------------------------------------------------------------------
players <- NBAPerGameStatistics()
players

## ------------------------------------------------------------------------
players <- NBAPerGameStatistics(season = 2017)

## ------------------------------------------------------------------------
players %>%
  dplyr::filter(mp > 20, pos %in% c("SF")) %>%
  dplyr::select(player, link) %>%
  dplyr::distinct()

## ------------------------------------------------------------------------
players <- NBAPerGameStatisticsPer36Min(season = 2017)

## ------------------------------------------------------------------------
players

## ------------------------------------------------------------------------
players <- NBAPerGameStatisticsPer36Min(season = 2019) %>%
  dplyr::filter(pos %in% c("SF", "PF")) %>%
  dplyr::top_n(n = 10, pts) %>% 
  dplyr::select(player, link) %>%
  dplyr::distinct()

## ------------------------------------------------------------------------
players

## ------------------------------------------------------------------------
for(i in 1:dim(players)[1]){
  print(i)#  i <- 7
  try({
    tmp <- NBAPlayerPerGameStats(players[i, 2]) %>%
      dplyr::filter(!is.na(age)) %>%
      dplyr::mutate(player = as.character(players[i, 1]))
    if(is.numeric(tmp$g)){
      if(exists("player_stats")){
        player_stats <- rbind(player_stats, tmp)
      } else{
        player_stats <- tmp
      }
    }
  }, silent = T)
}

## ------------------------------------------------------------------------
p <- ggplot2::ggplot(data = player_stats,
            aes(x = age, y = efgpercent, group = player, col = player))
p + ggplot2::geom_line(alpha = .25) +
  ggplot2::geom_point(alpha = .25) +
  ggplot2::scale_y_continuous("effective field goal %age", limit = c(0, 1),
                     labels = percent) +
  ggplot2::theme_bw()


## ------------------------------------------------------------------------
per_100 <- NBAPerGameStatisticsPer100Poss(season = 2018)
utils::head(per_100)

## ------------------------------------------------------------------------
adv_stats <- NBAPerGameAdvStatistics(season = 2018)
utils::head(adv_stats)

## ------------------------------------------------------------------------
library(rvest)

url <- "http://www.basketball-reference.com/teams/DEN/2017.html"
links <- xml2::read_html(url) %>%
    rvest::html_nodes(".center+ .left a") %>%
    rvest::html_attr('href')

## ------------------------------------------------------------------------
links 

