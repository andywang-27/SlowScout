exists_helper = function(string) {
  tryCatch({
    read_csv(string)
  }, error = function(e) {
    return(data.frame())
  })
}

raw_player_box26 = 
  rbind(
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "w_player_box26.csv")
    ),
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "m_player_box26.csv")
    )
  )

raw_team_box26 = 
  rbind(
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "w_team_box26.csv")
    ),
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "m_team_box26.csv")
    )
  )

raw_plays26 = 
  rbind(
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "w_plays26.csv")
    ),
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "m_plays_box26.csv")
    )
  )

if (!is.null(nrow("raw_player_box26"))) {
  player_box26 = 
    raw_player_box26 %>%
    merge(uaa_teams_csv, by="Team", all.x=T) %>%
    separate(`FGM-A`, into = c("FGM", "FGA"), sep = "-") %>%
    separate(`3PM-A`, into = c("3PM", "3PA"), sep = "-") %>%
    separate(`FTM-A`, into = c("FTM", "FTA"), sep = "-") %>%
    mutate(FGM = as.integer(FGM),
           FGA = as.integer(FGA),
           `3PM` = as.integer(`3PM`),
           `3PA` = as.integer(`3PA`),
           `2PM` = FGM - `3PM`,
           `2PA` = FGA - `3PA`,
           FTM = as.integer(FTM),
           FTA = as.integer(FTA),
           `FG%` = 100 * FGM / FGA,
           `3P%` = 100 * `3PM` / `3PA`,
           `FT%` = 100 * FTM / FTA,
           PPS = PTS / (FGA + 0.44*FTA),
           `AST/TO` = AST / TO,
           `FT-R` = 100 * FTA / FGA,
           `3P-R` = 100 * `3PA` / `FGA`,
           across(where(is.numeric), ~ round(., 1)),
           `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
           `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
           `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"))

  player_averages26 =
    player_box26 %>%
    rbind(player_box26) %>%
    filter(Team %in% uaa_teams) %>%
    group_by(League, Team, Conference, Player, URL) %>%
    summarize(URL = first(URL),
              across(where(is.numeric), mean, na.rm=TRUE),
              .groups='drop') %>%
    mutate(`FG%` = 100 * FGM / FGA,
           `3P%` = 100 * `3PM` / `3PA`,
           `FT%` = 100 * FTM / FTA,
           `FT-R` = 100 * FTA / FGA,
           `3P-R` = 100 * `3PA` / FGA,
           `AST/TO` = AST / TO,
           PPS = PTS / (FGA + 0.44*FTA),
           across(where(is.numeric), ~ round(., 1)),
           `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
           `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
           `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"))
}

refresh_dates26 = function() {
  if (exists("team_box26")) {
    if (length(filter(team_box26, League == "WBB")) == 0) {
      w_game_dates26 <<- unique(filter(team_box26, 
                                       League == "WBB",
                                       Team == "Chicago")$Date)
    } else {
      w_game_dates26 <<- c()
    }
    
    if (length(filter(team_box26, League == "MBB")) == 0) {
      m_game_dates26 <<- unique(filter(team_box26, 
                                       League == "MBB",
                                       Team == "Chicago")$Date)
    } else {
      m_game_dates26 <<- c()
    }
  } else {
    w_game_dates26 <<- c()
    m_game_dates26 <<- c()
  }
  
  w_all_dates26 <<- 
    unique(c(get_shots("WBB", "Chicago", y=2026)$Date, 
             get_events("WBB", "Chicago", y=2026)$Date)) %>%
    date_filter_helper(y=2026)
  w_practices26 <<- w_all_dates26[!(w_all_dates26 %in% w_game_dates26)]
  
  m_all_dates26 <<- 
    unique(c(get_shots("MBB", "Chicago", y=2026)$Date, 
             get_events("MBB", "Chicago", y=2026)$Date)) %>%
    date_filter_helper(y=2026)
  m_practices26 <<- m_all_dates26[!(m_all_dates26 %in% m_game_dates26)]
}

refresh_dates26()
