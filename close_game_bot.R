# twitter bot

`%>%` <- magrittr::`%>%`

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("BillPetti/baseballr")
# twitter bot functions

# reverse %in%
`%not_in%` <- purrr::negate(`%in%`)

fetch_live_games <- function(day){
  # day = desired date to pull games for, as.Date
  cat("hello")
  
  games <- baseballr::get_game_pks_mlb(day, level_ids = c(1)) %>%
    dplyr::select(game_pk, gameType, season, officialDate, status.abstractGameState,
           status.detailedState, teams.away.team.name, teams.home.team.name) %>%
    janitor::clean_names() %>%
    dplyr::filter(status_detailed_state == "In Progress") %>%
    dplyr::rename(away_team = teams_away_team_name) %>%
    dplyr::rename(home_team = teams_home_team_name)
  return(games)
}

#Functions needed:
# 1. Determine if game is "close game" (1-run game or tying run at the plate in 8th inning or later)
#    - currently just test if a "1-run bot" works. Afterwards, we can add the stuff for runners on base
#    - currently this will tweet out before every pitch, I only want it once per inning.
# 2. Tweet writer

close_games <- function(mlb_pbp, mlb_games){
  print("close_games")
	close_game_alerts <- mlb_pbp %>% 
		dplyr::select(-game_date) %>%
		dplyr::left_join(
	      dplyr::select(
	        mlb_games,
	        game_type,game_pk, official_date
	        ),
	      by = "game_pk") %>%
		janitor::clean_names() %>%
    # change column names to more familiar form
    dplyr::mutate(
      inning = about_inning,
      inning_topbot = about_half_inning,
      inning_topbot = dplyr::case_when(
        about_half_inning == "bottom" ~ "Bottom",
        about_half_inning == "top" ~ "Top",
        TRUE ~ about_half_inning
      ),
      home_score = result_home_score,
      away_score = result_away_score
    ) %>%
    # discard unneccessary columns
    dplyr::select(play_id, home_team, away_team,
    	inning, inning_topbot, home_score, away_score) %>%
    # only want late games
    dplyr::filter(inning >= 8) %>%
    # only want close games
    dplyr::filter(abs(home_score - away_score) <= 1)
	
	print("return statement")

	return(close_game_alerts)
}

write_tweet <- function(game){
  # game: single game sliced from close_games function
  # require:
  #   -game from close_games function

  # get team abbreviations for matchup data
  game_data <- game %>%
    dplyr::left_join(
      dplyr::select(team_hashtags, full_team_name, team_abbr) %>%
        dplyr::distinct(), #  because TOR is on there twice
      by = c("home_team" = "full_team_name")
    ) %>%
    dplyr::rename(home_abbr = team_abbr) %>%
    dplyr::left_join(
      dplyr::select(team_hashtags, full_team_name, team_abbr) %>%
        dplyr::distinct(), #  because TOR is on there twice
      by = c("away_team" = "full_team_name")
    ) %>%
    dplyr::rename(away_abbr = team_abbr)
  
  home_team <- dplyr::pull(game, home_abbr)
  
  away_team <- dplyr::pull(game, away_abbr)
  
  home_score <- dplyr::pull(game, home_score)
  
  away_score <- dplyr::pull(game, away_score)
  
  inning <- game %>%
    dplyr::mutate(inning = dplyr::case_when(
      inning == 1 ~ "1st",
      inning == 2 ~ "2nd",
      inning == 3 ~ "3rd",
      TRUE ~ as.character(glue::glue("{inning}th"))
    )) %>%
    dplyr::pull(inning)

  inning_half <- dplyr::pull(hit, inning_topbot)
      
  hashtag_away <- team_hashtags %>%
    dplyr::filter(full_team_name == dplyr::pull(game, away_team)) %>%
    dplyr::slice(1) %>% #  because toronto is on there twice for 2 stadiums
    dplyr::pull(team_hashtag)

  hashtag_home <- team_hashtags %>%
    dplyr::filter(full_team_name == dplyr::pull(game, home_team)) %>%
    dplyr::slice(1) %>% #  because toronto is on there twice for 2 stadiums
    dplyr::pull(team_hashtag)
  
  inning_emoji <- ifelse(
    inning_half == "Top", "\U0001f53a", "\U0001f53b"
  )
  if (home_score == away_score) {
  	tweet <- glue::glue(
        "Close Game Alert!
        {away_team} vs {home_team}
    #{hashtag_away} #{hashtag_home}
    
    This game is tied in the {inning_emoji} {inning}!
    
    {away_team} ({away_score}) @ {home_team} ({home_score})")
  }
  else {
  	tweet <- glue::glue(
        "Close Game Alert!
        {away_team} vs {home_team}
    #{hashtag_away} #{hashtag_home}
    
    This game is a 1-run game in the {inning_emoji} {inning}!
    
    {away_team} ({away_score}) @ {home_team} ({home_score})")
  }
  
  
  return(tweet)
}

# team abbreviations, hashtags, and stadium details
team_hashtags <- readr::read_csv("/Users/jonnathanbaquero/Downloads/dinger-machine-main/dong-bot/data/team_logos_hashtags.csv",
                                 col_types = readr::cols()) #%>%
  # use Sahlen Field for Blue Jays at the moment
  #mutate(stadium = ifelse(stadium == "Rogers Centre", "Sahlen Field", stadium))

# get plays that have already been tweeted
done_plays <- readRDS("/Users/jonnathanbaquero/Downloads/dinger-machine-main/dong-bot/data/done_plays.rds") %>%
#done_plays <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/dong-bot/data/done_plays.rds")) %>%
  dplyr::mutate(game_date = as.character(game_date))

# today
dates_to_pull <- Sys.Date()

# get games
games <- fetch_live_games(dates_to_pull)
print(length(games))

# get game ids for live games
game_ids <- games %>%
  dplyr::pull(game_pk)

# get twitter authorization token saved in environment
#twitter_token <- rtweet::rtweet_bot(
#  api_key       = Sys.getenv("DONG_TWITTER_API_KEY"),
#  api_secret    = Sys.getenv("DONG_TWITTER_API_SECRET"),
#  access_token  = Sys.getenv("DONG_TWITTER_ACCESS_TOKEN"),
#  access_secret = Sys.getenv("DONG_TWITTER_ACCESS_SECRET")
#)
if(length(game_ids) > 0) {

  # pull all live games
  pbp <- purrr::map_df(.x = game_ids, ~baseballr::get_pbp_mlb(game_pk = .x))
  print(length(game_ids))
  # clean it up
  game_data <- close_games(pbp, games) %>%
    # make sure we haven't tweeted the play already
    dplyr::filter(play_id %not_in% done_plays$play_id)
  
  if(nrow(game_data) > 0) {
  	print("inside if statement #2")
    
    for(i in 1:nrow(game_data)){
      game <- game_data %>% dplyr::slice(i)
      tweet <- write_tweet(game)
      rtweet::post_tweet(
        tweet,
        token = rtweet::rtweet_bot(
          api_key       = Sys.getenv("biYdwTAEZbIQmCJ4BWEnK637F"),
          api_secret    = Sys.getenv("PTQrDBpn5ytXtPbp2gFLToHI3QGn2Em5P0CfVPasBj6l5XJavq"),
          access_token  = Sys.getenv("1645081424699981826-cGiE25MvsmJrBZ1hz3N8HqiGgsMFwJ"),
          access_secret = Sys.getenv("MuLIukV5gEvqkCdtez3CifvCafi5AEbgrR9ChKXIuvuJm")
        )
      )
      tweeted_play <- dplyr::select(
        play_id, game_date, home_team, away_team,
	    	inning, inning_topbot, home_score, away_score
        ) #%>%
        #dplyr::mutate(url = tweet_url)
      done_plays <- dplyr::bind_rows(done_plays, tweeted_play)
      # update the finished plays
      done_plays %>% saveRDS("data/done_plays.rds")
      done_plays %>% readr::write_csv("data/done_plays.csv")
      rm(tweet, game, tweeted_play)
      # wait 30 seconds before tweeting again if there's multiple tweets
      if(nrow(game_data) - i > 0){
        Sys.sleep(30)
      }
    }
  }
} else{
	print("blah")
	tweet <- glue::glue(
        "Close Game Alert!
        Yankees vs Mets
    
    This game is a 1-run game in the 9th inning!")
    print("blah #2")
    response = client.create_tweet(tweet)
    rtweet::post_tweet(
        tweet,
        token = rtweet::rtweet_bot(
          api_key       = "biYdwTAEZbIQmCJ4BWEnK637F",
          api_secret    = "PTQrDBpn5ytXtPbp2gFLToHI3QGn2Em5P0CfVPasBj6l5XJavq",
          access_token  = "1645081424699981826-cGiE25MvsmJrBZ1hz3N8HqiGgsMFwJ",
          access_secret = "MuLIukV5gEvqkCdtez3CifvCafi5AEbgrR9ChKXIuvuJm"
        )
      )
      print("blah #3")
}
