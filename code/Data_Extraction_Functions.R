library(worldfootballR)
library(tidyverse)
library(dplyr)
library(broom)
library(progress)

##### ---- Historic Data ---- #####

# Function to extract the match links - from season 2017/18 to 'end_year'
# Saved Data Path: 'rda/historic_matches_links.rda'
extract_historic_match_links <- function(end_year = NULL){
  ucl_link <- "https://fbref.com/en/comps/8/history/Champions-League-Seasons"
  uel_link <- "https://fbref.com/en/comps/19/history/Europa-League-Seasons"
  
  # Extract the match links
  historic_matches_links <- c(fb_match_urls(country = c("ITA", "ENG", "FRA", "SPA", "GER"),
                                            gender = "M",
                                            season_end_year = c(2017:end_year),
                                            tier = '1st'), 
                              fb_match_urls(country = "ENG",
                                            gender = "M",
                                            season_end_year = c(2019:end_year),
                                            tier = '2nd'),
                              fb_match_urls(country = c("USA", "NED", "POR", "SPA", "GER"),
                                            gender = "M",
                                            season_end_year = c(2019:end_year),
                                            tier = '1st'),
                              fb_match_urls(country = "",
                                            gender = "M",
                                            season_end_year = c(2017:end_year),
                                            non_dom_league_url = ucl_link),
                              fb_match_urls(country = "",
                                            gender = "M",
                                            season_end_year = c(2017:end_year),
                                            non_dom_league_url = uel_link)
  )
  
  # Save the data into the RDA folder
  save(historic_matches_links, file = "rda/historic_matches_links.rda")
}


# Function to extract the historic match reports
# Saved Data Path: 'rda/historic_match_reports.rda'
extract_historic_match_reports <- function(){
  # Get the start time
  start_time <- Sys.time()
  
  historic_match_reports <- c()
  
  # Load the historic_matches_links
  load('rda/historic_matches_links.rda')
  
  # Settle progress bar
  pb <- progress_bar$new(total = length(historic_matches_links))
  
  # Extract the match report for every link in historic_matches_links
  for (match_link in historic_matches_links) {
    tryCatch({
      match_report <- fb_match_report(match_url = match_link)
      historic_match_reports <- bind_rows(historic_match_reports, match_report)
    }, error = function(e) {
      NULL # Do nothing if an error occurs
    })
    pb$tick() # For progress bar
  }
  
  # Save the data into the RDA folder
  save(historic_match_reports, file = "rda/historic_match_reports.rda")
  
  # Get the end time
  end_time <- Sys.time()
  
  # Calculate the running time
  running_time <- difftime(end_time, start_time, units = "secs")
  
  # Print the time data
  print(paste("Start time: ", start_time))
  print(paste("End time: ", end_time))
  print(paste("Running time: ", round(running_time, 2), "secs", "(", round(running_time/60, 2), "mins)"))
}


# Function to extract the historic match logs of specified stats
# Saved Data Path: 'rda/*.rda' (* = "historic_STAT_TEAM_PLAYER", depends on the input)
extract_historic_stat_match_logs <- function(stat = NULL, team_player = NULL){
  # Get the start time
  start_time <- Sys.time()
  
  # Load the historic_matches_links
  load('rda/historic_matches_links.rda')
  
  stats_data <- c()
  
  # Set progress bar
  pb <- progress_bar$new(total = length(historic_matches_links))
  
  # For loop to xtract the data of the specified stat and team/player for all the historic matches
  for (match_link in historic_matches_links) {
    # Use tryCatch to catch errors when extracting the match report
    tryCatch({
    match_stats <- fb_advanced_match_stats(match_url = historic_matches_links,
                                           stat_type = stat,
                                           team_or_player = team_player)
    stats_data <- bind_rows(stats_data, match_stats)
    }, error = function(e) {
      NULL # Do nothing if an error occurs
    })
    pb$tick() # For progress bar
  }
  
  # Custom variable name
  var_name <- paste0("historic_", stat, "_", team_player)
  assign(var_name, stats_data)
  
  # Save the data into the RDA folder
  save(list = var_name, file = file.path("rda", paste0(var_name, ".rda")))
  
  # Get the end time
  end_time <- Sys.time()
  
  # Calculate the running time
  running_time <- difftime(end_time, start_time, units = "secs")
  
  # Print the time data
  print(paste("Start time: ", start_time))
  print(paste("End time: ", end_time))
  print(paste("Running time: ", round(running_time, 2), "secs", "(", round(running_time/60, 2), "mins)"))
}





##### ---- Current Season's Data ---- #####

# Function to extract the current season's match links
# Returned Data: curr_szn_matches_links
extract_szn_match_links <- function(szn = NULL){
  ucl_link <- "https://fbref.com/en/comps/8/history/Champions-League-Seasons"
  uel_link <- "https://fbref.com/en/comps/19/history/Europa-League-Seasons"
  
  # Extract the match links
  curr_szn_matches_links <- c(fb_match_urls(country = c("ITA", "ENG", "FRA", "SPA", "GER"),
                                            gender = "M",
                                            season_end_year = szn,
                                            tier = '1st'), 
                              fb_match_urls(country = "ENG",
                                            gender = "M",
                                            season_end_year = szn,
                                            tier = '2nd'),
                              fb_match_urls(country = c("USA", "NED", "POR", "SPA", "GER"),
                                            gender = "M",
                                            season_end_year = szn,
                                            tier = '1st'),
                              fb_match_urls(country = "",
                                            gender = "M",
                                            season_end_year = szn,
                                            non_dom_league_url = ucl_link),
                              fb_match_urls(country = "",
                                            gender = "M",
                                            season_end_year = szn,
                                            non_dom_league_url = uel_link)
  )
  
  # Return the match links
  return(curr_szn_matches_links)
}


# Function to extract the current season's match reports
# Returned Data: curr_szn_match_reports
extract_szn_match_reports <- function(szn = NULL) {
  # Get the start time
  start_time <- Sys.time()
  
  # Run the function to get the current season's match links
  curr_szn_matches_links <- extract_szn_match_links(szn)
  
  curr_szn_match_reports <- c()
  
  # Set progress bar
  pb <- progress_bar$new(total = length(curr_szn_matches_links))
  
  # Extract the match report for every link in curr_szn_matches_links
  for (match_link in curr_szn_matches_links) {
    
    # Use tryCatch to catch errors when extracting the match report
    tryCatch({
      match_report <- fb_match_report(match_url = match_link)
      curr_szn_match_reports <- bind_rows(curr_szn_match_reports, match_report)
    }, error = function(e) {
      NULL # Do nothing if an error occurs
    })
    
    pb$tick() # For progress bar
  }
  
  # Get the end time
  end_time <- Sys.time()
  
  # Calculate the running time
  running_time <- difftime(end_time, start_time, units = "secs")
  
  # Print the time data
  print(paste("Start time: ", start_time))
  print(paste("End time: ", end_time))
  print(paste("Running time: ", round(running_time, 2), "secs", "(", round(running_time/60, 2), "mins)"))

  # Return the match reports
  return(curr_szn_match_reports)
}


# Function to extract the historic match logs of specified stats
# Saved Data Path: 'rda/*.rda' (* = "historic_STAT_TEAM_PLAYER", depends on the input)
extract_historic_stat_match_logs <- function(stat = NULL, team_player = NULL){
  # Get the start time
  start_time <- Sys.time()
  
  # Run the function to get the current season's match links
  curr_szn_matches_links <- extract_szn_match_links(szn)
  
  stats_data <- c()
  
  # Set progress bar
  pb <- progress_bar$new(total = length(curr_szn_matches_links))
  
  # For loop to xtract the data of the specified stat and team/player for all the historic matches
  for (match_link in curr_szn_matches_links) {
    # Use tryCatch to catch errors when extracting the match report
    tryCatch({
      match_stats <- fb_advanced_match_stats(match_url = curr_szn_matches_links,
                                             stat_type = stat,
                                             team_or_player = team_player)
      stats_data <- bind_rows(stats_data, match_stats)
    }, error = function(e) {
      NULL # Do nothing if an error occurs
    })
    pb$tick() # For progress bar
  }
  
  # Custom variable name
  var_name <- paste0("historic_", stat, "_", team_player)
  assign(var_name, stats_data)
  
  # Get the end time
  end_time <- Sys.time()
  
  # Calculate the running time
  running_time <- difftime(end_time, start_time, units = "secs")
  
  # Print the time data
  print(paste("Start time: ", start_time))
  print(paste("End time: ", end_time))
  print(paste("Running time: ", round(running_time, 2), "secs", "(", round(running_time/60, 2), "mins)"))
  
  # Return the stat match logs
  return(get(var_name))
}



#####



curr_szn_match_reports <- extract_szn_match_reports(2023)
