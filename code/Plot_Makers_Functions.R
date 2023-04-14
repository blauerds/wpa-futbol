# Function to generate a shot locations plot with Fotmob data
# Saved Image Path: 'figs/shots_plot.png'
shot_location_plot_maker <- function(matchID = NULL, team_or_player = NULL, team_name_fotmob = NULL, player_id = NULL){
  # Load libraries
  library(dplyr)
  library(worldfootballR)
  library(tidyverse)
  library(ggplot2)
  library(png)
  library(grid)
  library(magick)
  library(ggimage)
  
  # Check the arguments put by the user
  allowed_team_or_player <- c("team", "player")
  if (!team_or_player %in% allowed_team_or_player) {
    stop(sprintf("The argument 'team_or_player' must be one of the following values: %s",
                 paste(allowed_team_or_player, collapse = ", ")))
  } else if (is.null(matchID)) {
    stop(sprintf("The argument 'matchID' must be given"))
  } else if (is.null(team_name_fotmob) & is.null(player_id)) {
    stop(sprintf("One of the arguments 'player_id' or 'team_name_fotmob' must be given"))
  }
  
  # Load the pitch background image
  pitch <- readPNG("figs/pitch_background.png")
  
  # Extract the fotmob match data from the id given
  match_data <- worldfootballR::fotmob_get_match_players(matchID)
  
  # Extract the match data based on the user's input
  if (is.null(team_name_fotmob)) {
    # Filter by a specific player's ID
    filtered_match_data <- players %>% filter(id == as.character(player_id))
  } else if (is.null(player_id)) {
    # Filter by a specific team
    filtered_match_data <- players %>% filter(team_name == as.character(team_name_fotmob))
  }
  
  # Select the shotmap data
  salah_shotmap <- filtered_match_data %>% 
    select(player_id = id, shotmap) %>% 
    unnest(shotmap)
  
  # Stop the function if there's no shotmap data avalable for that match
  if(length(colnames(salah_shotmap)) <= 2) {
    stop("There´s no shotmap data available for that match")
  }
  
  # Add a column for the outcomes
  salah_shotmap <- salah_shotmap %>% mutate(outcome = case_when(
    eventType == 'Goal' ~ 'Goal',
    eventType == 'Miss' ~ 'Miss',
    eventType == 'AttemptSaved' & isBlocked == FALSE ~ 'Saved',
    eventType == 'AttemptSaved' & isBlocked == TRUE ~ 'Blocked'
  )) %>% mutate(outcome2 = outcome)
  
  # Set the custom colors and transparency values
  group.colors <- c("Goal" = "#316A34", "Miss" = "#AB2A34",
                    "Saved" = "#FFFFFF", "Blocked" = "#FFFFFF")
  group.alpha <- c("Goal" = 0.9, "Miss" = 0.5,
                   "Saved" = 0.5, "Blocked" = 0.5)
  group.shapes <- c("Goal" = 1, "Miss" = 1,
                    "Saved" = 1, "Blocked" = 1)
  
  # Create the shotmap plot
  plt <- ggplot(salah_shotmap, aes(x = x, y = 68 - y, color = outcome, alpha = outcome)) +
    annotation_custom(rasterGrob(pitch), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    geom_point(data = salah_shotmap, aes(x = x, y = 68 - y), size = 4) +
    xlim(73, 104.50) +
    ylim(0, 68.50) +
    theme_bw() +
    coord_flip() +
    theme(panel.grid = element_blank(), 
          axis.text = element_blank(), 
          axis.title = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none") +
    labs(x = "", y = "") +
    scale_color_manual(values = group.colors, name = "outcome") +
    scale_alpha_manual(values = group.alpha) +
    geom_point(data = salah_shotmap %>% filter(!outcome %in% c("Goal", "Miss")),
               aes(x = x, y = 68 - y,
                   shape = outcome,
                   stroke = 1),
               size = 4, alpha = 0.9, color= "#FFFFFF") +
    scale_shape_manual(values = group.shapes) +
    geom_point(data = salah_shotmap %>% filter(outcome == "Goal"),
               aes(x = x, y = 68 - y,
                   shape = outcome,
                   stroke = 1),
               size = 4, alpha = 0.9, color= "#51A956") +
    geom_point(data = salah_shotmap %>% filter(outcome == "Miss"),
               aes(x = x, y = 68 - y,
                   shape = outcome,
                   stroke = 1),
               size = 4, alpha = 0.9, color= "#EA3B49")
  
  # Save the plot as a PNG image file
  ggsave("figs/shots_plot.png", plt, units = "cm", width = 18.1, height = 8.71, dpi = 1500)
  
  # Read in the original image
  img <- image_read("figs/shots_plot.png")
  
  # Crop the image
  shots_plot <- image_crop(test_img, "9188x4855+779+116")
  
  # Save the cropped image
  image_write(shots_plot, "figs/shots_plot.png")
}


# Function to generate a percentile plot, with LMI logo added
# Saved Image Path: 'figs/plot_NO_LOGO.png' (without logo) or 'figs/plot_ready.png' (with LMI) logo
percentile_plot_creator <- function(playerlink = NULL, lang = "ENG", curr_szn = 2023){
  # Load libraries
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(cowplot)
  library(magick)
  library(worldfootballR)
  library(stringr)
  library(googledrive)
  library(showtext)
  library(roxygen2)
  library(lubridate)
  library(raster)
  library(grid)
  library(png)
  library(ggimage)
  library(lubridate)
  font_add_google(name = "Barlow", family = "barlow")
  showtext_auto()
  
  # check if the mandatory argument (playerlink) has been passed in
  if (is.null(playerlink)) {
    stop("The argument 'playerlink' is mandatory and must be provided")
  }
  
  # check if the restricted argument (lang) is in the allowed languages
  allowed_languages <- c("ENG", "ESP")
  if (!lang %in% allowed_languages) {
    stop(sprintf("The argument 'lang' must be one of the following values: %s", paste(allowed_languages, collapse = ", ")))
  }
  
  # check if the numeric argument (curr_szn) is a number
  if (!is.numeric(curr_szn)) {
    stop("The argument 'curr_szn' must be a number")
  }
  
  # execute the percentile plot maker function in the desired language (English (ENG) by default)
  try({
    if (lang == "ENG") {
      # data for later
      stats_percentiles <- c("Non-Penalty Goals", "Non-Penalty xG", "Shots on target", "Goals/Shot",
                             "xAG", "Passes Completed", "Pass Completion % (Short)", "Pass Completion % (Medium)",
                             "Pass Completion % (Long)", "Shot-Creating Actions", "Tackles", "Blocks", "Interceptions",
                             "Successful Take-Ons", "Progressive Carries", "Progressive Passes Rec", "Progressive Passes",
                             "Aerials Won", "Yellow Cards", "Fouls Committed", "Fouls Drawn")
      stats_GK_percentiles <- c("PSxG-GA", "Save Percentage", "Clean Sheet Percentage", "Save% (Penalty Kicks)",
                                "Pass Completion Percentage", "Launch %", "Crosses Stopped %", "Def. Actions Outside Pen. Area",
                                "Avg. Distance of Def. Actions", "PSxG/SoT", "Free Kick Goals Against", "Crosses Faced", "Goals Against",
                                "Avg. Length of Goal Kicks", "Goal Kicks", "Launch% (Goal Kicks)", "Penalty Kicks Attempted")
      current_season_end_year <- curr_szn
      
      # get player's scouting report
      player_scouting_report <- fb_player_scouting_report(player_url = as.character(playerlink), pos_versus = "primary", league_comp_name = "Last 365 Days Men's Big 5 Leagues, UCL, UEL")
      
      
      # different stats selection for field players vs goalkeepers
      if (player_scouting_report$Versus[1] != "Goalkeepers") {
        # filter to show the report data of the statistics we want, and remove duplicated stats
        player_percentiles_data <- player_scouting_report %>%
          filter(Statistic %in% stats_percentiles) %>%
          drop_na()
        player_percentiles_data <- player_percentiles_data[!duplicated(player_percentiles_data$Statistic), ]
        
        # change statistics names (to make the plot more readable)
        for (row in seq(1:nrow(player_percentiles_data))) {
          if (player_percentiles_data$Statistic[row] == "Non-Penalty Goals") {
            player_percentiles_data$Statistic[row] <- "np:Gls"
            player_percentiles_data$StatGroup[row] <- "Shooting"
          } else if (player_percentiles_data$Statistic[row] == "Non-Penalty xG") {
            player_percentiles_data$Statistic[row] <- "np:xG"
            player_percentiles_data$StatGroup[row] <- "Shooting"
          } else if (player_percentiles_data$Statistic[row] == "Shots on target") {
            player_percentiles_data$Statistic[row] <- "SoT"
            player_percentiles_data$StatGroup[row] <- "Shooting"
          } else if (player_percentiles_data$Statistic[row] == "Goals/Shot") {
            player_percentiles_data$Statistic[row] <- "Gls/Sh"
            player_percentiles_data$StatGroup[row] <- "Shooting"
          } else if (player_percentiles_data$Statistic[row] == "xAG") {
            player_percentiles_data$Statistic[row] <- "xAG"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Passes Completed") {
            player_percentiles_data$Statistic[row] <- "Cmp"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Short)") {
            player_percentiles_data$Statistic[row] <- "Short %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Medium)") {
            player_percentiles_data$Statistic[row] <- "Med. %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Long)") {
            player_percentiles_data$Statistic[row] <- "Long %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Shot-Creating Actions") {
            player_percentiles_data$Statistic[row] <- "SCA"
            player_percentiles_data$StatGroup[row] <- "Possession"
          } else if (player_percentiles_data$Statistic[row] == "Tackles") {
            player_percentiles_data$Statistic[row] <- "Tkl"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Blocks") {
            player_percentiles_data$Statistic[row] <- "Blocks"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Interceptions") {
            player_percentiles_data$Statistic[row] <- "Int"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Successful Take-Ons") {
            player_percentiles_data$Statistic[row] <- "Drib Cmp"
            player_percentiles_data$StatGroup[row] <- "Possession"
          } else if (player_percentiles_data$Statistic[row] == "Progressive Carries") {
            player_percentiles_data$Statistic[row] <- "Prog Carries"
            player_percentiles_data$StatGroup[row] <- "Possession"
          } else if (player_percentiles_data$Statistic[row] == "Progressive Passes") {
            player_percentiles_data$Statistic[row] <- "Prog Passes"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Progressive Passes Rec") {
            player_percentiles_data$Statistic[row] <- "Prog Rec"
            player_percentiles_data$StatGroup[row] <- "Possession"
          } else if (player_percentiles_data$Statistic[row] == "Aerials Won") {
            player_percentiles_data$Statistic[row] <- "Aerials Won"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Yellow Cards") {
            player_percentiles_data$Statistic[row] <- "Yellows"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Fouls Committed") {
            player_percentiles_data$Statistic[row] <- "Fouls"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Fouls Drawn") {
            player_percentiles_data$Statistic[row] <- "Fouls Created"
            player_percentiles_data$StatGroup[row] <- "Possession"
          }
        }
        
        # order by stat group
        player_percentiles_data <- player_percentiles_data[order(player_percentiles_data$StatGroup), ]
        
        # add id for each stat group and current season
        player_percentiles_data <- player_percentiles_data %>% mutate(id = case_when(
          StatGroup == "Defense" ~ 1,
          StatGroup == "Passing" ~ 2,
          StatGroup == "Possession" ~ 3,
          StatGroup == "Shooting" ~ 4
        ),
        season = paste(current_season_end_year-1, "/", current_season_end_year, sep=""))
        
        # group colors for the plot
        group.colors <- c("Defense" = "#C86742", "Passing" = "#84B86F",
                          "Possession" = "#E3AE2E", "Shooting" = "#6F78B3")
        
      } else if (player_scouting_report$Versus[1] == "Goalkeepers") {
        # filter to show the report data of the statistics we want, and remove duplicated stats
        player_percentiles_data <- player_scouting_report %>%
          filter(Statistic %in% stats_GK_percentiles) %>%
          drop_na()
        player_percentiles_data <- player_percentiles_data[!duplicated(player_percentiles_data$Statistic), ]
        
        # change statistics names (to make the plot more readable)
        for (row in seq(1:nrow(player_percentiles_data))) {
          if (player_percentiles_data$Statistic[row] == "PSxG-GA") {
            player_percentiles_data$Statistic[row] <- "PSxG-GA"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Save Percentage") {
            player_percentiles_data$Statistic[row] <- "Save %"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Clean Sheet Percentage") {
            player_percentiles_data$Statistic[row] <- "Clean Sheets %"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Save% (Penalty Kicks)") {
            player_percentiles_data$Statistic[row] <- "Penalty %"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          } else if (player_percentiles_data$Statistic[row] == "Pass Completion Percentage") {
            player_percentiles_data$Statistic[row] <- "Cmp %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Launch %") {
            player_percentiles_data$Statistic[row] <- "Launch %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Crosses Stopped %") {
            player_percentiles_data$Statistic[row] <- "Crosses Stp %"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          } else if (player_percentiles_data$Statistic[row] == "Def. Actions Outside Pen. Area") {
            player_percentiles_data$Statistic[row] <- "DOA"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Avg. Distance of Def. Actions") {
            player_percentiles_data$Statistic[row] <- "Dist DOA"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "PSxG/SoT") {
            player_percentiles_data$Statistic[row] <- "PSxG/SoT"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Free Kick Goals Against") {
            player_percentiles_data$Statistic[row] <- "FK Goals"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          } else if (player_percentiles_data$Statistic[row] == "Crosses Faced") {
            player_percentiles_data$Statistic[row] <- "Crosses Faced"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          } else if (player_percentiles_data$Statistic[row] == "Goals Against") {
            player_percentiles_data$Statistic[row] <- "Gls Against"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Avg. Length of Goal Kicks") {
            player_percentiles_data$Statistic[row] <- "Length Goal Kicks"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Goal Kicks") {
            player_percentiles_data$Statistic[row] <- "Goal Kicks"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Launch% (Goal Kicks)") {
            player_percentiles_data$Statistic[row] <- "Goal Kicks %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Penalty Kicks Attempted") {
            player_percentiles_data$Statistic[row] <- "PK Against"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          } 
        }
        
        
        # order by stat group
        player_percentiles_data <- player_percentiles_data[order(player_percentiles_data$StatGroup), ]
        
        # add id for each stat group and current season
        player_percentiles_data <- player_percentiles_data %>% mutate(id = case_when(
          StatGroup == "Defense" ~ 1,
          StatGroup == "Passing" ~ 2,
          StatGroup == "Goalkeeping" ~ 3,
          StatGroup == "Set Piece" ~ 4
        ),
        season = paste(current_season_end_year-1, "/", current_season_end_year, sep=""))
        
        # group colors for the plot
        group.colors <- c("Defense" = "#C86742", "Passing" = "#84B86F",
                          "Goalkeeping" = "#6F78B3", "Set Piece" = "#E3AE2E")
      }
      
      
      ###### PLOT #####
      # get player's name and versus for the plot data
      playername <- player_percentiles_data$Player[1]
      playerversus <- player_percentiles_data$Versus[1]
      
      # create the plot
      plt <- ggplot(player_percentiles_data) +
        # make custom panel grid
        geom_hline(
          aes(yintercept = y),
          data.frame(y = c(25, 50, 75, 100)),
          color = "lightgrey", linetype = "dotted"
        ) +
        
        # add bars to represent the percentiles
        geom_col(
          aes(x = reorder(str_wrap(Statistic, 5), id), y = Percentile, fill = StatGroup
          ),
          position = "dodge2", show.legend = TRUE, alpha = 1
        ) +
        
        # lollipop shaf for guidance
        geom_segment(
          aes(x = reorder(str_wrap(Statistic, 5), id), y = 0,
              xend = reorder(str_wrap(Statistic, 5), id), yend = 100),
          color = "black",
          linetype = "dotted"
        ) +
        
        # make it circular
        coord_polar() +
        
        # scale y axis so bars don't start at the center
        scale_y_continuous(
          limits = c(-50, 100)
        ) +
        
        theme(
          # remove axis ticks and text
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          
          # use gray text for the labels
          axis.text.x = element_text(color = "gray12", size = 35, lineheight = 0.3,
                                     hjust = 20),
          
          # move the legend to the bottom and make it horizontal
          legend.direction = "horizontal",
          legend.position = "bottom"
        ) +
        
        # add labels
        labs(
          title = (paste("\n", playername, paste("(",player_percentiles_data$season[1], ")", sep = ""))),
          subtitle = str_wrap(paste("\n", "Statistics comapred to ", playerversus,
                                    " in Men's big 5 leagues, UCL y UEL in the last 365 days", sep = ""), 70),
          caption = "\n @blauerds - @LaMediaInglesa\n https://github.com/blauerds\nSource: FBref"
        ) +
        
        # add labels with the percentile values
        geom_label(aes(x = reorder(str_wrap(Statistic, 5), id), y = Percentile, color = StatGroup),
                   label = player_percentiles_data$Percentile, size = 10,
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        
        # customize general theme
        theme(
          # customize the text in the title, subtitle, and caption
          text = element_text(color = "grey12", family = "barlow",size = 35),
          plot.title = element_text(face = "bold", size = 50, hjust = 0.5, lineheight = 0.3),
          plot.subtitle = element_text(size = 35, hjust = 0.5, lineheight = 0.4),
          plot.caption = element_text(size = 29, hjust = 0.5, lineheight = 0.4),
          
          # make the background white and remove extra grid lines
          panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          
          legend.box.just = "center"
        ) +
        
        # specify group colors manually
        scale_fill_manual(values = group.colors, name = NULL) +
        scale_colour_manual(values = group.colors, name = NULL)
      
      # save the plot
      ggsave("figs\\plot_NO_LOGO.png", plt)
      
      
      
      # get the IMAGE of the plot and the logo
      final_plot <- image_read("figs\\plot_NO_LOGO.png")
      LMI_logo <- image_read("figs\\la_media_inglesa.jpg") %>% image_resize("300x300")
      
      # calculate the offset (the position of the logo inside the plot)
      # the 0.01 and 0.99 are to put a 1% and 99% padding (respectively)
      height_off <- image_info(final_plot)$height - image_info(LMI_logo)$height -
        image_info(final_plot)$height * 0.01
      width_off <- (image_info(final_plot)$width - image_info(LMI_logo)$width) * 0.99
      offset_image <- paste("+", width_off,"+", height_off, sep = "")
      
      # plot + logo
      plot_logoed <- final_plot %>% image_composite(LMI_logo, offset = offset_image)
      
      # save the plot after the logo was added
      image_write(plot_logoed, path = "figs\\plot_ready.png", format = "png")
      
    } else if (lang == "ESP") {
      # data for later
      stats_percentiles <- c("Non-Penalty Goals", "Non-Penalty xG", "Shots on target", "Goals/Shot",
                             "xAG", "Passes Completed", "Pass Completion % (Short)", "Pass Completion % (Medium)",
                             "Pass Completion % (Long)", "Shot-Creating Actions", "Tackles", "Blocks", "Interceptions",
                             "Successful Take-Ons", "Progressive Carries", "Progressive Passes Rec", "Progressive Passes",
                             "Aerials Won", "Yellow Cards", "Fouls Committed", "Fouls Drawn")
      stats_GK_percentiles <- c("PSxG-GA", "Save Percentage", "Clean Sheet Percentage", "Save% (Penalty Kicks)",
                                "Pass Completion Percentage", "Launch %", "Crosses Stopped %", "Def. Actions Outside Pen. Area",
                                "Avg. Distance of Def. Actions", "PSxG/SoT", "Free Kick Goals Against", "Crosses Faced", "Goals Against",
                                "Avg. Length of Goal Kicks", "Goal Kicks", "Launch% (Goal Kicks)", "Penalty Kicks Attempted")
      current_season_end_year <- curr_szn
      
      # get player's scouting report
      player_scouting_report <- fb_player_scouting_report(player_url = as.character(playerlink), pos_versus = "primary", league_comp_name = "Last 365 Days Men's Big 5 Leagues, UCL, UEL")
      
      
      # different stats selection for field players vs goalkeepers
      if (player_scouting_report$Versus[1] != "Goalkeepers") {
        # filter to show the report data of the statistics we want, and remove duplicated stats
        player_percentiles_data <- player_scouting_report %>%
          filter(Statistic %in% stats_percentiles) %>%
          drop_na()
        player_percentiles_data <- player_percentiles_data[!duplicated(player_percentiles_data$Statistic), ]
        
        # change statistics names (to make the plot more readable)
        for (row in seq(1:nrow(player_percentiles_data))) {
          if (player_percentiles_data$Statistic[row] == "Non-Penalty Goals") {
            player_percentiles_data$Statistic[row] <- "np:Gls"
            player_percentiles_data$StatGroup[row] <- "Shooting"
          } else if (player_percentiles_data$Statistic[row] == "Non-Penalty xG") {
            player_percentiles_data$Statistic[row] <- "np:xG"
            player_percentiles_data$StatGroup[row] <- "Shooting"
          } else if (player_percentiles_data$Statistic[row] == "Shots on target") {
            player_percentiles_data$Statistic[row] <- "SoT"
            player_percentiles_data$StatGroup[row] <- "Shooting"
          } else if (player_percentiles_data$Statistic[row] == "Goals/Shot") {
            player_percentiles_data$Statistic[row] <- "Gls/Sh"
            player_percentiles_data$StatGroup[row] <- "Shooting"
          } else if (player_percentiles_data$Statistic[row] == "xAG") {
            player_percentiles_data$Statistic[row] <- "xAG"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Passes Completed") {
            player_percentiles_data$Statistic[row] <- "Cmp"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Short)") {
            player_percentiles_data$Statistic[row] <- "Short %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Medium)") {
            player_percentiles_data$Statistic[row] <- "Med. %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Long)") {
            player_percentiles_data$Statistic[row] <- "Long %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Shot-Creating Actions") {
            player_percentiles_data$Statistic[row] <- "SCA"
            player_percentiles_data$StatGroup[row] <- "Possession"
          } else if (player_percentiles_data$Statistic[row] == "Tackles") {
            player_percentiles_data$Statistic[row] <- "Tkl"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Blocks") {
            player_percentiles_data$Statistic[row] <- "Blocks"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Interceptions") {
            player_percentiles_data$Statistic[row] <- "Int"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Successful Take-Ons") {
            player_percentiles_data$Statistic[row] <- "Drib Cmp"
            player_percentiles_data$StatGroup[row] <- "Possession"
          } else if (player_percentiles_data$Statistic[row] == "Progressive Carries") {
            player_percentiles_data$Statistic[row] <- "Prog Carries"
            player_percentiles_data$StatGroup[row] <- "Possession"
          } else if (player_percentiles_data$Statistic[row] == "Progressive Passes") {
            player_percentiles_data$Statistic[row] <- "Prog Passes"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Progressive Passes Rec") {
            player_percentiles_data$Statistic[row] <- "Prog Rec"
            player_percentiles_data$StatGroup[row] <- "Possession"
          } else if (player_percentiles_data$Statistic[row] == "Aerials Won") {
            player_percentiles_data$Statistic[row] <- "Aerials Won"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Yellow Cards") {
            player_percentiles_data$Statistic[row] <- "Yellows"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Fouls Committed") {
            player_percentiles_data$Statistic[row] <- "Fls"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Fouls Drawn") {
            player_percentiles_data$Statistic[row] <- "Fls Created"
            player_percentiles_data$StatGroup[row] <- "Possession"
          }
        }
        
        # order by stat group
        player_percentiles_data <- player_percentiles_data[order(player_percentiles_data$StatGroup), ]
        
        # add id for each stat group and current season
        player_percentiles_data <- player_percentiles_data %>% mutate(id = case_when(
          StatGroup == "Defense" ~ 1,
          StatGroup == "Passing" ~ 2,
          StatGroup == "Possession" ~ 3,
          StatGroup == "Shooting" ~ 4
        ),
        season = paste(current_season_end_year-1, "/", current_season_end_year, sep=""),
        StatGroup = case_when(
          StatGroup == "Defense" ~ "Defensa",
          StatGroup == "Passing" ~ "Pases",
          StatGroup == "Possession" ~ "Posesión",
          StatGroup == "Shooting" ~ "Tiros"
        ))
        
        # group colors for the plot
        group.colors <- c("Defensa" = "#C86742", "Pases" = "#84B86F",
                          "Posesión" = "#E3AE2E", "Tiros" = "#6F78B3")
        
      } else if (player_scouting_report$Versus[1] == "Goalkeepers") {
        # filter to show the report data of the statistics we want, and remove duplicated stats
        player_percentiles_data <- player_scouting_report %>%
          filter(Statistic %in% stats_GK_percentiles) %>%
          drop_na()
        player_percentiles_data <- player_percentiles_data[!duplicated(player_percentiles_data$Statistic), ]
        
        # change statistics names (to make the plot more readable)
        for (row in seq(1:nrow(player_percentiles_data))) {
          if (player_percentiles_data$Statistic[row] == "PSxG-GA") {
            player_percentiles_data$Statistic[row] <- "PSxG-GA"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Save Percentage") {
            player_percentiles_data$Statistic[row] <- "Save %"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Clean Sheet Percentage") {
            player_percentiles_data$Statistic[row] <- "Clean Sheets %"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Save% (Penalty Kicks)") {
            player_percentiles_data$Statistic[row] <- "Penalty %"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          } else if (player_percentiles_data$Statistic[row] == "Pass Completion Percentage") {
            player_percentiles_data$Statistic[row] <- "Cmp %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Launch %") {
            player_percentiles_data$Statistic[row] <- "Launch %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Crosses Stopped %") {
            player_percentiles_data$Statistic[row] <- "Crosses Stp %"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Def. Actions Outside Pen. Area") {
            player_percentiles_data$Statistic[row] <- "DOA"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "Avg. Distance of Def. Actions") {
            player_percentiles_data$Statistic[row] <- "Dist DOA"
            player_percentiles_data$StatGroup[row] <- "Defense"
          } else if (player_percentiles_data$Statistic[row] == "PSxG/SoT") {
            player_percentiles_data$Statistic[row] <- "PSxG/SoT"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Free Kick Goals Against") {
            player_percentiles_data$Statistic[row] <- "FK Goals"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          } else if (player_percentiles_data$Statistic[row] == "Crosses Faced") {
            player_percentiles_data$Statistic[row] <- "Crosses Faced"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          } else if (player_percentiles_data$Statistic[row] == "Goals Against") {
            player_percentiles_data$Statistic[row] <- "Gls Against"
            player_percentiles_data$StatGroup[row] <- "Goalkeeping"
          } else if (player_percentiles_data$Statistic[row] == "Avg. Length of Goal Kicks") {
            player_percentiles_data$Statistic[row] <- "Length Goal Kicks"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Goal Kicks") {
            player_percentiles_data$Statistic[row] <- "Goal Kicks"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Launch% (Goal Kicks)") {
            player_percentiles_data$Statistic[row] <- "Goal Kicks %"
            player_percentiles_data$StatGroup[row] <- "Passing"
          } else if (player_percentiles_data$Statistic[row] == "Penalty Kicks Attempted") {
            player_percentiles_data$Statistic[row] <- "PK Against"
            player_percentiles_data$StatGroup[row] <- "Set Piece"
          }
        }
        
        # order by stat group
        player_percentiles_data <- player_percentiles_data[order(player_percentiles_data$StatGroup), ]
        
        # add id for each stat group and current season
        player_percentiles_data <- player_percentiles_data %>% mutate(id = case_when(
          StatGroup == "Defense" ~ 1,
          StatGroup == "Passing" ~ 2,
          StatGroup == "Goalkeeping" ~ 3,
          StatGroup == "Set Piece" ~ 4
        ),
        season = paste(current_season_end_year-1, "/", current_season_end_year, sep=""),
        StatGroup = case_when(
          StatGroup == "Defense" ~ "Defensa",
          StatGroup == "Passing" ~ "Pases",
          StatGroup == "Goalkeeping" ~ "Portero",
          StatGroup == "Set Piece" ~ "Balón Parado"
        ))
        
        # group colors for the plot
        group.colors <- c("Defensa" = "#C86742", "Pases" = "#84B86F",
                          "Portero" = "#6F78B3", "Balón Parado" = "#E3AE2E")
      }
      
      
      ###### PLOT #####
      # get player's name and versus for the plot data
      playername <- player_percentiles_data$Player[1]
      playerversus <- player_percentiles_data$Versus[1]
      if (playerversus == "Forwards") {
        playerversus <- "Delanteros"
      } else if (playerversus == "Att Mid / Wingers") {
        playerversus <- "Med. Ofensivos / Extremos"
      } else if (playerversus == "Midfielders") {
        playerversus <- "Mediocampistas"
      } else if (playerversus == "Center Backs") {
        playerversus <- "Defensas Centrales"
      } else if (playerversus == "Fullbacks") {
        playerversus <- "Laterales"
      } else if (playerversus == "Goalkeepers") {
        playerversus <- "Porteros"
      }
      
      
      # create the plot
      plt <- ggplot(player_percentiles_data) +
        # make custom panel grid
        geom_hline(
          aes(yintercept = y),
          data.frame(y = c(25, 50, 75, 100)),
          color = "lightgrey", linetype = "dotted"
        ) +
        
        # add bars to represent the percentiles
        geom_col(
          aes(x = reorder(str_wrap(Statistic, 5), id), y = Percentile, fill = StatGroup
          ),
          position = "dodge2", show.legend = TRUE, alpha = 1
        ) +
        
        # lollipop shaf for guidance
        geom_segment(
          aes(x = reorder(str_wrap(Statistic, 5), id), y = 0,
              xend = reorder(str_wrap(Statistic, 5), id), yend = 100),
          color = "black",
          linetype = "dotted"
        ) +
        
        # make it circular
        coord_polar() +
        
        # scale y axis so bars don't start at the center
        scale_y_continuous(
          limits = c(-50, 100)
        ) +
        
        theme(
          # remove axis ticks and text
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          
          # use gray text for the labels
          axis.text.x = element_text(color = "gray12", size = 35, lineheight = 0.3,
                                     hjust = 20),
          
          # move the legend to the bottom and make it horizontal
          legend.direction = "horizontal",
          legend.position = "bottom"
        ) +
        
        # add labels
        labs(
          title = (paste("\n", playername, paste("(",player_percentiles_data$season[1], ")", sep = ""))),
          subtitle = str_wrap(paste("\n", "Estadísticas comparadas con ", playerversus,
                                    " de las 5 grandes ligas europeas, UCL y UEL en los últimos 365 días", sep = ""), 70),
          caption = "\n @blauerds - @LaMediaInglesa\n https://github.com/blauerds\nSource: FBref"
        ) +
        
        # add labels with the percentile values
        geom_label(aes(x = reorder(str_wrap(Statistic, 5), id), y = Percentile, color = StatGroup),
                   label = player_percentiles_data$Percentile, size = 10,
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        
        # customize general theme
        theme(
          # customize the text in the title, subtitle, and caption
          text = element_text(color = "grey12", family = "barlow",size = 35),
          plot.title = element_text(face = "bold", size = 50, hjust = 0.5, lineheight = 0.3),
          plot.subtitle = element_text(size = 35, hjust = 0.5, lineheight = 0.4),
          plot.caption = element_text(size = 29, hjust = 0.5, lineheight = 0.4),
          
          # make the background white and remove extra grid lines
          panel.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_blank(),
          
          legend.box.just = "center"
        ) +
        
        # specify group colors manually
        scale_fill_manual(values = group.colors, name = NULL) +
        scale_colour_manual(values = group.colors, name = NULL)
      
      # save the plot
      ggsave("figs\\plot_NO_LOGO.png", plt)
      
      
      
      # get the IMAGE of the plot and the logo
      final_plot <- image_read("figs\\plot_NO_LOGO.png")
      LMI_logo <- image_read("figs\\la_media_inglesa.jpg") %>% image_resize("300x300")
      
      # calculate the offset (the position of the logo inside the plot)
      # the 0.01 and 0.99 are to put a 1% and 99% padding (respectively)
      height_off <- image_info(final_plot)$height - image_info(LMI_logo)$height -
        image_info(final_plot)$height * 0.01
      width_off <- (image_info(final_plot)$width - image_info(LMI_logo)$width) * 0.99
      offset_image <- paste("+", width_off,"+", height_off, sep = "")
      
      # plot + logo
      plot_logoed <- final_plot %>% image_composite(LMI_logo, offset = offset_image)
      
      # save the plot after the logo was added
      image_write(plot_logoed, path = "figs\\plot_ready.png", format = "png")
    }
  })
}


# Function to get big5 leagues, teams and players URLs
get_players_urls_big5 <- function(curr_szn = 2023, leagues = c("ENG", "ESP", "FRA", "ITA", "GER")) {
  # get leagues' URLs
  league_urls <- fb_league_urls(country = leagues,
                                gender = "M",
                                season_end_year = curr_szn)
  league_names <- c("Premier-League", "La-Liga", "Ligue-1", "Bundesliga", "Serie-A")
  
  # for loop to get the teams' and players' URLs
  all_teams_urls <- c()
  all_players_urls <- c()
  for (URL in league_urls) {
    # get the URLs from the teams that play in that league
    team_urls <- fb_teams_urls(URL)
    
    # loop through the league names and check if each one is contained in the league URL
    matching_league <- ""
    for (i in 1:length(league_names)) {
      if (str_detect(URL, league_names[i])) {
        matching_league <- league_names[i]
        break
      }
    }
    
    # get the URL's from the players that play in each of those teams
    for (team_link in team_urls) {
      player_urls <- fb_player_urls(team_link)
      
      # get the team name from the team's URL
      matching_team <- sub("-Stats", "", substr(team_link, 38, nchar(team_link)))
      
      # create a dataframe with all the player's team and league data
      players_data_frame <- data.frame("Players URL" = player_urls,
                                       "Team" = rep(matching_team, length(player_urls)),
                                       "League" = rep(matching_league, length(player_urls)))
      
      # unite the data frame with the other team's players data
      if (is.null(all_players_urls)) {
        all_players_urls <- players_data_frame
      } else {
        all_players_urls <- rbind(all_players_urls, players_data_frame)
      }
      
      # save the teams' URLs
      all_teams_urls <- c(all_teams_urls, team_link)
    }
  }
  
  return(all_players_urls)
  return(all_teams_urls)
  return(league_urls)
}


# Function to generate percentile plots for each player of the selected leagues...
# ...and then save it in the drive folder
drive_plot_saver <- function(language_def = "ENG", logo_in_plot = FALSE, leagues_selected = c("ENG", "ESP", "FRA", "ITA", "GER")){
  # get the data from the current season
  big5_team_playing_time <- fb_big5_advanced_season_stats(season_end_year= curr_szn,
                                                          stat_type= "playing_time",
                                                          team_or_player= "player")
  
  # get leagues' URLs
  league_urls <- fb_league_urls(country = leagues_selected,
                                gender = "M",
                                season_end_year = curr_szn)
  league_names <- c("Premier-League", "La-Liga", "Ligue-1", "Bundesliga", "Serie-A")
  
  # for loop to get the teams' and players' URLs
  all_teams_urls <- c()
  all_players_urls <- c()
  for (URL in league_urls) {
    # get the URLs from the teams that play in that league
    team_urls <- fb_teams_urls(URL)
    
    # loop through the league names and check if each one is contained in the league URL
    matching_league <- ""
    for (i in 1:length(league_names)) {
      if (str_detect(URL, league_names[i])) {
        matching_league <- league_names[i]
        break
      }
    }
    
    # get the URL's from the players that play in each of those teams
    for (team_link in team_urls) {
      player_urls <- fb_player_urls(team_link)
      
      # get the team name from the team's URL
      matching_team <- sub("-Stats", "", substr(team_link, 38, nchar(team_link)))
      
      # create a dataframe with all the player's team and league data
      players_data_frame <- data.frame("Players URL" = player_urls,
                                       "Team" = rep(matching_team, length(player_urls)),
                                       "League" = rep(matching_league, length(player_urls)))
      
      # unite the data frame with the other team's players data
      if (is.null(all_players_urls)) {
        all_players_urls <- players_data_frame
      } else {
        all_players_urls <- rbind(all_players_urls, players_data_frame)
      }
      
      # save the teams' URLs
      all_teams_urls <- c(all_teams_urls, team_link)
    }
  }
  
  # for loop to create a plot for every player (in all_players_urls) and save it on google drive
  for (rowd in seq(1, nrow(all_players_urls))) {
    try({
      URL_player <- all_players_urls$Players.URL[rowd]
      TEAM_player <- all_players_urls$Team[rowd]
      LEAGUE_player <- all_players_urls$League[rowd]
      NAME_player <- substring(URL_player, 39)
      
      
      # get the match logs of the last 2 seasons
      player_last_365 <- c()
      for (szn in c(curr_szn-1, curr_szn)) {
        player_summary <- fb_player_match_logs(URL_player,
                                               season_end_year = szn, stat_type = 'summary')
        if (!is.null(player_last_365)) {
          player_last_365 <- rbind(player_last_365, player_summary)
        } else if (is.null(player_last_365)) {
          player_last_365 <- player_summary
        }
      }
      
      # filter the match logs to show the matches in the last 365 days
      #... and to only include matches from the big 5 leagues, UCL and UEL
      player_last_365 <- player_last_365 %>% filter(Date >=  (Sys.Date() - 365), Comp %in% c(unique(big5_team_playing_time$Comp),"Champions Lg", "Europa Lg"))
      
      # sum the amount of minutes to check if the player played more than 450m in the big 5 leagues in the last 365 days
      MINS_player <- sum(as.numeric(player_last_365$Min), na.rm = T)
      
      
      # create the plot if the player played more than 450 mins in the last 365 days
      if (as.numeric(MINS_player) >= 450) {
        percentile_plot_creator(playerlink = URL_player, lang = language_def)
        
        # save the PNG file generated in the respective DRIVE folder, with a custom name (player's name)
        path_file <- paste("LMI/Percentiles/", LEAGUE_player, sep = "")
        name_file <- paste(NAME_player, ".png", sep = "")
        drive_upload("figs/plot_ready.png", path = path_file, overwrite = T, name = name_file)
        
        
        if (logo_in_plot == FALSE) {
          # save the PNG file generated in the respective DRIVE folder, with a custom name (player's name)
          path_file <- paste("LMI/Percentiles/", LEAGUE_player, sep = "")
          name_file <- paste(NAME_player, ".png", sep = "")
          drive_upload("figs/plot_NO_LOGO.png", path = path_file, overwrite = T, name = name_file)
        } else if (logo_in_plot == TRUE) {
          # save the PNG file generated in the respective DRIVE folder, with a custom name (player's name)
          path_file <- paste("LMI/Percentiles/", LEAGUE_player, sep = "")
          name_file <- paste(NAME_player, ".png", sep = "")
          drive_upload("figs/plot_ready.png", path = path_file, overwrite = T, name = name_file)
        }
      }
    })
  }
}

