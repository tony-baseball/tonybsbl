#' quick pitch characteristics table
#'
#' @param data,
#' @return a summary of pitch metrics for the given data
#' @export
pitcher_pitch_metrics <- function(data, cols_to_group_by) {

  data_new <<- data %>%
    # using recode will allow us to save space on the document
    dplyr:: mutate(TaggedPitchType = factor(TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "Changeup", "Splitter", 'Knuckleball', 'Other')),
                   TaggedPitchType = dplyr::recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                                   Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Knuckleball = 'KN', Other = 'OT' )
    ) %>%
    dplyr::mutate(SpinAxis_inferred = atan2(HorzBreak, InducedVertBreak) * (180/ pi) + 180, .after = SpinAxis
    )

  table <- data_new %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(cols_to_group_by))) %>%
    dplyr::summarise('No.' = n(),
                     'Usage' = n(),
                     'Usage %' = n(),
                     'Velo' = round(mean(RelSpeed, na.rm = TRUE),1),
                     'VeloMax' = round(max(RelSpeed, na.rm = TRUE),1),
                     'Tilt' = round(mean(SpinAxis, na.rm = TRUE),0),
                     'Time' = sapply(`Tilt`, function(x) if (is.na(x)){return(NA)}
                                     else if(x > 180 & x <= 360){(x/30)-6}
                                     else if(x == 180){12}
                                     else{(x/30)+6}),
                     'HH' = as.integer(Time),
                     'HH' = sapply(HH, function(x) if (is.na(x)){return(NA)}
                                   else if(x == 0){x+12}
                                   else if(x > 12){x-12}
                                   else{x+0}),
                     "MM" = formatC(round((Time%%1)*60, digits = 0), width = 2, flag = "0"),
                     'Tilt' = paste0(HH,":", MM),
                     'SpinAxis' = round(mean(SpinAxis, na.rm = TRUE),0),
                     'Tilt_inferred' = round(mean(SpinAxis_inferred, na.rm = TRUE),0),
                     'Time' = sapply(`Tilt_inferred`, function(x) if (is.na(x)){return(NA)}
                                     else if(x > 180 & x <= 360){(x/30)-6}
                                     else if(x == 180){12}
                                     else{(x/30)+6}),
                     'HH' = as.integer(Time),
                     'HH' = sapply(HH, function(x) if (is.na(x)){return(NA)}
                                   else if(x == 0){x+12}
                                   else if(x > 12){x-12}
                                   else{x+0}),
                     "MM" = formatC(round((Time%%1)*60, digits = 0), width = 2, flag = "0"),
                     'Tilt_inferred' = paste0(HH,":", MM),
                     'SpinAxis_inferred' = round(mean(SpinAxis_inferred, na.rm = TRUE),0),
                     'SpinRate' =  round(mean(SpinRate, na.rm= TRUE),0),
                     'SpinEff%' = round(mean(yt_Efficiency, na.rm= TRUE),0),
                     'SVR' = round(SpinRate/Velo,1),
                     'Vert' = round(mean(InducedVertBreak, na.rm = TRUE),1),
                     'Horz' = round(mean(HorzBreak, na.rm = TRUE),1),
                     'VAA' = round(mean(VertApprAngle, na.rm = TRUE),1),
                     'RelHt' = round(mean(RelHeight, na.rm = TRUE),1),
                     'RelSide' = round(mean(RelSide, na.rm = TRUE),1),
                     'Ext' = round(mean(Extension, na.rm = TRUE),1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>%
    dplyr::select(-Usage,-Time,-HH,-MM)

  return(table)

}


#' quick pitch characteristics table
#'
#' @param data,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
pitcher_mvmt_plot <- function(path_to_csv_file, pitcher_name) {

  pitchers <- unique(read.csv(path_to_csv_file)$Pitcher)

  print(pitchers)

  pitcher <- read.csv(path_to_csv_file) %>%
    dplyr::filter(grepl(pitcher_name , Pitcher)) %>%
    dplyr::mutate(TaggedPitchType = recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                           Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Knuckleball = 'KN' ),
                  TaggedPitchType = factor(TaggedPitchType, levels = c("FB", 'SI', 'CT', 'CB', 'SL', 'CH', 'SPL', 'KN', 'OT' )) )

  pitch_info = paste('/nPitch #',pitcher$PitchNo, '/n',
                     round(pitcher$RelSpeed,1), "mph/n",
                     round(pitcher$SpinRate), 'rpm/n',
                     round(pitcher$yt_Efficiency), '%')
  plotly::ggplotly(
    ggplot2::ggplot(data = pitcher, ggplot2::aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType,
                                                 label = pitch_info)) +
      ggplot2::labs(color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)",  title = pitcher$Pitcher[1]) +
      ggplot2::xlim(-25, 25) +
      ggplot2::ylim(-25, 25) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
      ggplot2::geom_segment(ggplot2::aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
      ggplot2::geom_point(size =4, alpha = .8) +
      ggplot2::scale_color_manual(values = c('FB' = 'red', 'CB' = 'darkgreen', 'SI' = 'orange',  'SL'='cornflowerblue',
                                             'CT' = 'gold',  'CH'='violet', 'OT' = 'black', 'SPL' = 'black', 'KN' = 'black')) + # , na.rm = TRUE)+
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5)) +
      ggplot2::theme(legend.position = "bottom",
                     legend.text = ggplot2::element_text(size = 12),
                     axis.title = ggplot2::element_text(size = 14))
  ) %>%
    plotly::layout(autosize = T,
                   showlegend = TRUE )

}
#
#' quick pitch characteristics table
#'
#' @param path_to_file,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
game_check <- function(path_to_file) {
  game_test <- read.csv(path_to_file) %>%
    dplyr::select(PitchNo, Inning, Top.Bottom, PAofInning, PitchofPA, Pitcher, Batter, Balls, Strikes, PitchCall, KorBB, PlayResult) %>%
    dplyr::group_by(Inning, Top.Bottom, PAofInning) %>%
    dplyr::mutate(#check = n_distinct(Batter),
      pa_check = ifelse(n_distinct(Batter) == 1, T, F),
      pitch_check = ifelse(lag(PitchofPA) < PitchofPA, T, F),
      count_check = ifelse(paste(Balls, Strikes) != lag(paste(Balls, Strikes) ), T,
                           ifelse(paste(Balls, Strikes) == lag(paste(Balls, Strikes)) & lag(PitchCall) %in% c('Foul'), T, F)),
      across(c(pa_check, pitch_check, count_check), ~ifelse(is.na(.),T,.) )
    ) %>%
    ungroup() %>%
    dplyr::group_by(Inning, Top.Bottom, Batter) %>%
    dplyr::mutate(
      distinct_batter = length(unique(PAofInning)),
      pa1 = unique(PAofInning)[1],
      pa2 = unique(PAofInning)[2],
      distinct_batter = ifelse(pa2 > pa1 + 8, T, F),
      distinct_batter = ifelse(is.na(distinct_batter), T, distinct_batter),
      pa_check = case_when(
        distinct_batter == T & pa_check == T ~ T,
        distinct_batter == F & pa_check == T ~ F,
        distinct_batter == T & pa_check == F ~ F,
        distinct_batter == F & pa_check == F ~ F,
        T ~ pa_check
      )
    ) %>%
    ungroup()

  print(
    game_test %>%
      dplyr::summarise(pa_check = sum(pa_check == FALSE, na.rm = T),
                       pitch_check = sum(pitch_check == FALSE, na.rm = T),
                       count = sum(count_check == FALSE, na.rm = T),
                       distinct_batter = sum(distinct_batter == FALSE, na.rm = T)
      )
  )

  return(game_test)
}
#
game_check_db <- function(data, date) {
  game_test <- data %>%
    filter(Date == date)  %>%
    dplyr::select(PitchNo, Inning, Top.Bottom, PAofInning, PitchofPA, Pitcher, Batter, Balls, Strikes, PitchCall, KorBB, PlayResult) %>%
    dplyr::group_by(Inning, Top.Bottom, PAofInning) %>%
    dplyr::mutate(#check = n_distinct(Batter),
      pa_check = ifelse(n_distinct(Batter) == 1, T, F),
      pitch_check = ifelse(lag(PitchofPA) < PitchofPA, T, F),
      count_check = ifelse(paste(Balls, Strikes) != lag(paste(Balls, Strikes) ), T,
                           ifelse(paste(Balls, Strikes) == lag(paste(Balls, Strikes)) & lag(PitchCall) %in% c('Foul'), T, F)),
      across(c(pa_check, pitch_check, count_check), ~ifelse(is.na(.),T,.) )
    ) %>%
    ungroup() %>%
    dplyr::group_by(Inning, Top.Bottom, Batter) %>%
    dplyr::mutate(
      distinct_batter = length(unique(PAofInning)),
      pa1 = unique(PAofInning)[1],
      pa2 = unique(PAofInning)[2],
      distinct_batter = ifelse(pa2 > pa1 + 8, T, F),
      distinct_batter = ifelse(is.na(distinct_batter), T, distinct_batter),
      pa_check = case_when(
        distinct_batter == T & pa_check == T ~ T,
        distinct_batter == F & pa_check == T ~ F,
        distinct_batter == T & pa_check == F ~ F,
        distinct_batter == F & pa_check == F ~ F,
        T ~ pa_check
      )
    ) %>%
    ungroup()

  print(
    game_test %>%
      dplyr::summarise(pa_check = sum(pa_check == FALSE, na.rm = T),
                       pitch_check = sum(pitch_check == FALSE, na.rm = T),
                       count = sum(count_check == FALSE, na.rm = T),
                       distinct_batter = sum(distinct_batter == FALSE, na.rm = T )
      )
  )

  return(game_test)
}


#
#' quick pitch characteristics table
#'
#' @param database_connection,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
check_player_teams <- function(database_connection){

  # Batters, current correct = 0 rows
  suppressMessages(
    print(
      RSQLite::dbGetQuery(database_connection, 'SELECT * FROM yak_24') %>%
        dplyr::group_by(Batter, BatterTeam) %>%
        dplyr::summarise(
          # savant Batting
          PA = length(unique(GameID, Inning, PAofInning))
        ) %>%
        dplyr::ungroup() %>%
        filter(duplicated(Batter) | duplicated(Batter, fromLast = T))
    )
  )
  # Pitchers, current correct = 0 rows
  suppressMessages(
    print(
      RSQLite::dbGetQuery(database_connection, 'SELECT * FROM yak_24') %>%
        dplyr::group_by(Pitcher, PitcherTeam) %>%
        summarise(
          # savant Batting
          PA = length(unique(GameID, Inning, PAofInning))
        ) %>%
        dplyr::ungroup() %>%
        filter(duplicated(Pitcher) | duplicated(Pitcher, fromLast = T))
    )
  )
  # Catchers, current correct = 0 rows
  suppressMessages(
    print(
      RSQLite::dbGetQuery(database_connection, 'SELECT * FROM yak_24') %>%
        dplyr::group_by(Catcher, CatcherTeam) %>%
        dplyr::summarise(
          # savant Batting
          n = n()
        ) %>%
        dplyr::ungroup() %>%
        filter(duplicated(Catcher) | duplicated(Catcher, fromLast = T))
    )
  )

}


#
#' quick pitch characteristics table
#'
#' @param database_connection,
#' @return The final dataframe.
#' @examples
#' db_to_bats('2024-05-10')
#'
#' @export
check_player_names <- function(database_connection){
  # ----- Match Player Names -----
  # This code down here will take each player's name from the frontier league website and compare it up against Yakkertech player names.
  # sometimes, YT playernames are not formatted/spelled correctly, so it will cause issues when trying to join stats from both sources
  # ideally, we wont have any NA in either name column, but that's currently not the case
  # If there is an NA in the FL_Name column, it can mean that there is a player on the roster that has not played yet (either injured, visa problems) or misspelled
  # If there is an NA in the YT_Name column, it usually means that the name is misspelled somewhere.
  # You then need to investigate to find the right spelling of the name so it can match (FrontierLeague.com, baseball reference, etc)
  # Then let me know the incorrect spelling, and what the spelling should be!!!!

  # Hitters, current correct rows for hit should be 215
  fl_h <- RSQLite::dbGetQuery(database_connection, 'SELECT * from front24 order by NAME') %>%
    filter(!grepl('P', POS)) %>%
    filter(!grepl('Player|Team', NAME)) %>%
    filter(AB > 0) %>%
    select(NAME, TEAM)

  yak_h <- RSQLite::dbGetQuery(database_connection, 'SELECT distinct Batter from yak_24 order by Batter')

  hit <<- fl_h %>%
    dplyr::full_join(yak_h, by = c('NAME' = 'Batter'), keep = T) %>%
    dplyr::rename(FL_Name = NAME,
                  YT_Name = Batter)

  # Pitchers, current correct rows for pitch should be 231
  fl_p <- RSQLite::dbGetQuery(database_connection, 'SELECT * from front_p24 order by NAME') %>%
    filter(!grepl('Player|Team', NAME)) %>%
    filter(APP > 0) %>%
    select(NAME, TEAM)

  yak_p <- RSQLite::dbGetQuery(database_connection, 'SELECT distinct Pitcher from yak_24 order by Pitcher')

  pitch <<- fl_p %>%
    dplyr::full_join(yak_p, by = c('NAME' = 'Pitcher'), keep = T)%>%
    dplyr::rename(FL_Name = NAME,
                  YT_Name = Pitcher)


  # Catchers, current correct rows for catch should be 48
  # Pitchers and batters are a little more straight forward since people pay attention to those when tagging, but there are usually mistakes with catchers
  fl_c <- RSQLite::dbGetQuery(database_connection, 'SELECT * from front24 order by NAME') %>%
    filter(!grepl('Player|Team', NAME)) %>%
    filter(G > 0) %>%
    filter(grepl('C', POS)) %>%
    select(NAME, TEAM, POS)

  yak_c <- RSQLite::dbGetQuery(database_connection, 'SELECT distinct Catcher from yak_24 order by Catcher')

  catch <<- fl_c %>%
    dplyr::full_join(yak_c, by = c('NAME' = 'Catcher'), keep = T)%>%
    dplyr::rename(FL_Name = NAME,
                  YT_Name = Catcher)
}



pitch_types_factor <- function(data) {
  data <- data %>%
    dplyr::mutate(TaggedPitchType = factor(TaggedPitchType, levels= c("Fastball", 'Sinker', 'Cutter', 'Curveball', 'Slider', 'Changeup',
                                                          'Splitter', 'Knuckleball', 'Other' ))
                  )

  return(data)
}

pitch_types_recode <- function(data) {
  data <- data %>%
    dplyr::mutate(TaggedPitchType = dplyr::recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                 Cutter = 'CT', Changeup = 'CH', Other = 'OT', Splitter = 'SPL', Knuckleball = 'KN' )
    )

  return(data)
}

battrax_clean_format <- function(file_path) {

  cat("Reading BatTrax CSV file...",fill = T)
  setTxtProgressBar(pb, 1, label = 'hi')
  cat("\n")
  Sys.sleep(3)

  cat("Getting CSV file properties...",fill = T)
  setTxtProgressBar(pb, 2)
  cat("\n")
  Sys.sleep(3)

  csv_location <- dirname(file_path)

  file_name <- gsub("\\.csv","",(basename(file_path)))

  cat("Transforming BatTrax CSV file...",fill = T)
  setTxtProgressBar(pb, 3)
  cat("\n")
  Sys.sleep(3)

  csv <- read.csv(file_path) %>%
    dplyr::rename(TaggedPitchType = PitchType,
                  RelSpeed = PitchSpeed,
                  `Top.Bottom` = TopBottom) %>%
    dplyr::group_by(Inning,`Top.Bottom`,Date) %>%
    dplyr::mutate(PAofInning = cumsum(!duplicated(Batter) | lag(Batter) != Batter)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PAofGame = cumsum(!duplicated(Batter) | lag(Batter) != Batter),
                  PitchUUID = paste0(gsub('-','',Date),"_", substr(HomeTeam,1,3),"_",substr(AwayTeam,1,3), "_", sprintf("%02d",Inning),sprintf("%02d",PitchNo)),
                  across(c(HomeTeam,AwayTeam,BatterTeam,PitcherTeam,CatcherTeam), ~ '_ADV'))

  csv_cols <- colnames(csv)

  tm_cols <- c("PitchNo", "Date", "Time", "PAofInning", "PitchofPA", "Pitcher",
               "PitcherId", "PitcherThrows", "PitcherTeam", "Batter", "BatterId",
               "BatterSide", "BatterTeam", "PitcherSet", "Inning", "Top.Bottom",
               "Outs", "Balls", "Strikes", "TaggedPitchType", "AutoPitchType",
               "PitchCall", "KorBB", "HitType", "PlayResult", "OutsOnPlay",
               "RunsScored", "Notes", "RelSpeed", "VertRelAngle", "HorzRelAngle",
               "SpinRate", "SpinAxis", "Tilt", "RelHeight", "RelSide", "Extension",
               "VertBreak", "InducedVertBreak", "HorzBreak", "PlateLocHeight",
               "PlateLocSide", "ZoneSpeed", "VertApprAngle", "HorzApprAngle",
               "ZoneTime", "ExitSpeed", "Angle", "Direction", "HitSpinRate",
               "PositionAt110X", "PositionAt110Y", "PositionAt110Z", "Distance",
               "LastTrackedDistance", "Bearing", "HangTime", "pfxx", "pfxz",
               "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax0", "ay0", "az0", "HomeTeam",
               "AwayTeam", "Stadium", "Level", "League", "GameID", "PitchUUID",
               "Catcher", "CatcherId", "CatcherTeam")


  missing_columns <- setdiff(tm_cols, csv_cols)

  for(col in missing_columns) {
    csv[[col]] <- NA
  }

  csv_ <- csv %>%
    dplyr::select(all_of(tm_cols))

  cat(paste("Writing BatTrax_transformed CSV file to", paste0(csv_location,"/",file_name,"_transformed.csv")),fill = T)
  setTxtProgressBar(pb, 4)
  cat("\n")
  Sys.sleep(3)

  write.csv(csv_, paste0(csv_location,"/",file_name,"_transformed.csv"), na = '', row.names = F)

  cat("DONE!",fill = T)
  setTxtProgressBar(pb, 5)
}
