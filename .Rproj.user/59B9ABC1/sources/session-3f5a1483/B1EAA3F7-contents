#' quick pitch characteristics table
#'
#' @param data,
#' @return a summary of pitch metrics for the given data
#' @export
pitcher_pitch_metrics <- function(data) {

  table <- data %>%
    # using recode will allow us to save space on the document
    dplyr:: mutate(TaggedPitchType = factor(TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "Changeup", "Splitter", 'Knuckleball', 'Other')),
                   TaggedPitchType = dplyr::recode(TaggedPitchType, Fastball = "FB", Curveball = 'CB', Sinker = 'SI', Slider = 'SL',
                                            Cutter = 'CT', Changeup = 'CH', Splitter = 'SPL', Knuckleball = 'KN', Other = 'OT' )
    ) %>%
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
                     'Spin' = round(mean(SpinRate, na.rm = TRUE),0),
                     'SpinEff%' = round(mean(yt_Efficiency, na.rm= TRUE),0),
                     'SVR' = round(Spin/Velo,1),
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
pitcher_plot <- function(path_to_csv_file, pitcher_name) {

  pitchers <- unique(read.csv(path_to_file)$Pitcher)

  print(pitchers)

  pitcher <- read.csv(path_to_file) %>%
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
      ggplot2::xlim(-22, 22) +
      ggplot2::ylim(-22, 22) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = -22, xend = 0, yend = 22), size = 1, color = "grey55") +
      ggplot2::geom_segment(ggplot2::aes(x = -22, y = 0, xend = 22, yend = 0), size = 1, color = "grey55") +
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
game_check <- function(path_to_file) {
  game_test <- read.csv(path_to_file) %>%
    select(PitchNo, Inning, Top.Bottom, PAofInning, PitchofPA, Pitcher, Batter, Balls, Strikes, PitchCall, KorBB, PlayResult) %>%
    dplyr::group_by(Inning, Top.Bottom, PAofInning) %>%
    dplyr::mutate(#check = n_distinct(Batter),
      pa_check = ifelse(n_distinct(Batter) == 1, T, F),
      pitch_check = ifelse(lag(PitchofPA) < PitchofPA, T, F),
      count_check = ifelse(paste(Balls, Strikes) != lag(paste(Balls, Strikes)), T,
                           ifelse(paste(Balls, Strikes) == lag(paste(Balls, Strikes)) & lag(PitchCall) %in% c('Foul'), T, F)),
    ) %>%
    ungroup()

  print(
    game_test %>%
      dplyr::summarise(pa_check = sum(pa_check == FALSE, na.rm = T),
                       pitch_check = sum(pitch_check == FALSE, na.rm = T),
                       count = sum(count_check == FALSE, na.rm = T)
      )
  )

  return(game_test)
}
