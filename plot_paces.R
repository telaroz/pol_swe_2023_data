library(handbaloner)
library(ggflags)
library(ggplot2)
plot_paces <- function(chosen_match_id, move_explanation_right = 0){
  raw_data <- data.table::fread("C:/Balonmano/pol_swe_2023_data/PBP_pol_swe.csv")

  data <- raw_data[start_of_possession != "",
                   .(match_id, score, half, team, teams, is_home, number_of_possession,
                     possession, start_of_possession, end_of_possession,
                     lead, numeric_time)]

  data <- data[,.SD[.N], .(match_id, number_of_possession)]

  data[, start_of_possession := data.table::fifelse(stringr::str_length(start_of_possession) == 8,
                                                    stringr::str_sub(start_of_possession, 1, 5),
                                                    start_of_possession)]


  data[, end_of_possession := data.table::fifelse(stringr::str_length(end_of_possession) == 8,
                                                  stringr::str_sub(end_of_possession, 1, 5),
                                                  end_of_possession)]

  data[, possession_length := as.numeric(lubridate::ms(end_of_possession)) -
         as.numeric(lubridate::ms(start_of_possession))]

  data[, sum(possession_length)]
  data[, sum(possession_length), .(match_id, half)]


  data[, lead_at_beginning_of_possession := data.table::shift(lead), match_id]
  data[is.na(lead_at_beginning_of_possession), lead_at_beginning_of_possession := 0]


  data[, .(pace = mean(possession_length)), possession][order(pace)]



  ex <- data[match_id == chosen_match_id][, numeric_time := as.numeric(lubridate::ms(start_of_possession))][]

  which_home <- ex[is_home == TRUE]$team[1]
  ex[, is_home := possession == which_home]
  ex[, lead_by_team := data.table::fifelse(is_home,
                                           lead_at_beginning_of_possession,
                                           -lead_at_beginning_of_possession)]

  ex[, mean_pace := round(mean(possession_length)), possession]


  add_pace <- function(minutes){
    ex[, glue::glue("interval_{minutes}_minutes") := (numeric_time %/% (minutes*60))*minutes]
    ex[, glue::glue("pace_by_{minutes}_minutes") := mean(possession_length), by = c("possession", glue::glue("interval_{minutes}_minutes"))]
  }

  purrr::walk(c(1, 5, 10, 15, 20, 25, 30), add_pace)

  ex <- complete_team_names[ex, on = "team==possession"][, possession := team]

  for_plot <- ex[, .(numeric_time, possession = team,
                     minutes = interval_5_minutes,
                     pace = pace_by_5_minutes,
                     lead_by_team, score, lower_ggflag)]

  add_bands(data = for_plot,
            minutes_column =  "minutes",
            band_cuts = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))
  #interval_5_minutes,
  # pace_by_1_minutes, pace_by_10_minutes, pace_by_5_minutes, pace_by_15_minutes, pace_by_20_minutes, pace_by_25_minutes, pace_by_30_minutes)]

  pace_plot <- for_plot[,.(minutes, numeric_time, pace, possession, score, minutes_band, lower_ggflag)
  ][,.SD[1],  by = .(minutes, pace, possession)
  ][order(numeric_time)]


  for_plot[, max_pace := max(pace), .(minutes)]
  lead_plot <- for_plot[,.(minutes, lead_by_team, possession)] %>% unique()

  scores_at_the_beggining_of_cuts <- for_plot[,.SD[1],
                                              by = .(minutes)]

  final_score <- for_plot[.N]$score


  for_title_part1 <- glue::glue("{ex[is_home == TRUE]$country[1]} - {ex[is_home == FALSE]$country[1]}")
  for_title_part2 <- glue::glue("Final score: {ex[is_home == TRUE]$possession[1]} {final_score} {ex[is_home == FALSE]$possession[1]}")
  for_title_part3 <- glue::glue("Match id: {ex$match_id[1]}")

  for_subtitle <- glue::glue("Displayed is the score each interval began with.")
  for_means <- glue::glue("
                           Mean pace in seconds:
                              {ex[is_home == TRUE]$possession[1]}: {ex[is_home == TRUE]$mean_pace[1]}
                              {ex[is_home == FALSE]$possession[1]}: {ex[is_home == FALSE]$mean_pace[1]}
                          ")


  round_up <- function(x, to = 5)
  {
    to*(x%/%to + as.logical(x%%to))
  }

  maximo_hacia_arriba <- round_up(max(pace_plot$pace) + 5, 5)

  pace_plot[, two_min_image := "2min.png"]


  plot_colours <- c("#B9AF49", "#5049B9")

  ggplot(pace_plot, mapping = aes(x = minutes, y = pace, colour = possession, country = lower_ggflag)) +
    theme_classic() +
    geom_area(aes(fill = possession), alpha = 0.10, position = 'identity', show.legend = FALSE) +
    geom_line(size = 0.7) +
    geom_flag(size = 5, show.legend = FALSE) +
    # geom_text(data = scores_at_the_beggining_of_cuts,
    #           aes(y = max(pace_plot$pace) + 2,label = score),
    #           colour = "black",
    #           size = 4) +
    geom_text(data = scores_at_the_beggining_of_cuts,
              aes(y = max_pace + 3,label = score),
              colour = "black",
              size = 4) +
    #    scale_x_continuous(, limits = c(0,60)) + scale_y_continuous(expand = c(0, maximo_hacia_arriba)) +
    scale_x_continuous(breaks = seq(0, 55, by = 5),
                       expand = c(0.015, 0.30),
                       labels = unique(for_plot$minutes_band)) +
    scale_y_continuous(breaks = seq(0, maximo_hacia_arriba, by = 5)) +
    scale_fill_manual(values = plot_colours) +
    scale_color_manual(values = plot_colours) +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#F0EEEF", colour = "black", size = 1)) +
    guides(colour = guide_legend(title = "Team")) +
    # guides(fill = "none") +
    #scale_country(guide = "possession") +
    labs(title = bquote(atop(bold(.(for_title_part1)), atop(.(for_title_part2), atop(.(for_title_part3)), ""))), # look at: https://stackoverflow.com/questions/35367095/multi-line-ggplot-title-with-different-font-size-face-etc
         subtitle = for_subtitle,
         x = "Interval in minutes",
         y = "Pace",
         caption = "Source: ihf.info") +
    annotate("label", x = 5.5 + move_explanation_right,
             y = maximo_hacia_arriba,
             label = "Pace is defined as the mean possession \n  length in seconds for a specified game interval",
             fill = "#B9AF49") +
    annotate("label", x = 25,
             y = 5,
             label = for_means, fill = "#A6A5EE")
}

plot_paces(20)
