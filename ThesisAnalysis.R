## Installing necessary packages
{
install.packages(tidyverse)
install.packages(hockeyR)
install.packages(magrittr)
install.packages(fixest)
install.packages("modelsummary")
library(tidyverse)
library(hockeyR)
library(magrittr)
library(fixest)
library(modelsummary)
}

## Filtering Data For Analysis
# 17-18 Season
{
  # Filtering data
  pbp1718 <- load_pbp('2017-2018')
  pbp1718$gd <- abs(pbp1718$home_score - pbp1718$away_score)
  enPBP1718 <- pbp1718 %>% 
    filter(season_type == "R",
           game_seconds_remaining <= 300,
           period == 3,
           event_type != "STOP",
           strength_state == "6v5",
           !is.na(event_team)) |>
    filter(row_number() == 1, .by = c(season, game_id, event_team_abbr))
  
  optimal1718 <- enPBP1718 |> 
    mutate(is_optimal_criteria = game_seconds_remaining <= 180 & gd == 1 & strength_state == "6v5") |>
    summarize(
      optimal_pulls = sum(is_optimal_criteria, na.rm = TRUE),
      non_optimal_pulls = sum(!is_optimal_criteria, na.rm = TRUE),
      .by = c(event_team_abbr)
    ) 
  
  points1718 <- get_team_records(2018) %>%
    mutate(team = ifelse(team_abbr == "VEG", "VGK", team_abbr)) %>%
    select(team, st_points)
  
  playoffs1718 <- pbp1718 |>
    group_by(team = event_team_abbr) |>
    summarize(
      gp = length(unique(game_id)),
      playoffs = ifelse(gp > 82, 1, 0)
    ) |>
    select(team, playoffs)
  
  GFGA1718 <- pbp1718 |>
    filter(!is.na(event_team) & period < 5 & season_type == "R") |>
    group_by(team = event_team_abbr) |>
    summarise(
      gp = length(unique(game_id)),
      gf = sum(event_type == "GOAL"),
      .groups = "drop"
    ) |>
    left_join(
      pbp1718 |>
        filter(!is.na(event_team) & period < 5) |>
        mutate(team = ifelse(event_team_abbr == home_abbreviation, away_abbreviation, home_abbreviation)) |>
        group_by(team) |>
        summarise(
          ga = sum(event_type == "GOAL"),
          .groups = "drop"
        ),
      by = "team"
    ) 
  
  en1718GameIDs <- enPBP1718$game_id
  success1718 <- pbp1718 %>%
    filter(game_id %in% en1718GameIDs,
           period == 3, 
           event_type == "PERIOD_END",
           gd == c("1", "2", "0")) 
  successCalc1718 <- success1718 |>
    mutate(successfulPullCalc = gd == 0) |>
    summarize(
      successfulPull = sum(successfulPullCalc, na.rm = TRUE),
      nonsuccessfulPull = sum(!successfulPullCalc, na.rm = TRUE),
    )
  
  # Joining filters
  points1718 <- left_join(GFGA1718, points1718, by = "team")
  playoffsGFGA1718 <- left_join(points1718, playoffs1718, by = "team")
  final1718 <- left_join(playoffsGFGA1718, optimal1718, by = c("team" = "event_team_abbr"))
  final1718$optimalPullPercent <- as.numeric(final1718$optimal_pulls/(final1718$optimal_pulls + final1718$non_optimal_pulls))
  final1718$gfa <- (final1718$gf/final1718$gp)
  final1718$gaa <- (final1718$ga/final1718$gp)
  final1718$st_points <- as.numeric(final1718$st_points)
  final1718 <- final1718 %>% select(-gp)
}

# 18-19 Season
{
# Filtering data
pbp1819 <- load_pbp('2018-2019')
pbp1819$gd <- abs(pbp1819$home_score - pbp1819$away_score)
enPBP1819 <- pbp1819 %>% 
  filter(season_type == "R",
         game_seconds_remaining <= 300,
         period == 3,
         event_type != "STOP",
         strength_state == "6v5",
         !is.na(event_team)) |>
  filter(row_number() == 1, .by = c(season, game_id, event_team_abbr))

optimal1819 <- enPBP1819 |> 
  mutate(is_optimal_criteria = game_seconds_remaining <= 180 & gd == 1 & strength_state == "6v5") |>
  summarize(
    optimal_pulls = sum(is_optimal_criteria, na.rm = TRUE),
    non_optimal_pulls = sum(!is_optimal_criteria, na.rm = TRUE),
    .by = c(event_team_abbr)
  ) 

points1819 <- get_team_records(2019) %>%
  mutate(team = ifelse(team_abbr == "VEG", "VGK", team_abbr)) %>%
  select(team, st_points)

playoffs1819 <- pbp1819 |>
  group_by(team = event_team_abbr) |>
  summarize(
    gp = length(unique(game_id)),
    playoffs = ifelse(gp > 82, 1, 0)
  ) |>
  select(team, playoffs)

GFGA1819 <- pbp1819 |>
  filter(!is.na(event_team) & period < 5 & season_type == "R") |>
  group_by(team = event_team_abbr) |>
  summarise(
    gp = length(unique(game_id)),
    gf = sum(event_type == "GOAL"),
    .groups = "drop"
  ) |>
  left_join(
    pbp1819 |>
      filter(!is.na(event_team) & period < 5) |>
      mutate(team = ifelse(event_team_abbr == home_abbreviation, away_abbreviation, home_abbreviation)) |>
      group_by(team) |>
      summarise(
        ga = sum(event_type == "GOAL"),
        .groups = "drop"
      ),
    by = "team"
  ) 

# Successful Pull Calcs
en1819GameIDs <- enPBP1819$game_id
success1819 <- pbp1819 %>%
  filter(game_id %in% en1819GameIDs,
         period == 3, 
         event_type == "PERIOD_END",
         gd == c("1", "2", "0")) 
successCalc1819 <- success1819 |>
  mutate(successfulPullCalc = gd == 0) |>
  summarize(
    successfulPull = sum(successfulPullCalc, na.rm = TRUE),
    nonsuccessfulPull = sum(!successfulPullCalc, na.rm = TRUE),
  )

# Joining filters
points1819 <- left_join(GFGA1819, points1819, by = "team")
playoffsGFGA1819 <- left_join(points1819, playoffs1819, by = "team")
final1819 <- left_join(playoffsGFGA1819, optimal1819, by = c("team" = "event_team_abbr"))
final1819$optimalPullPercent <- as.numeric(final1819$optimal_pulls/(final1819$optimal_pulls + final1819$non_optimal_pulls))
final1819$gfa <- (final1819$gf/final1819$gp)
final1819$gaa <- (final1819$ga/final1819$gp)
final1819$st_points <- as.numeric(final1819$st_points)
final1819 <- final1819 %>% select(-gp)
}

# 19-20 Season
{
  # Filtering data
  pbp1920 <- load_pbp('2019-2020')
  pbp1920$gd <- abs(pbp1920$home_score - pbp1920$away_score)
  enPBP1920 <- pbp1920 %>% 
    filter(season_type == "R",
           game_seconds_remaining <= 300,
           period == 3,
           event_type != "STOP",
           strength_state == "6v5",
           !is.na(event_team)) |>
    filter(row_number() == 1, .by = c(season, game_id, event_team_abbr))
  
  optimal1920 <- enPBP1920 |> 
    mutate(is_optimal_criteria = game_seconds_remaining <= 180 & gd == 1 & strength_state == "6v5") |>
    summarize(
      optimal_pulls = sum(is_optimal_criteria, na.rm = TRUE),
      non_optimal_pulls = sum(!is_optimal_criteria, na.rm = TRUE),
      .by = c(event_team_abbr)
    ) 
  
  points1920 <- get_team_records(2020) %>%
    mutate(team = ifelse(team_abbr == "VEG", "VGK", team_abbr)) %>%
    select(team, st_points)
  
  GFGA1920 <- pbp1920 |>
    filter(!is.na(event_team) & period < 5 & season_type == "R") |>
    group_by(team = event_team_abbr) |>
    summarise(
      gp = length(unique(game_id)),
      gf = sum(event_type == "GOAL"),
      .groups = "drop"
    ) |>
    left_join(
      pbp1920 |>
        filter(!is.na(event_team) & period < 5) |>
        mutate(team = ifelse(event_team_abbr == home_abbreviation, away_abbreviation, home_abbreviation)) |>
        group_by(team) |>
        summarise(
          ga = sum(event_type == "GOAL"),
          .groups = "drop"
        ),
      by = "team"
    ) 
  
  # Successful Pull Calcs
  en1920GameIDs <- enPBP1920$game_id
  success1920 <- pbp1920 %>%
    filter(game_id %in% en1920GameIDs,
           period == 3, 
           event_type == "PERIOD_END",
           gd == c("1", "2", "0")) 
  successCalc1920 <- success1920 |>
    mutate(successfulPullCalc = gd == 0) |>
    summarize(
      successfulPull = sum(successfulPullCalc, na.rm = TRUE),
      nonsuccessfulPull = sum(!successfulPullCalc, na.rm = TRUE),
    )
  
  # Joining filters
  points1920 <- left_join(GFGA1920, points1920, by = "team")
  final1920 <- left_join(points1920, optimal1920, by = c("team" = "event_team_abbr"))
  # Hardcode playoffs to first round teams, excluding qualifying round
  playoffTeams2020 <- c("PHI", "MTL", "TBL", "CBJ", "WSH", "NYI", "BOS", "CAR", "VGK", "CHI", "COL", "ARI", "DAL", "CAL", "STL", "VAN")
  final1920$playoffs <- 0
  final1920$playoffs[final1920$team %in% playoffTeams2020] <- 1
  final1920$optimalPullPercent <- as.numeric(final1920$optimal_pulls/(final1920$optimal_pulls + final1920$non_optimal_pulls))
  final1920$gfa <- (final1920$gf/final1920$gp)
  final1920$gaa <- (final1920$ga/final1920$gp)
  final1920$st_points <- as.numeric(final1920$st_points)
  final1920 <- final1920 %>% select(-gp)
}

# 20-21 Season
{
  # Filtering data
  pbp21 <- load_pbp('2021')
  pbp21$gd <- abs(pbp21$home_score - pbp21$away_score)
  enPBP21 <- pbp21 %>% 
    filter(season_type == "R",
           game_seconds_remaining <= 300,
           period == 3,
           event_type != "STOP",
           strength_state == "6v5",
           !is.na(event_team)) |>
    filter(row_number() == 1, .by = c(season, game_id, event_team_abbr))
  
  optimal21 <- enPBP21 |> 
    mutate(is_optimal_criteria = game_seconds_remaining <= 180 & gd == 1 & strength_state == "6v5") |>
    summarize(
      optimal_pulls = sum(is_optimal_criteria, na.rm = TRUE),
      non_optimal_pulls = sum(!is_optimal_criteria, na.rm = TRUE),
      .by = c(event_team_abbr)
    ) 
  
  points21 <- get_team_records(2021) %>%
    mutate(team = ifelse(team_abbr == "VEG", "VGK", team_abbr)) %>%
    select(team, st_points)
  
  playoffs21 <- pbp21 |>
    group_by(team = event_team_abbr) |>
    summarize(
      gp = length(unique(game_id)),
      playoffs = ifelse(gp > 56, 1, 0)
    ) |>
    select(team, playoffs)
  
  GFGA21 <- pbp21 |>
    filter(!is.na(event_team) & period < 5 & season_type == "R") |>
    group_by(team = event_team_abbr) |>
    summarise(
      gp = length(unique(game_id)),
      gf = sum(event_type == "GOAL"),
      .groups = "drop"
    ) |>
    left_join(
      pbp21 |>
        filter(!is.na(event_team) & period < 5) |>
        mutate(team = ifelse(event_team_abbr == home_abbreviation, away_abbreviation, home_abbreviation)) |>
        group_by(team) |>
        summarise(
          ga = sum(event_type == "GOAL"),
          .groups = "drop"
        ),
      by = "team"
    ) 
  
  # Successful Pull Calcs
  en21GameIDs <- enPBP21$game_id
  success21 <- pbp21 %>%
    filter(game_id %in% en21GameIDs,
           period == 3, 
           event_type == "PERIOD_END",
           gd == c("1", "2", "0")) 
  successCalc21 <- success21 |>
    mutate(successfulPullCalc = gd == 0) |>
    summarize(
      successfulPull = sum(successfulPullCalc, na.rm = TRUE),
      nonsuccessfulPull = sum(!successfulPullCalc, na.rm = TRUE),
    )
  
  # Joining filters
  points21 <- left_join(GFGA21, points21, by = "team")
  playoffsGFGA21 <- left_join(points21, playoffs21, by = "team")
  final21 <- left_join(playoffsGFGA21, optimal21, by = c("team" = "event_team_abbr"))
  final21$optimalPullPercent <- as.numeric(final21$optimal_pulls/(final21$optimal_pulls + final21$non_optimal_pulls))
  final21$gfa <- (final21$gf/final21$gp)
  final21$gaa <- (final21$ga/final21$gp)
  final21$st_points <- as.numeric(final21$st_points)
  final21 <- final21 %>% select(-gp)
}

# 21-22 Season
{
  # Filtering data
  pbp2122 <- load_pbp('2021-2022')
  pbp2122$gd <- abs(pbp2122$home_score - pbp2122$away_score)
  enPBP2122 <- pbp2122 %>% 
    filter(season_type == "R",
           game_seconds_remaining <= 300,
           period == 3,
           event_type != "STOP",
           strength_state == "6v5",
           !is.na(event_team)) |>
    filter(row_number() == 1, .by = c(season, game_id, event_team_abbr))
  
  optimal2122 <- enPBP2122 |> 
    mutate(is_optimal_criteria = game_seconds_remaining <= 180 & gd == 1 & strength_state == "6v5") |>
    summarize(
      optimal_pulls = sum(is_optimal_criteria, na.rm = TRUE),
      non_optimal_pulls = sum(!is_optimal_criteria, na.rm = TRUE),
      .by = c(event_team_abbr)
    ) 
  
  points2122 <- get_team_records(2022) %>%
    mutate(team = ifelse(team_abbr == "VEG", "VGK", team_abbr)) %>%
    select(team, st_points)
  
  playoffs2122 <- pbp2122 |>
    group_by(team = event_team_abbr) |>
    summarize(
      gp = length(unique(game_id)),
      playoffs = ifelse(gp > 82, 1, 0)
    ) |>
    select(team, playoffs)
  
  GFGA2122 <- pbp2122 |>
    filter(!is.na(event_team) & period < 5 & season_type == "R") |>
    group_by(team = event_team_abbr) |>
    summarise(
      gp = length(unique(game_id)),
      gf = sum(event_type == "GOAL"),
      .groups = "drop"
    ) |>
    left_join(
      pbp2122 |>
        filter(!is.na(event_team) & period < 5) |>
        mutate(team = ifelse(event_team_abbr == home_abbreviation, away_abbreviation, home_abbreviation)) |>
        group_by(team) |>
        summarise(
          ga = sum(event_type == "GOAL"),
          .groups = "drop"
        ),
      by = "team"
    ) 
  
  # Successful Pull Calcs
  en2122GameIDs <- enPBP2122$game_id
  success2122 <- pbp2122 %>%
    filter(game_id %in% en2122GameIDs,
           period == 3, 
           event_type == "PERIOD_END",
           gd == c("1", "2", "0")) 
  successCalc2122 <- success2122 |>
    mutate(successfulPullCalc = gd == 0) |>
    summarize(
      successfulPull = sum(successfulPullCalc, na.rm = TRUE),
      nonsuccessfulPull = sum(!successfulPullCalc, na.rm = TRUE),
    )
  
  # Joining filters
  points2122 <- left_join(GFGA2122, points2122, by = "team")
  playoffsGFGA2122 <- left_join(points2122, playoffs2122, by = "team")
  final2122 <- left_join(playoffsGFGA2122, optimal2122, by = c("team" = "event_team_abbr"))
  final2122$optimalPullPercent <- as.numeric(final2122$optimal_pulls/(final2122$optimal_pulls + final2122$non_optimal_pulls))
  final2122$gfa <- (final2122$gf/final2122$gp)
  final2122$gaa <- (final2122$ga/final2122$gp)
  final2122$st_points <- as.numeric(final2122$st_points)
  final2122 <- final2122 %>% select(-gp)
}

# 22-23 Season
{
  # Filtering data
  pbp2223 <- load_pbp('2022-2023')
  pbp2223$gd <- abs(pbp2223$home_score - pbp2223$away_score)
  enPBP2223 <- pbp2223 %>% 
    filter(season_type == "R",
           game_seconds_remaining <= 300,
           period == 3,
           event_type != "STOP",
           strength_state == "6v5",
           !is.na(event_team)) |>
    filter(row_number() == 1, .by = c(season, game_id, event_team_abbr))
  
  optimal2223 <- enPBP2223 |> 
    mutate(is_optimal_criteria = game_seconds_remaining <= 180 & gd == 1 & strength_state == "6v5") |>
    summarize(
      optimal_pulls = sum(is_optimal_criteria, na.rm = TRUE),
      non_optimal_pulls = sum(!is_optimal_criteria, na.rm = TRUE),
      .by = c(event_team_abbr)
    ) 
  
  points2223 <- get_team_records(2023) %>%
    mutate(team = ifelse(team_abbr == "VEG", "VGK", team_abbr)) %>%
    select(team, st_points)
  
  playoffs2223 <- pbp2223 |>
    group_by(team = event_team_abbr) |>
    summarize(
      gp = length(unique(game_id)),
      playoffs = ifelse(gp > 82, 1, 0)
    ) |>
    select(team, playoffs)
  
  GFGA2223 <- pbp2223 |>
    filter(!is.na(event_team) & period < 5 & season_type == "R") |>
    group_by(team = event_team_abbr) |>
    summarise(
      gp = length(unique(game_id)),
      gf = sum(event_type == "GOAL"),
      .groups = "drop"
    ) |>
    left_join(
      pbp2223 |>
        filter(!is.na(event_team) & period < 5) |>
        mutate(team = ifelse(event_team_abbr == home_abbreviation, away_abbreviation, home_abbreviation)) |>
        group_by(team) |>
        summarise(
          ga = sum(event_type == "GOAL"),
          .groups = "drop"
        ),
      by = "team"
    ) 
  
  # Successful Pull Calcs
  en2223GameIDs <- enPBP2223$game_id
  success2223 <- pbp2223 %>%
    filter(game_id %in% en2223GameIDs,
           period == 3, 
           event_type == "PERIOD_END",
           gd == c("1", "2", "0")) 
  successCalc2223 <- success2223 |>
    mutate(successfulPullCalc = gd == 0) |>
    summarize(
      successfulPull = sum(successfulPullCalc, na.rm = TRUE),
      nonsuccessfulPull = sum(!successfulPullCalc, na.rm = TRUE),
    )
  
  # Joining filters
  points2223 <- left_join(GFGA2223, points2223, by = "team")
  playoffsGFGA2223 <- left_join(points2223, playoffs2223, by = "team")
  final2223 <- left_join(playoffsGFGA2223, optimal2223, by = c("team" = "event_team_abbr"))
  final2223$optimalPullPercent <- as.numeric(final2223$optimal_pulls/(final2223$optimal_pulls + final2223$non_optimal_pulls))
  final2223$gfa <- (final2223$gf/final2223$gp)
  final2223$gaa <- as.numeric(final2223$ga/final2223$gp)
  final2223$st_points <- as.numeric(final2223$st_points)
  final2223 <- final2223 %>% select(-gp)
}

## Analysis
# Formulas
{
  panelFormula <- st_points ~ optimalPullPercent + gfa + gaa | team
  logitFormula <- playoffs ~ optimalPullPercent + gfa + gaa | team
}

# Regressions and Histograms
{
allyears <- bind_rows(final1718, final1819, final1920, final21, final2122, final2223)
allSuccess <- bind_rows(successCalc1718, successCalc1819, successCalc1920, successCalc21, successCalc2122, successCalc2223)
successRate <- (sum(allSuccess$successfulPull)/sum(allSuccess$nonsuccessfulPull)) * 100
print(successRate)
# Summary stats
summary(allyears)
sdList <- list("gfa"=sd(allyears$gfa), "gaa"=sd(allyears$gaa), "optimal_pulls"=sd(allyears$optimal_pulls), 
                 "non_optimal_pulls"=sd(allyears$non_optimal_pulls), "optimalPullPercent"=sd(allyears$optimalPullPercent), 
                 "st_points"=sd(allyears$st_points), "playoffs"=sd(allyears$playoffs), 
                 "gf"=sd(allyears$gf), "ga"=sd(allyears$ga))
print(sdList)
  
# Panel Regression with Team Fixed Effects 
panelReg <- feols(panelFormula, data = allyears)
summary(panelReg)

# Logit Panel Regression
logitReg <- feglm(logitFormula, data = allyears)
summary(logitReg)
# Transform beta to odds ratio
exp(logitReg$coefficients)

## Histograms
#optimal Pulling Percentage
hist(allyears$optimalPullPercent, 
     main = "Histogram of Optimal Pull Percentage",
     xlab = "Optimal Pull Percentage",
     ylab = "Frequency",
     border = "white",  # Add a white border to bars for better visibility
     breaks = 20  # Adjust the number of bins as needed
)

playoffAvg <- allyears %>%
  filter(playoffs == 1) 
averagePointsQual <- mean(playoffAvg$st_points)

hist(allyears$st_points, 
     main = "Histogram of Standing Points",
     xlab = "Standing Points",
     ylab = "Frequency",
     border = "white",  
     breaks = 20,
)
abline(v = averagePointsQual, col = "black", lwd = 2)

# Normality Tests
ks.test(allyears$optimalPullPercent, "pnorm", mean = mean(allyears$optimalPullPercent), sd = sd(allyears$optimalPullPercent))
ks.test(allyears$st_points, "pnorm", mean = mean(allyears$st_points), sd = sd(allyears$st_points))
}
