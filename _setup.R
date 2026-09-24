library(tidyverse)
library(reactable)
library(htmltools)
library(leaflet)
library(plotly)

# Load tournament data
all_matches <- read_csv("data/naia_matches_all_years.csv", show_col_types = FALSE)
schools_geo <- read_csv("data/naia_schools_geo.csv", show_col_types = FALSE)

# Color badge helper for win method
badge_win_method <- \(value) {
  if (is.na(value) || value == "") return("")
  val <- tolower(as.character(value))
  col <- if (grepl("fall|vfa", val)) "#dc2626" else if (grepl("tf|tech|vsu", val)) "#2563eb" else if (grepl("dec|vpo", val)) "#475569" else "#64748b"
  bg <- if (grepl("fall|vfa", val)) "#fee2e2" else if (grepl("tf|tech|vsu", val)) "#dbeafe" else if (grepl("dec|vpo", val)) "#f1f5f9" else "#f8fafc"
  tags$span(
    style = sprintf("display: inline-block; padding: 2px 8px; border-radius: 9999px; font-size: 0.75rem; font-weight: 700; background: %s; color: %s; border: 1px solid %s33;", bg, col, col),
    value
  )
}

# Badges to emphasize champions and podium medalists
badge_champion <- \(value) {
  if (is.na(value) || value == "") return("")
  tags$span(
    style = "display: inline-block; font-weight: 800; color: #92400e; background: #fef3c7; border: 1.5px solid #f59e0b; border-radius: 8px; padding: 3px 10px;",
    paste0("🥇 ", value)
  )
}

badge_runner_up <- \(value) {
  if (is.na(value) || value == "") return("")
  tags$span(
    style = "display: inline-block; font-weight: 700; color: #334155; background: #f1f5f9; border: 1px solid #cbd5e1; border-radius: 8px; padding: 3px 8px;",
    paste0("🥈 ", value)
  )
}

badge_place <- \(value) {
  if (is.na(value) || value == "") return("")
  if (grepl("1st", value)) {
    tags$span(
      style = "display: inline-block; font-weight: 800; color: #92400e; background: #fef3c7; border: 1.5px solid #f59e0b; border-radius: 9999px; padding: 3px 12px; font-size: 0.85rem;",
      value
    )
  } else if (grepl("2nd", value)) {
    tags$span(
      style = "display: inline-block; font-weight: 700; color: #334155; background: #f1f5f9; border: 1.5px solid #cbd5e1; border-radius: 9999px; padding: 2px 10px; font-size: 0.85rem;",
      value
    )
  } else if (grepl("3rd", value)) {
    tags$span(
      style = "display: inline-block; font-weight: 700; color: #9a3412; background: #ffedd5; border: 1.5px solid #fdba74; border-radius: 9999px; padding: 2px 10px; font-size: 0.85rem;",
      value
    )
  } else {
    tags$span(
      style = "display: inline-block; font-weight: 600; color: #475569; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 9999px; padding: 2px 8px; font-size: 0.8rem;",
      value
    )
  }
}

badge_state <- \(value) {
  if (is.na(value) || value == "") return("")
  tags$span(
    style = "display: inline-block; padding: 2px 8px; border-radius: 9999px; font-size: 0.75rem; font-weight: 700; background: #eff6ff; color: #1d4ed8; border: 1px solid #bfdbfe;",
    value
  )
}

badge_champ_count <- \(value) {
  if (value > 0) {
    tags$span(
      style = "display: inline-block; font-weight: 800; color: #92400e; background: #fef3c7; border: 1px solid #fde68a; border-radius: 6px; padding: 2px 8px;",
      paste0("👑 ", value)
    )
  } else {
    tags$span(style = "color: #94a3b8;", "0")
  }
}

badge_aa_count <- \(value) {
  if (value > 0) {
    tags$span(
      style = "display: inline-block; font-weight: 700; color: #0f172a; background: #f1f5f9; border: 1px solid #cbd5e1; border-radius: 6px; padding: 2px 8px;",
      paste0("🎖️ ", value)
    )
  } else {
    tags$span(style = "color: #94a3b8;", "0")
  }
}

# Theme for the tables
tbl_theme <- reactableTheme(
  borderColor = "#e2e8f0",
  stripedColor = "#f8fafc",
  highlightColor = "#eff6ff",
  cellPadding = "8px 12px",
  headerStyle = list(
    backgroundColor = "#f1f5f9",
    borderBottom = "2px solid #cbd5e1",
    fontWeight = 700,
    fontSize = "0.8rem",
    letterSpacing = "0.04em",
    textTransform = "uppercase",
    color = "#0f172a"
  )
)

# Extract podium placements from medal rounds across all years
is_1st <- all_matches$round == "Finals" | grepl("^1st Place", all_matches$round)
is_3rd <- grepl("^3rd Place", all_matches$round)
is_5th <- grepl("^5th Place", all_matches$round)
is_7th <- grepl("^7th Place", all_matches$round)

placements <- bind_rows(
  all_matches[is_1st, ] |> transmute(year, weight_class, wrestler = winner, place = "👑 Champion"),
  all_matches[is_1st, ] |> transmute(year, weight_class, wrestler = loser, place = "🥈 2nd Place"),
  all_matches[is_3rd, ] |> transmute(year, weight_class, wrestler = winner, place = "🥉 3rd Place"),
  all_matches[is_3rd, ] |> transmute(year, weight_class, wrestler = loser, place = "🏅 4th Place"),
  all_matches[is_5th, ] |> transmute(year, weight_class, wrestler = winner, place = "🏅 5th Place"),
  all_matches[is_5th, ] |> transmute(year, weight_class, wrestler = loser, place = "🏅 6th Place"),
  all_matches[is_7th, ] |> transmute(year, weight_class, wrestler = winner, place = "🏅 7th Place"),
  all_matches[is_7th, ] |> transmute(year, weight_class, wrestler = loser, place = "🏅 8th Place")
) |> distinct(year, weight_class, wrestler, .keep_all = TRUE)

placer_counts <- placements |>
  count(wrestler, name = "times_placed")

# Normalize school names
map_to_geo <- function(x) {
  s <- str_remove_all(x, "\\s*\\([A-Za-z0-9\\.\\s]+\\)")
  s <- str_trim(s)
  case_when(
    grepl("^Life Pacific", s) ~ NA_character_,
    grepl("^Life", s) ~ "Life University",
    grepl("^Southern Oregon", s) ~ "Southern Oregon University",
    grepl("^Cumberlands|^University of Cumberlands", s) ~ "University of the Cumberlands",
    grepl("^Grand View", s) ~ "Grand View University",
    grepl("^Campbellsville", s) ~ "Campbellsville University",
    grepl("^Providence", s) ~ "University of Providence",
    grepl("^Missouri Valley", s) ~ "Missouri Valley College",
    grepl("^Oklahoma City", s) ~ "Oklahoma City University",
    grepl("^Menlo", s) ~ "Menlo College",
    grepl("^William Penn", s) ~ "William Penn University",
    grepl("^Indiana Tech", s) ~ "Indiana Tech",
    grepl("^Wayland Baptist|^Oviance", s) ~ "Wayland Baptist University",
    grepl("^Ottawa", s) ~ "Ottawa University",
    grepl("^Eastern Oregon", s) ~ "Eastern Oregon University",
    grepl("^Texas Wesleyan", s) ~ "Texas Wesleyan University",
    grepl("^Jamestown", s) ~ "University of Jamestown",
    grepl("^Missouri Baptist", s) ~ "Missouri Baptist University",
    grepl("^Hastings", s) ~ "Hastings College",
    grepl("^Baker", s) ~ "Baker University",
    grepl("^Midland", s) ~ "Midland University",
    grepl("^Lyon", s) ~ "Lyon College",
    grepl("^Saint Mary|^St. Mary|^University of St", s) ~ "University of Saint Mary",
    grepl("^Waldorf", s) ~ "Waldorf University",
    grepl("^Lindsey Wilson", s) ~ "Lindsey Wilson College",
    grepl("^Doane", s) ~ "Doane University",
    grepl("^York", s) ~ "York University",
    grepl("^Central Methodist", s) ~ "Central Methodist University",
    grepl("^Brewton", s) ~ "Brewton-Parker College",
    grepl("^Corban", s) ~ "Corban University",
    grepl("^Avila", s) ~ "Avila University",
    grepl("^Central Christian", s) ~ "Central Christian College",
    grepl("^Dickinson", s) ~ "Dickinson State University",
    grepl("^Friends", s) ~ "Friends University",
    grepl("^Evergreen", s) ~ "Evergreen State College",
    grepl("^Lourdes", s) ~ "Lourdes University",
    grepl("^Vanguard", s) ~ "Vanguard University",
    grepl("^Simpson", s) ~ "Simpson University",
    grepl("^Bismarck", s) ~ "Bismarck State College",
    grepl("^St. Andrews", s) ~ "St. Andrews University",
    grepl("^Westcliff", s) ~ "Westcliff University",
    grepl("^William Woods", s) ~ "William Woods University",
    grepl("^Montreat", s) ~ "Montreat College",
    grepl("^Siena Heights", s) ~ "Siena Heights University",
    grepl("^Arizona Christian", s) ~ "Arizona Christian University",
    grepl("^Dakota Wesleyan", s) ~ "Dakota Wesleyan University",
    grepl("^Georgetown", s) ~ "Georgetown College",
    grepl("^Jarvis Christian", s) ~ "Jarvis Christian University",
    grepl("^Rio Grande", s) ~ "University of Rio Grande",
    grepl("^Rochester", s) ~ "Rochester Christian University",
    grepl("^Iowa Wesleyan", s) ~ "Iowa Wesleyan University",
    grepl("^Lincoln", s) ~ "Lincoln College",
    grepl("^Warner Pacific", s) ~ "Warner Pacific University",
    TRUE ~ s
  )
}

# Records
records_df <- bind_rows(
  all_matches |> transmute(year, weight_class, wrestler = winner, res = "W"),
  all_matches |> transmute(year, weight_class, wrestler = loser, res = "L")
) |>
  filter(!is.na(wrestler), wrestler != "") |>
  group_by(year, weight_class, wrestler) |>
  summarize(
    record = paste0(sum(res == "W"), "-", sum(res == "L")),
    .groups = "drop"
  )

fmt_bout <- function(lead, opp, score, method) {
  sc <- if_else(is.na(score) | score == "", method, score)
  paste0(lead, " ", opp, " (", sc, ")")
}

# National Placers dataset
placers_df <- bind_rows(
  all_matches[is_1st, ] |> transmute(year, weight_class, place_num = 1, place = "🥇 1st Place", wrestler = winner, school = map_to_geo(winner_school), medal_bout = fmt_bout("Def.", loser, score, win_method)),
  all_matches[is_1st, ] |> transmute(year, weight_class, place_num = 2, place = "🥈 2nd Place", wrestler = loser, school = map_to_geo(loser_school), medal_bout = fmt_bout("Finalist vs.", winner, score, win_method)),
  all_matches[is_3rd, ] |> transmute(year, weight_class, place_num = 3, place = "🥉 3rd Place", wrestler = winner, school = map_to_geo(winner_school), medal_bout = fmt_bout("Def.", loser, score, win_method)),
  all_matches[is_3rd, ] |> transmute(year, weight_class, place_num = 4, place = "4th Place", wrestler = loser, school = map_to_geo(loser_school), medal_bout = fmt_bout("vs.", winner, score, win_method)),
  all_matches[is_5th, ] |> transmute(year, weight_class, place_num = 5, place = "5th Place", wrestler = winner, school = map_to_geo(winner_school), medal_bout = fmt_bout("Def.", loser, score, win_method)),
  all_matches[is_5th, ] |> transmute(year, weight_class, place_num = 6, place = "6th Place", wrestler = loser, school = map_to_geo(loser_school), medal_bout = fmt_bout("vs.", winner, score, win_method)),
  all_matches[is_7th, ] |> transmute(year, weight_class, place_num = 7, place = "7th Place", wrestler = winner, school = map_to_geo(winner_school), medal_bout = fmt_bout("Def.", loser, score, win_method)),
  all_matches[is_7th, ] |> transmute(year, weight_class, place_num = 8, place = "8th Place", wrestler = loser, school = map_to_geo(loser_school), medal_bout = fmt_bout("vs.", winner, score, win_method))
) |>
  distinct(year, weight_class, place_num, .keep_all = TRUE) |>
  left_join(records_df, by = c("year", "weight_class", "wrestler")) |>
  mutate(record = replace_na(record, "—")) |>
  arrange(desc(year), weight_class, place_num)

# Yearly participation growth
growth_df <- bind_rows(
  all_matches |> transmute(year, wrestler = winner, school = winner_school),
  all_matches |> transmute(year, wrestler = loser, school = loser_school)
) |>
  filter(!is.na(wrestler), wrestler != "") |>
  group_by(year) |>
  summarize(
    Athletes = n_distinct(wrestler),
    Colleges = n_distinct(school[school != "" & school != "Unknown"]),
    .groups = "drop"
  )

matches_norm <- all_matches |>
  mutate(
    norm_winner_school = map_to_geo(winner_school),
    norm_loser_school = map_to_geo(loser_school)
  )

all_school_placers <- bind_rows(
  matches_norm[is_1st, ] |> transmute(year, weight_class, place = "1st Place", wrestler = winner, school = norm_winner_school),
  matches_norm[is_1st, ] |> transmute(year, weight_class, place = "2nd Place", wrestler = loser, school = norm_loser_school),
  matches_norm[is_3rd, ] |> transmute(year, weight_class, place = "3rd Place", wrestler = winner, school = norm_winner_school),
  matches_norm[is_3rd, ] |> transmute(year, weight_class, place = "4th Place", wrestler = loser, school = norm_loser_school),
  matches_norm[is_5th, ] |> transmute(year, weight_class, place = "5th Place", wrestler = winner, school = norm_winner_school),
  matches_norm[is_5th, ] |> transmute(year, weight_class, place = "6th Place", wrestler = loser, school = norm_loser_school),
  matches_norm[is_7th, ] |> transmute(year, weight_class, place = "7th Place", wrestler = winner, school = norm_winner_school),
  matches_norm[is_7th, ] |> transmute(year, weight_class, place = "8th Place", wrestler = loser, school = norm_loser_school)
) |>
  filter(!is.na(school), school != "")

school_placers_summary <- all_school_placers |>
  group_by(school) |>
  summarize(
    national_champions = sum(grepl("1st", place)),
    all_americans = n(),
    .groups = "drop"
  )

school_bouts_summary <- bind_rows(
  matches_norm |> transmute(school = norm_winner_school, result = "Win"),
  matches_norm |> transmute(school = norm_loser_school, result = "Loss")
) |>
  filter(!is.na(school), school != "") |>
  group_by(school) |>
  summarize(
    total_bouts = n(),
    bouts_won = sum(result == "Win"),
    win_pct = paste0(round(bouts_won / total_bouts * 100), "%"),
    .groups = "drop"
  )

school_legacy_table <- schools_geo |>
  left_join(school_placers_summary, by = "school") |>
  left_join(school_bouts_summary, by = "school") |>
  mutate(
    national_champions = replace_na(national_champions, 0),
    all_americans = replace_na(all_americans, 0),
    bouts_won = replace_na(bouts_won, 0),
    total_bouts = replace_na(total_bouts, 0),
    win_pct = replace_na(win_pct, "0%")
  ) |>
  arrange(desc(national_champions), desc(all_americans), desc(bouts_won))

# Roster
roster_df <- bind_rows(
  all_matches |> transmute(year, weight_class, wrestler = winner, school = winner_school, result = "Won"),
  all_matches |> transmute(year, weight_class, wrestler = loser, school = loser_school, result = "Lost")
) |>
  filter(!is.na(wrestler), wrestler != "") |>
  group_by(year, weight_class, wrestler) |>
  summarize(
    school = {
      s <- na.omit(school)
      s <- s[s != "" & s != "Unknown"]
      if (length(s) > 0) s[length(s)] else "—"
    },
    wins = sum(result == "Won"),
    losses = sum(result == "Lost"),
    record = paste0(wins, "-", losses),
    .groups = "drop"
  ) |>
  left_join(placements, by = c("year", "weight_class", "wrestler")) |>
  mutate(place = replace_na(place, "Competitor")) |>
  arrange(desc(year), weight_class, desc(wins))

total_matches <- format(nrow(all_matches), big.mark = ",")
total_tournaments <- n_distinct(all_matches$year)
total_wrestlers <- format(n_distinct(c(all_matches$winner, all_matches$loser)), big.mark = ",")
