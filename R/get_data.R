#' get_data
#'
#' @return df
#' @export
#'
#' @import dplyr
get_data <- function() {
  screenshot <- tibble::tribble(
    ~game_id, ~player, ~score, ~gold, ~corporation,
    'S2G01', 'Dennis', 69, 41, 'EcoLine',
    'S2G01', 'Paul', 61, 45, 'Morning Star Inc.',
    'S2G01', 'Wim', 56, 50, 'Thorgate',
    'S2G01', 'Bas', 48, 35, 'Helion',
    'S2G01', 'Rudo', 39, 37, 'CrediCor'
  )

  game_files <- list.files(system.file("games", package = "tm"))

  names(game_files) <- tools::file_path_sans_ext(game_files)

  games_df <- purrr::map_dfr(game_files, function(game_file) {

    game_object <- jsonlite::fromJSON(file.path("games", game_file))

    scores <- game_object[["scores"]]

    players <- tibble::tibble(
      corporation = game_object[["game"]][["players"]][["corporationCard"]][["name"]],
      player = game_object[["game"]][["players"]][["name"]],
      gold = game_object[["game"]][["players"]][["terraformRating"]] +
        game_object[["game"]][["players"]][["terraformRatingAtGenerationStart"]] +
        game_object[["game"]][["players"]][["megaCreditProduction"]]
    )

    left_join(scores, players, by = "corporation") %>%
      select(player, score = playerScore, gold, corporation)
  }, .id = "game_id")

  games_df <- rbind(games_df, screenshot)

  games_df <- games_df %>%
    mutate(game_n = as.integer(stringr::str_sub(game_id, -2)) + 1)

  games_df <- games_df %>%
    arrange(game_n) %>%
    group_by(player) %>%
    mutate(average_points = cumsum(score) / game_n)

  games_df <- games_df %>%
    group_by(game_n) %>%
    arrange(gold) %>%
    mutate(rank = rank(score, ties.method = "first") - 1) %>%
    ungroup()

  games_df <- games_df %>%
    arrange(game_n) %>%
    group_by(player) %>%
    mutate(average_rank = cumsum(rank) / game_n)

  games_df <- games_df %>%
    mutate(magic_total = average_points * average_rank)

  games_df
}

