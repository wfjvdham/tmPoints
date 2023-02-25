#' get_data
#'
#' @return df
#' @export
#'
#' @import dplyr
get_data <- function() {
  screenshot <- tibble::tribble(
    ~game_id, ~player, ~score, ~gold, ~corporation, ~start_order,
    'S2G01', 'Dennis', 69, 41, 'EcoLine', -1,
    'S2G01', 'Paul', 61, 45, 'Morning Star Inc.', -1,
    'S2G01', 'Wim', 56, 50, 'Thorgate', -1,
    'S2G01', 'Bas', 48, 35, 'Helion', -1,
    'S2G01', 'Rudo', 39, 37, 'CrediCor', -1
  )

  games_df <- purrr::map_dfr(get_game_objects(), function(game_object) {

    # game_object <- last(get_game_objects())

    first_player_last_gen_id <- game_object[["game"]][["first"]]
    which(game_object[["game"]][["players"]][["id"]] == first_player_last_gen_id)
    shift <- (game_object[["generations"]] - 1) %% 5
    last_gen_order <- 1:5 - which(
      game_object[["game"]][["players"]][["id"]] == first_player_last_gen_id
    ) + shift

    scores <- game_object[["scores"]] %>%
      mutate(corporation = sub("\\|.*", "", corporation))

    corporation <- game_object[["game"]][["players"]][["corporationCard"]][["name"]]
    if (is.null(corporation)) {
      corporation <- game_object[["game"]][["players"]][["pickedCorporationCard"]]
      # corporation <- purrr::map_chr(
      #   game_object[["game"]][["players"]][["corporations"]], "name"
      # )
    }

    players <- tibble::tibble(
      corporation = corporation,
      player = game_object[["game"]][["players"]][["name"]],
      start_order = last_gen_order %% 5,
      gold = game_object[["game"]][["players"]][["terraformRating"]] +
        game_object[["game"]][["players"]][["terraformRatingAtGenerationStart"]] +
        game_object[["game"]][["players"]][["megaCreditProduction"]]
    ) %>%
      mutate(player = stringr::str_trim(player))

    left_join(scores, players, by = "corporation") %>%
      select(
        player, score = playerScore, gold, corporation, start_order
      )
  }, .id = "game_id")

  games_df <- bind_rows(games_df, screenshot)

  games_df <- games_df %>%
    # Why?
    filter(game_id != "S2G00") %>%
    mutate(
      game_n = as.integer(stringr::str_sub(game_id, -2)),
      season_n = as.integer(stringr::str_sub(game_id, 2, 2))
    )

  games_df <- games_df %>%
    group_by(game_id) %>%
    mutate(
      score_org = score,
      score = scales::rescale(score, to = c(0, 10), from = c(0, max(score)))
    )

  games_df <- games_df %>%
    group_by(season_n, game_n) %>%
    arrange(gold) %>%
    mutate(
      rank = rank(score, ties.method = "first"),
      rank = scales::rescale(rank, to = c(0, 10))
    ) %>%
    ungroup()

  games_df <- games_df %>%
    group_by(season_n, player) %>%
    arrange(game_n) %>%
    mutate(
      average_points = cumsum(score),
      start_order = start_order + 1,
      average_start_order = cumsum(start_order) / game_n,
      average_rank = cumsum(rank)
    )

  colors_df <- tibble::tibble(
    player = c("Bas", "Dennis", "Paul", "Rudo", "Wim"),
    col = c("#AAAA00", "#009900", "#0066FF", "#AAAAAA", "#991100")
  )

  games_df <- games_df %>%
    dplyr::left_join(colors_df, by = "player")

  games_df %>%
    ungroup()
}

