#' Get Cards per player
#'
#' @return df
#' @export
#'
#' @import dplyr
get_cards <- function() {

  game_objects <- get_game_objects()
  #game_object <- game_objects[20][[1]]
  #game_object <- last(game_objects)

  purrr::map_dfr(game_objects, function(game_object) {

    tibble(
      cards = game_object[["game"]][["players"]][["playedCards"]],
      player = game_object[["game"]][["players"]][["name"]]
    ) %>%
      tidyr::unnest(cards)

  })
}
