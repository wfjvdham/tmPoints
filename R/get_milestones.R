#' Get milestones per player
#'
#' @return df
#' @export
#'
#' @import dplyr
get_milestones <- function() {

  game_objects <- get_game_objects()
  #game_object <- game_objects[20][[1]]
  #game_object <- last(game_objects)

  purrr::map_dfr(game_objects, function(game_object) {

    milestones_df <- tibble(game_object[["game"]][["claimedMilestones"]]) %>%
      rename(milestone = name)

    players_df <- tibble::tibble(
      playerId = game_object[["game"]][["players"]][["id"]],
      player = game_object[["game"]][["players"]][["name"]]
    )

    milestones_df %>%
      left_join(players_df) %>%
      select(player, milestone)
  })
}
