#' Get corps per player
#'
#' @return df
#' @export
#'
#' @import dplyr
get_corps <- function() {

  game_objects <- get_game_objects()

  purrr::map_dfr(game_objects, function(game_object) {

    picked_corp <- game_object[["game"]][["players"]][["pickedCorporationCard"]]
    choices <- game_object[["game"]][["players"]][["dealtCorporationCards"]]

    not_picked_corp <- purrr::map2(
      picked_corp, choices, function(p, ch) {
        ch[!ch == p]
    }) %>%
      unlist()

    rbind(
      tibble::tibble(
        corporation = picked_corp,
        chosen = 1
      ),
      tibble::tibble(
        corporation = not_picked_corp,
        chosen = 0
      )
    )
  })
}
