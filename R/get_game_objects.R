#' Get all the game object from the files
#'
#' @return list
#' @export
get_game_objects <- function() {

  game_files <- list.files(system.file("games", package = "tm"))

  names(game_files) <- tools::file_path_sans_ext(game_files)

  game_objects <- purrr::map(game_files, function(game_file) {

    file_path <- file.path(system.file("games", package = "tm"), game_file)

    game_object <- jsonlite::fromJSON(file_path)

    game_object[["game"]][["players"]][["name"]] <- purrr::map_chr(
      game_object[["game"]][["players"]][["name"]],
      stringr::str_trim
    )

    game_object
  })

  game_objects
}
