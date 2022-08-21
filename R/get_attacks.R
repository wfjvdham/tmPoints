#' get_attacks
#'
#' @return df
#' @export
#'
#' @import dplyr
get_attacks <- function() {

  game_files <- list.files(system.file("games", package = "tm"))

  names(game_files) <- tools::file_path_sans_ext(game_files)

  attacks_df <- purrr::map_dfr(game_files, function(game_file) {

    game_object <- jsonlite::fromJSON(file.path("games", game_file))

    attacks_std <- game_object[["game"]][["gameLog"]] %>%
      select(message, data) %>%
      filter(
        message %in% c(
          "${0}'s ${1} amount ${2} by ${3} stolen by ${4}",
          "${0}'s ${1} amount ${2} by ${3} by ${4}"
        )
      ) %>%
      tidyr::unnest(data) %>%
      mutate(id = rep(1:(nrow(.) / 5), each = 5)) %>%
      mutate(name = rep(c(
        "target", "resource", "type", "amount", "source"
      ), nrow(.) / 5)) %>%
      tidyr::pivot_wider(id_cols = id) %>%
      filter(
        type == "decreased",
        source %in% c("blue", "black", "red", "yellow", "green")
      )

    animal_cards <- c(
      "Livestock", "Herbivores", "Penguins", "Fish", "Martian Zoo", "Birds"
    )
    microbe_cards <- c(
      "GHG Producing Bacteria", "Regolith Eaters", "Psychrophiles",
      "Nitrite Reducing Bacteria", "Decomposers", "Tardigrades"
    )

    attacks_ext <- game_object[["game"]][["gameLog"]] %>%
      select(message, data) %>%
      filter(
        message %in% c(
          "${0} removed ${1} resource(s) from ${2}'s ${3}"
        )
      ) %>%
      tidyr::unnest(data) %>%
      mutate(id = rep(1:(nrow(.) / 4), each = 4)) %>%
      mutate(name = rep(c(
        "source", "amount", "target", "card"
      ), nrow(.) / 4)) %>%
      tidyr::pivot_wider(id_cols = id) %>%
      filter(source != target) %>%
      mutate(
        resource = case_when(
          card %in% animal_cards ~ "animal",
          card %in% microbe_cards ~ "microbe",
          TRUE ~ "resource"
        )
      )

    attacks_std %>%
      bind_rows(attacks_ext) %>%
      select(-id, -type) %>%
      mutate(amount = as.numeric(amount))
  }, .id = "game_id")

  mapping_df <- tibble::tibble(
    player = c("Paul", "Rudo", "Wim", "Bas", "Dennis"),
    color = c("blue", "black", "red", "yellow", "green")
  )

  attacks_df <- attacks_df %>%
    left_join(mapping_df, by = c("target" = "color")) %>%
    select(-target) %>%
    rename(target = player)

  attacks_df <- attacks_df %>%
    left_join(mapping_df, by = c("source" = "color")) %>%
    select(-source) %>%
    rename(source = player)

  attacks_df
}
