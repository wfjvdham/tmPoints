#' get_attacks
#'
#' @return df
#' @export
#'
#' @import dplyr
get_attacks <- function() {

  game_objects <- get_game_objects()
  #game_object <- game_objects[20][[1]]
  #game_object <- last(game_objects)

  attacks_df <- purrr::map_dfr(game_objects, function(game_object) {

    attacks_std <- game_object[["game"]][["gameLog"]] %>%
      select(message, data) %>%
      filter(
        message %in% c(
          "${0}'s ${1} amount ${2} by ${3} stolen by ${4}",
          "${0}'s ${1} amount ${2} by ${3} by ${4}",
          "${0}'s ${1} production ${2} by ${3} stolen by ${4}",
          "${0}'s ${1} production ${2} by ${3} by ${4}"
        )
      ) %>%
      tidyr::unnest(data) %>%
      mutate(id = rep(1:(nrow(.) / 5), each = 5)) %>%
      mutate(name = rep(c(
        "target", "resource", "type", "amount", "source"
      ), nrow(.) / 5)) %>%
      tidyr::pivot_wider(id_cols = c(id, message)) %>%
      filter(
        type == "decreased",
        source %in% c("blue", "black", "red", "yellow", "green")
      ) %>%
      mutate(
        resource = case_when(
          stringr::str_detect(message, "production") ~ paste(resource, "production"),
          TRUE ~ resource
        )
      ) %>%
      select(-message)

    animal_cards <- c(
      "Livestock", "Herbivores", "Penguins", "Fish", "Martian Zoo", "Birds",
      "Small Animals", "Anthozoa", "Stratospheric Birds", "Venusian Animals",
      "Predators", "Ecological Zone"
    )
    microbe_cards <- c(
      "GHG Producing Bacteria", "Regolith Eaters", "Psychrophiles",
      "Nitrite Reducing Bacteria", "Decomposers", "Tardigrades", "Ants",
      "Thermophiles", "Sulphur-Eating Bacteria", "Extremophiles",
      "Rust Eating Bacteria", "Venusian Insects", "Darkside Incubation Plant",
      "Recyclon"
    )

    attacks_ext <- game_object[["game"]][["gameLog"]] %>%
      select(message, data) %>%
      filter(
        message %in% c(
          "${0} removed ${1} resource(s) from ${2}'s ${3}"
        )
      ) %>%
      tidyr::unnest(data)

    if (nrow(attacks_ext) > 0) {
      attacks_ext <- attacks_ext %>%
        mutate(id = rep(1:(nrow(.) / 4), each = 4)) %>%
        mutate(name = rep(c(
          "source", "amount", "target", "card"
        ), nrow(.) / 4)) %>%
        tidyr::pivot_wider(id_cols = c(id, message)) %>%
        filter(source != target) %>%
        mutate(
          resource = case_when(
            card %in% animal_cards ~ "animal",
            card %in% microbe_cards ~ "microbe",
            TRUE ~ card
          )
        )
    }

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
    rename(source = player) %>%
    mutate(
      game_n = as.integer(stringr::str_sub(game_id, -2)),
      season_n = as.integer(stringr::str_sub(game_id, 2, 2))
    )

  attacks_df <- attacks_df %>%
    mutate(
      resource_factor = case_when(
        resource == "plants" ~ 2,
        resource == "microbe" ~ 2,
        resource == "megacredits" ~ 1,
        resource == "steel" ~ 2,
        resource == "heat production" ~ 6,
        resource == "plants production" ~ 10,
        resource == "titanium production" ~ 10,
        resource == "energy production" ~ 7,
        resource == "animal" ~ 3,
        resource == "titanium" ~ 3,
        resource == "megacredits production" ~ 5,
        resource == "steel production" ~ 8,
        resource == "heat" ~ 0.5
      ),
      amount_factor = amount * resource_factor
    )

  attacks_df
}
