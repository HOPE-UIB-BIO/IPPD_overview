#' @title Obtain the table of depositional environments from Neotoma API
get_neotoma_depenv <- function() {
  # Get relevant tables with information from Neotoma API
  dep_envt_types_request <-
    httr2::request(
      "https://api.neotomadb.org/v2.0/dbtables/table?table=depenvttypes"
    )  %>% 
    httr2::req_url_query(limit = 10e3)

  dep_envt_types_responce <-
    httr2::req_perform(dep_envt_types_request)

  assertthat:::assert_that(
    httr2::resp_status(dep_envt_types_responce) == 200, # status 200 = success
    msg = "List of all depositional environments was NOT downloaded from Neotoma"
  )

  dep_envt_types_dwn <-
    httr2::resp_body_json(dep_envt_types_responce) %>%
    purrr::chuck("data")

  # extract all types of dep.env. into table
  dep_envt_types <-
    tibble::tibble(
      depenvtid = dep_envt_types_dwn$data %>%
        purrr::map_int("depenvtid"),
      depenvt = dep_envt_types_dwn$data %>%
        purrr::map_chr("depenvt"),
      depenvthigherid = dep_envt_types_dwn$data %>%
        purrr::map_int("depenvthigherid")
    ) %>%
    dplyr::rename(
      dep_env_id = depenvtid,
      dep_env = depenvt,
      higher_id = depenvthigherid
    )

  # Prepare DepEnvtTypes - rebuild table to filter information
  # This is needed as dep.env. is constructed in hierarchical structure
  dep_envt_types_transform <-
    dep_envt_types %>%
    dplyr::left_join(
      dep_envt_types,
      by = c("higher_id" = "dep_env_id"),
      suffix = c("", "_level_1")
    ) %>%
    dplyr::left_join(
      dep_envt_types,
      by = c("higher_id_level_1" = "dep_env_id"),
      suffix = c("", "_level_2")
    ) %>%
    dplyr::left_join(
      dep_envt_types,
      by = c("higher_id_level_2" = "dep_env_id"),
      suffix = c("", "_level_3")
    ) %>%
    dplyr::left_join(
      dep_envt_types,
      by = c("higher_id_level_3" = "dep_env_id"),
      suffix = c("", "_level_4")
    )  %>% 
    dplyr::select(
      dep_env,
      dep_env_level_1,
      dep_env_level_2,
      dep_env_level_3,
      dep_env_level_4
    )

  return(dep_envt_types_transform)
}
