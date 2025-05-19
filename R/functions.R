

#' add_bubble
#'
#' @param data dataframe containin ...
#' @param size variable containing size
#'
#' @return
#' @export
#'
add_bubble <- function(data, size = n) {
    ggplot2::geom_point(data = data,
                        ggplot2::aes(lon, lat, size = {{ size }}),
                        colour = "red", alpha = 0.2)
}
#' @export
p_capture <- function(NAME, st, bi, drop_zero = TRUE) {
  res <-
    st |>
    dplyr::select(.sid, glon, glat) |>
    dplyr::left_join(bi |>
                       dplyr::filter(name == NAME),
                     by = dplyr::join_by(.sid)) |>
    dplyr::mutate(n = ifelse(is.na(n), 0, 1)) |>
    dplyr::group_by(glon, glat) |>
    dplyr::summarise(n.st = dplyr::n(),
                     n.bi = sum(n),
                     p = n.bi / n.st,
                     .groups = "drop")
  if(drop_zero) {
    res <- res |> dplyr::filter(p > 0)
  }
  return(res)
}
