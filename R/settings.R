#' Set or get the VWorld API key
#'
#' Stores the key in the `ADMB_VWORLD_KEY` environment variable for the
#' current R session. The getter also falls back to `VWORLD_KEY`.
#'
#' To make the key persist across sessions, add the line
#' `ADMB_VWORLD_KEY=...` to your `~/.Renviron` file
#' (use `usethis::edit_r_environ()`). Do not write the key in scripts
#' you share.
#'
#' @param key Character. 브이월드(vworld.kr) 오픈API 인증키.
#'
#' @return Invisibly returns the key (or the empty string for the getter).
#'
#' @examples
#' \dontrun{
#' admb_set_vworld_key("XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX")
#' admb_get_vworld_key()
#' }
#'
#' @export
admb_set_vworld_key <- function(key) {
  if (is.null(key) || !is.character(key) ||
      length(key) != 1L || !nzchar(key)) {
    stop("`key` 는 비어있지 않은 문자열이어야 합니다.", call. = FALSE)
  }
  Sys.setenv(ADMB_VWORLD_KEY = key)
  invisible(key)
}

#' @rdname admb_set_vworld_key
#' @export
admb_get_vworld_key <- function() {
  v <- Sys.getenv("ADMB_VWORLD_KEY", unset = "")
  if (!nzchar(v)) v <- Sys.getenv("VWORLD_KEY", unset = "")
  v
}
