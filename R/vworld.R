# 브이월드 WFS 레이어 정의: 코드 컬럼과 코드 자릿수
.vw_layers <- data.frame(
  level    = c("sido", "sigungu", "emd", "ri"),
  name     = c("시도", "시군구", "읍면동", "리"),
  typename = c("lt_c_adsido_info", "lt_c_adsigg_info",
               "lt_c_ademd_info", "lt_c_adri_info"),
  code_col = c("ctprvn_cd", "sig_cd", "emd_cd", "li_cd"),
  code_len = c(2L, 5L, 8L, 10L),
  stringsAsFactors = FALSE
)

# 한 조건으로 받을 수 있는 최대 건수 (count <= 1000, startindex <= 1000)
.vw_page <- 1000L
.vw_max  <- 2000L

#' VWorld boundary layers
#'
#' @return A data frame with one row per supported level: `level`, `name`
#'   (Korean name), `typename` (WFS layer), `code_col` (district-code
#'   column) and `code_len` (number of digits in the code).
#'
#' @examples
#' admb_layers()
#'
#' @export
admb_layers <- function() .vw_layers

.vw_layer <- function(level) {
  i <- match(level, .vw_layers$level)
  if (length(level) != 1L || is.na(i)) {
    stop("`level` 은 ", paste0('"', .vw_layers$level, '"', collapse = ", "),
         " 중 하나여야 합니다.", call. = FALSE)
  }
  as.list(.vw_layers[i, ])
}

# 코드 앞자리(prefix)로 시작하는 모든 코드를 고르는 WFS 필터.
# 상한에 숫자가 아닌 글자("z")를 붙여 문자열 비교가 되게 한다. 상·하한을
# 모두 숫자로만 주면 서버가 숫자로 비교해 10자리 리 코드에서 0건이 나온다.
.vw_filter <- function(col, prefix) {
  lower <- if (nzchar(prefix)) prefix else "0"
  sprintf(paste0(
    "<ogc:Filter><ogc:PropertyIsBetween>",
    "<ogc:PropertyName>%s</ogc:PropertyName>",
    "<ogc:LowerBoundary><ogc:Literal>%s</ogc:Literal></ogc:LowerBoundary>",
    "<ogc:UpperBoundary><ogc:Literal>%s</ogc:Literal></ogc:UpperBoundary>",
    "</ogc:PropertyIsBetween></ogc:Filter>"), col, lower, paste0(prefix, "z"))
}

# 응답 GeoJSON 끝부분의 numberMatched 값
.vw_matched <- function(txt) {
  tail <- substring(txt, max(1L, nchar(txt) - 3000L))
  m <- regmatches(tail, regexpr('"numberMatched":[0-9]+', tail))
  if (!length(m)) stop("응답에서 건수(numberMatched)를 찾지 못했습니다.",
                       call. = FALSE)
  as.integer(sub(".*:", "", m))
}

# WFS GetFeature 1회 호출 → GeoJSON 문자열.
# props 를 주면 그 속성만(도형 없이) 받는다.
.vw_call <- function(lay, prefix, count, start, key, domain, srs, pause,
                     props = NULL) {
  resp <- httr2::request("https://api.vworld.kr/req/wfs") |>
    httr2::req_url_query(
      service = "WFS", version = "2.0.0", request = "GetFeature",
      typename = lay$typename, output = "application/json", srsname = srs,
      count = count, startindex = start, sortby = lay$code_col,
      filter = .vw_filter(lay$code_col, prefix),
      propertyname = if (!is.null(props)) paste(props, collapse = ","),
      key = key, domain = if (nzchar(domain)) domain) |>
    httr2::req_user_agent(.ua) |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
    httr2::req_timeout(600) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  txt <- httr2::resp_body_string(resp, encoding = "UTF-8")
  if (!grepl('"features"', txt, fixed = TRUE)) {
    msg <- regmatches(txt, regexpr("<ServiceException [^>]*>[^<]*", txt))
    msg <- if (length(msg)) sub("^<[^>]*>", "", msg) else substr(txt, 1, 300)
    stop("브이월드 오류 응답: ", msg, call. = FALSE)
  }
  if (pause > 0) Sys.sleep(pause)
  txt
}

# prefix 범위를 받되, 2000건이 넘으면 다음 자리 0~9 로 쪼개 재귀 호출
.vw_fetch <- function(lay, prefix, quiet, ..., read = .vw_read_sf) {
  n <- .vw_matched(.vw_call(lay, prefix, 1L, 0L, ...))
  if (n == 0L) return(list())
  if (n > .vw_max) {
    if (nchar(prefix) >= lay$code_len - 1L) {
      stop("코드 ", prefix, " 는 더 쪼갤 수 없습니다.", call. = FALSE)
    }
    return(do.call(c, lapply(0:9, function(d)
      .vw_fetch(lay, paste0(prefix, d), quiet, ..., read = read))))
  }
  if (!quiet) message(sprintf("%s [%s*] %d건", lay$name, prefix, n))
  lapply(seq(0L, n - 1L, by = .vw_page), function(s)
    read(.vw_call(lay, prefix, .vw_page, s, ...)))
}

.vw_read_sf <- function(txt) sf::st_read(txt, quiet = TRUE)
.vw_read_props <- function(txt) jsonlite::fromJSON(txt)$features$properties

.vw_check_key <- function(key) {
  if (!is.character(key) || length(key) != 1L || !nzchar(key)) {
    stop("브이월드 인증키가 없습니다. admb_set_vworld_key() 로 등록하세요.",
         call. = FALSE)
  }
}

.vw_check_code <- function(code, lay) {
  prefix <- if (is.null(code)) "" else as.character(code)
  if (length(prefix) != 1L || grepl("[^0-9]", prefix) ||
      nchar(prefix) > lay$code_len) {
    stop("`code` 는 ", lay$code_len, "자리 이하의 숫자 문자열이어야 합니다.",
         call. = FALSE)
  }
  prefix
}

#' List district codes and names from VWorld
#'
#' 도형 없이 구역 코드와 이름만 받는다. 빠르기 때문에 [admb_vworld()] 에
#' 넘길 `code` 를 찾을 때 쓴다.
#'
#' @inheritParams admb_vworld
#'
#' @return 데이터 프레임. `code`(구역 코드), `name`(한글 이름),
#'   `full_name`(상위 구역을 포함한 이름; 시도는 `name` 과 같음).
#'   해당 구역이 없으면 0행.
#'
#' @examples
#' \dontrun{
#' admb_codes("sido")
#' admb_codes("sigungu", code = "41")   # 경기도 시군구
#' }
#'
#' @export
admb_codes <- function(level = c("sido", "sigungu", "emd", "ri"),
                       code = NULL,
                       key = admb_get_vworld_key(),
                       domain = "",
                       pause = 0.2) {
  lay <- .vw_layer(if (missing(level)) "sido" else level)
  .vw_check_key(key)
  prefix <- .vw_check_code(code, lay)
  nm <- sub("_cd$", "_kor_nm", sub("^ctprvn", "ctp", lay$code_col))
  props <- c(lay$code_col, nm, if (lay$level != "sido") "full_nm")
  parts <- .vw_fetch(lay, substr(prefix, 1L, lay$code_len - 1L), TRUE,
                     key = key, domain = domain, srs = "EPSG:4326",
                     pause = pause, props = props, read = .vw_read_props)
  empty <- data.frame(code = character(), name = character(),
                      full_name = character(), stringsAsFactors = FALSE)
  if (!length(parts)) return(empty)
  x <- do.call(rbind, parts)
  out <- data.frame(
    code = as.character(x[[lay$code_col]]),
    name = as.character(x[[nm]]),
    full_name = as.character(if (lay$level == "sido") x[[nm]] else x$full_nm),
    stringsAsFactors = FALSE)
  out <- out[!duplicated(out$code) & startsWith(out$code, prefix), ]
  out <- out[order(out$code), ]
  rownames(out) <- NULL
  out
}

#' Download legal-district boundaries from VWorld
#'
#' Fetches boundary polygons from the VWorld WFS service
#' (공공데이터포털 15059008 「국토교통부_행정구역도(WMS/WFS)」) and returns
#' them as an `sf` object. `admb_vworld_shp()` also writes a shapefile.
#'
#' `level` 없이 `admb_vworld()` 만 실행하면 선택창이 차례로 뜬다:
#' **경계 종류**(시도·시군구·읍면동·리) → **시·도**(전국 또는 하나) →
#' **시·군·구**(전체 또는 하나) → **저장 방식**(R 객체만 / SHP 파일로도 저장).
#' `level` 을 주면 선택창 없이 바로 받는다.
#'
#' 서버가 한 조건에 최대 2,000건만 돌려주므로, 법정동 코드 범위로 조회하고
#' 2,000건이 넘는 범위는 코드 앞자리를 한 자리씩 늘려 자동으로 나눠 받는다.
#' 전국 `"ri"` 는 10분 이상 걸릴 수 있다.
#'
#' 일부 도형은 `sf::st_is_valid()` 가 `FALSE` 이다. 면적·중첩 계산에 쓸
#' 때는 `make_valid = TRUE` 를 권장한다.
#'
#' @param level 경계 단위. `"sido"`(시도), `"sigungu"`(시군구),
#'   `"emd"`(읍면동), `"ri"`(리) 중 하나. [admb_layers()] 참고.
#'   `NULL`(기본값)이면 선택창으로 고른다.
#' @param code 법정동 코드 앞자리(숫자 문자열). 주면 그 코드로 시작하는
#'   구역만 받는다. 예: `"11"` 서울, `"41135"` 성남시 분당구. `NULL` 이면 전국.
#'   [admb_codes()] 로 코드를 찾을 수 있다.
#' @param key 브이월드 인증키. 기본값은 [admb_get_vworld_key()].
#' @param domain 인증키 발급 때 등록한 URL. 보통 비워 둔다.
#' @param crs 좌표계 EPSG 번호. 기본 `4326`.
#' @param make_valid `TRUE` 면 [sf::st_make_valid()] 를 적용한다.
#' @param quiet `TRUE` 면 진행 메시지를 숨긴다.
#' @param pause 호출 사이 대기 시간(초).
#'
#' @return `admb_vworld()` 는 `sf` 객체. 구역 코드 컬럼(`ctprvn_cd`,
#'   `sig_cd`, `emd_cd`, `li_cd` 중 하나)과 한글·영문 이름을 담는다.
#'   해당 구역이 없거나 선택창에서 취소하면 `NULL`. 선택창으로 받은
#'   결과와 `admb_vworld_shp()` 의 결과는 보이지 않게 반환한다.
#'
#' @examples
#' \dontrun{
#' admb_set_vworld_key("MY_VWORLD_KEY")
#' x <- admb_vworld()            # 선택창으로 고르기
#' sido <- admb_vworld("sido")
#' seoul_emd <- admb_vworld("emd", code = "11")
#' admb_vworld_shp("sigungu", "boundary/sigungu.shp")
#' }
#'
#' @export
admb_vworld <- function(level = NULL,
                        code = NULL,
                        key = admb_get_vworld_key(),
                        domain = "",
                        crs = 4326,
                        make_valid = FALSE,
                        quiet = FALSE,
                        pause = 0.2) {
  .vw_check_key(key)
  shp_path <- NULL
  by_menu <- is.null(level)
  if (by_menu) {
    sel <- .menu_select(code, key = key, domain = domain, pause = pause)
    if (is.null(sel)) return(invisible(NULL))
    level <- sel$level; code <- sel$code; shp_path <- sel$path
  }
  lay <- .vw_layer(level)
  prefix <- .vw_check_code(code, lay)
  # 코드 전체 자릿수를 하한으로 주면 서버 비교가 틀어지므로, 조회는 한 자리
  # 짧게 하고 결과에서 다시 거른다.
  query <- substr(prefix, 1L, lay$code_len - 1L)
  parts <- .vw_fetch(lay, query, quiet, key = key, domain = domain,
                     srs = paste0("EPSG:", crs), pause = pause)
  if (!length(parts)) return(NULL)
  res <- do.call(rbind, parts)
  res <- res[!duplicated(res[[lay$code_col]]) &
               startsWith(as.character(res[[lay$code_col]]), prefix), ]
  if (!nrow(res)) return(NULL)
  res <- sf::st_set_crs(res, crs)
  if (make_valid) res <- sf::st_make_valid(res)
  if (!is.null(shp_path)) .write_shp(res, shp_path)
  if (by_menu) invisible(res) else res
}

.write_shp <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(x, path, layer_options = "ENCODING=UTF-8",
               delete_layer = TRUE, quiet = TRUE)
  message("저장: ", path, " (", nrow(x), "건)")
}

#' @rdname admb_vworld
#' @param path 저장할 `.shp` 파일 경로. 폴더가 없으면 만든다. 같은 이름의
#'   파일은 덮어쓴다.
#' @param ... `admb_vworld()` 로 넘길 인자.
#' @export
admb_vworld_shp <- function(level, path, ...) {
  x <- admb_vworld(level, ...)
  if (is.null(x)) stop("받은 구역이 없어 저장하지 않았습니다.", call. = FALSE)
  .write_shp(x, path)
  invisible(x)
}
