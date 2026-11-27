# 행정구역 경계 SHP 자동 다운로드
#  방법 A) 브이월드 WFS (data.go.kr 15059008)  -> 법정구역(시도/시군구/읍면동/리), 인증키 필요
#  방법 B) data.go.kr 파일데이터 15129688 (SGIS) -> 행정구역(행정동) SHP 원본, 키·로그인 불필요
#
# 필요 패키지: sf, httr2, jsonlite

library(sf)
library(httr2)

UA <- "Mozilla/5.0 (R httr2)"

# ───────────────────────── 방법 A: 브이월드 WFS → SHP ─────────────────────────
# typename: lt_c_adsido_info(시도) / lt_c_adsigg_info(시군구) /
#           lt_c_ademd_info(읍면동) / lt_c_adri_info(리)
# 서버 제한: 1회 최대 1000건, startindex 는 1000 이하 → 한 조건으로 최대 2000건.
# 그래서 코드 범위(PropertyIsBetween)로 조회하고, 2000건이 넘으면 코드 앞자리를
# 한 자리씩 늘려 자동으로 쪼개 받는다.
.vw_layers <- list(
  lt_c_adsido_info = c(col = "ctprvn_cd", len = 2),
  lt_c_adsigg_info = c(col = "sig_cd",    len = 5),
  lt_c_ademd_info  = c(col = "emd_cd",    len = 8),
  lt_c_adri_info   = c(col = "li_cd",     len = 10))

vworld_wfs <- function(typename,
                       key    = Sys.getenv("VWORLD_KEY"),
                       domain = Sys.getenv("VWORLD_DOMAIN"),  # 필요할 때만
                       srs    = "EPSG:4326", pause = 0.2) {
  if (!nzchar(key)) stop("VWORLD_KEY 환경변수(브이월드 인증키)가 없습니다.")
  lay <- .vw_layers[[typename]]
  if (is.null(lay)) stop("지원하지 않는 typename: ", typename)
  col <- lay[["col"]]; len <- as.integer(lay[["len"]])

  call_wfs <- function(prefix, count, start) {
    pad <- function(ch) paste0(prefix, strrep(ch, len - nchar(prefix)))
    flt <- sprintf(paste0(
      "<ogc:Filter><ogc:PropertyIsBetween><ogc:PropertyName>%s</ogc:PropertyName>",
      "<ogc:LowerBoundary><ogc:Literal>%s</ogc:Literal></ogc:LowerBoundary>",
      "<ogc:UpperBoundary><ogc:Literal>%s</ogc:Literal></ogc:UpperBoundary>",
      "</ogc:PropertyIsBetween></ogc:Filter>"), col, pad("0"), pad("9"))
    resp <- request("https://api.vworld.kr/req/wfs") |>
      req_url_query(service = "WFS", version = "2.0.0", request = "GetFeature",
                    typename = typename, output = "application/json",
                    srsname = srs, count = count, startindex = start,
                    sortby = col, filter = flt,
                    key = key, domain = if (nzchar(domain)) domain) |>
      req_user_agent(UA) |> req_retry(max_tries = 3) |> req_timeout(300) |>
      req_error(is_error = \(r) FALSE) |> req_perform()
    txt <- resp_body_string(resp, encoding = "UTF-8")
    if (!grepl('"features"', txt, fixed = TRUE))
      stop("브이월드 오류 응답: ", substr(txt, 1, 500))
    Sys.sleep(pause)
    txt
  }
  matched <- function(txt)
    as.integer(sub('.*"numberMatched":([0-9]+).*', "\\1", substring(txt, nchar(txt) - 2000)))

  fetch <- function(prefix) {
    n <- matched(call_wfs(prefix, 1, 0))
    if (n == 0) return(list())
    if (n > 2000) {
      if (nchar(prefix) >= len) stop("더 쪼갤 수 없습니다: ", prefix)
      return(do.call(c, lapply(0:9, function(d) fetch(paste0(prefix, d)))))
    }
    message(sprintf("%s [%s*] %d건", typename, prefix, n))
    lapply(seq(0, n - 1, by = 1000), function(s)
      st_read(call_wfs(prefix, 1000, s), quiet = TRUE))
  }
  res <- do.call(rbind, fetch(""))
  res <- res[!duplicated(res$id), ]
  st_set_crs(res, as.integer(sub("EPSG:", "", srs)))
}

vworld_to_shp <- function(typename, shp_path, ...) {
  x <- vworld_wfs(typename, ...)
  dir.create(dirname(shp_path), recursive = TRUE, showWarnings = FALSE)
  st_write(x, shp_path, layer_options = "ENCODING=UTF-8",
           delete_layer = TRUE, quiet = TRUE)
  message("저장: ", shp_path, " (", nrow(x), "건)")
  invisible(x)
}

# ───────────────── 방법 B: data.go.kr 파일데이터(SHP zip) 직접 받기 ─────────────────
# 15129688 = 국가데이터처_SGIS 행정구역 통계 및 경계 (약 270MB zip)
datagokr_file_url <- function(public_data_pk = "15129688") {
  page <- request(sprintf("https://www.data.go.kr/data/%s/fileData.do", public_data_pk)) |>
    req_user_agent(UA) |> req_perform() |> resp_body_string()
  detail_pk <- regmatches(page, regexpr("uddi:[0-9a-f-]{36}", page))
  if (!length(detail_pk)) stop("상세 PK(uddi)를 찾지 못했습니다. 페이지 구조 변경 여부 확인.")
  meta <- request("https://www.data.go.kr/tcs/dss/selectFileDataDownload.do") |>
    req_url_query(publicDataPk = public_data_pk, publicDataDetailPk = detail_pk,
                  fileDetailSn = 1, publicDataTyCode = "PR0051") |>
    req_headers(`X-Requested-With` = "XMLHttpRequest") |>
    req_user_agent(UA) |> req_perform() |> resp_body_string() |>
    jsonlite::fromJSON()
  if (!isTRUE(meta$status)) stop("다운로드 정보 조회 실패 (횟수 제한/캡차 가능)")
  list(url = sprintf("https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=%s&fileDetailSn=%s",
                     meta$atchFileId, meta$fileDetailSn),
       name = meta$dataSetFileDetailInfo$dataNm,
       updated = meta$dataSetFileDetailInfo$updtDt)
}

datagokr_shp_download <- function(public_data_pk = "15129688", dest_dir = "data/boundary") {
  info <- datagokr_file_url(public_data_pk)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest_dir, paste0(public_data_pk, ".zip"))
  message("다운로드: ", info$name, " (갱신 ", info$updated, ")")
  request(info$url) |> req_user_agent(UA) |> req_timeout(3600) |>
    req_progress() |> req_perform(path = zip_path)
  unzip(zip_path, exdir = dest_dir)
  # zip 안에 zip 이 또 들어 있는 경우까지 풀기
  for (z in setdiff(list.files(dest_dir, "\\.zip$", recursive = TRUE, full.names = TRUE), zip_path))
    unzip(z, exdir = sub("\\.zip$", "", z))
  shp <- list.files(dest_dir, "\\.shp$", recursive = TRUE, full.names = TRUE)
  message("SHP ", length(shp), "개"); shp
}

# ───────────────────────────────── 사용 예 ─────────────────────────────────
if (sys.nframe() == 0 && !interactive()) {
  # A) Sys.setenv(VWORLD_KEY = "...", VWORLD_DOMAIN = "...") 후
  # vworld_to_shp("lt_c_adsido_info", "data/boundary/vworld/sido.shp")
  # vworld_to_shp("lt_c_adsigg_info", "data/boundary/vworld/sigungu.shp")
  # vworld_to_shp("lt_c_ademd_info",  "data/boundary/vworld/emd.shp")
  # vworld_to_shp("lt_c_adri_info",   "data/boundary/vworld/ri.shp")

  # B) 키 없이 바로
  # shp <- datagokr_shp_download("15129688"); sido <- st_read(shp[1])
  print(datagokr_file_url("15129688"))
}
