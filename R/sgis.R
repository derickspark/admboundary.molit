#' Download the SGIS administrative-district boundary file
#'
#' 공공데이터포털 파일데이터 15129688 「국가데이터처_SGIS 행정구역 통계 및
#' 경계」 zip 을 받는다. 행정동 기준 경계 SHP 와 통계 CSV 가 들어 있고,
#' 인증키나 로그인이 필요 없다. 파일은 약 270MB 이다.
#'
#' 포털은 짧은 시간에 여러 번 받으면 자동입력방지(캡차)를 요구한다. 그때는
#' 함수가 오류로 멈추므로 시간을 두고 다시 시도한다.
#'
#' @param dest_dir zip 을 저장하고 압축을 풀 폴더.
#' @param public_data_pk 공공데이터포털 파일데이터 번호.
#' @param quiet `TRUE` 면 진행 표시를 숨긴다.
#'
#' @return `admb_sgis_url()` 은 `url`, `name`, `updated` 를 담은 리스트.
#'   `admb_sgis_download()` 는 압축을 푼 `.shp` 파일 경로의 문자 벡터.
#'
#' @examples
#' \dontrun{
#' admb_sgis_url()
#' shp <- admb_sgis_download("sgis")
#' x <- sf::st_read(shp[1])
#' }
#'
#' @export
admb_sgis_download <- function(dest_dir, public_data_pk = "15129688",
                               quiet = FALSE) {
  info <- admb_sgis_url(public_data_pk)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(dest_dir, paste0(public_data_pk, ".zip"))
  if (!quiet) message("다운로드: ", info$name, " (갱신 ", info$updated, ")")
  req <- httr2::request(info$url) |>
    httr2::req_user_agent(.ua) |>
    httr2::req_timeout(3600)
  if (!quiet) req <- httr2::req_progress(req)
  httr2::req_perform(req, path = zip_path)
  utils::unzip(zip_path, exdir = dest_dir)
  # zip 안에 zip 이 또 들어 있는 경우까지 푼다
  inner <- setdiff(list.files(dest_dir, "\\.zip$", recursive = TRUE,
                              full.names = TRUE), zip_path)
  for (z in inner) utils::unzip(z, exdir = sub("\\.zip$", "", z))
  list.files(dest_dir, "\\.shp$", recursive = TRUE, full.names = TRUE)
}

#' @rdname admb_sgis_download
#' @export
admb_sgis_url <- function(public_data_pk = "15129688") {
  page <- httr2::request(
    sprintf("https://www.data.go.kr/data/%s/fileData.do", public_data_pk)) |>
    httr2::req_user_agent(.ua) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  detail_pk <- .sgis_detail_pk(page)
  meta <- httr2::request(
    "https://www.data.go.kr/tcs/dss/selectFileDataDownload.do") |>
    httr2::req_url_query(publicDataPk = public_data_pk,
                         publicDataDetailPk = detail_pk,
                         fileDetailSn = 1, publicDataTyCode = "PR0051") |>
    httr2::req_headers(`X-Requested-With` = "XMLHttpRequest") |>
    httr2::req_user_agent(.ua) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  if (!isTRUE(meta$status)) {
    stop("다운로드 정보를 받지 못했습니다 (횟수 제한일 수 있습니다).",
         call. = FALSE)
  }
  list(
    url = sprintf(
      "https://www.data.go.kr/cmm/cmm/fileDownload.do?atchFileId=%s&fileDetailSn=%s",
      meta$atchFileId, meta$fileDetailSn),
    name = meta$dataSetFileDetailInfo$dataNm,
    updated = meta$dataSetFileDetailInfo$updtDt)
}

# 상세 페이지 HTML 에서 파일 상세 PK(uddi:...) 추출
.sgis_detail_pk <- function(page) {
  m <- regmatches(page, regexpr("uddi:[0-9a-f-]{36}", page))
  if (!length(m)) {
    stop("상세 PK(uddi)를 찾지 못했습니다. 포털 페이지 구조가 바뀌었을 수 있습니다.",
         call. = FALSE)
  }
  m
}
