geojson <- function(codes, matched, col = "emd_cd") {
  feats <- vapply(codes, function(cd) sprintf(
    '{"type":"Feature","id":"x.%s","geometry":{"type":"Polygon","coordinates":[[[127,37],[127.1,37],[127.1,37.1],[127,37]]]},"properties":{"%s":"%s"}}',
    cd, col, cd), "")
  sprintf('{"type":"FeatureCollection","features":[%s],"totalFeatures":%d,"numberMatched":%d,"numberReturned":%d}',
          paste(feats, collapse = ","), matched, matched, length(codes))
}

test_that("레이어 표와 level 검증", {
  expect_equal(admb_layers()$level, c("sido", "sigungu", "emd", "ri"))
  expect_equal(.vw_layer("ri")$code_len, 10L)
  expect_error(.vw_layer("dong"))
})

test_that("필터는 문자열 범위(prefix ~ prefix+z)를 만든다", {
  f <- .vw_filter("emd_cd", "41")
  expect_match(f, "<ogc:Literal>41</ogc:Literal>", fixed = TRUE)
  expect_match(f, "<ogc:Literal>41z</ogc:Literal>", fixed = TRUE)
  f0 <- .vw_filter("ctprvn_cd", "")
  expect_match(f0, "<ogc:Literal>0</ogc:Literal>", fixed = TRUE)
  expect_match(f0, "<ogc:Literal>z</ogc:Literal>", fixed = TRUE)
})

test_that("전체 자릿수 code 는 한 자리 짧게 조회한 뒤 거른다", {
  seen <- character()
  local_mocked_bindings(.vw_call = function(lay, prefix, count, start, ...) {
    seen <<- c(seen, prefix)
    geojson(c("11110101", "11110102"), 2L)
  })
  res <- admb_vworld("emd", code = "11110102", key = "k", quiet = TRUE, pause = 0)
  expect_equal(unique(seen), "1111010")
  expect_equal(res$emd_cd, "11110102")
})

test_that("numberMatched 를 읽는다", {
  expect_equal(.vw_matched(geojson("11110101", 5067L)), 5067L)
  expect_error(.vw_matched("{}"))
})

test_that("키와 code 입력 검증", {
  expect_error(admb_vworld("sido", key = ""), "인증키")
  expect_error(admb_vworld("sido", code = "1a", key = "k"), "code")
  expect_error(admb_vworld("sido", code = "123", key = "k"), "code")
})

test_that("2000건 초과 범위는 다음 자리로 쪼개 받는다", {
  calls <- character()
  local_mocked_bindings(.vw_call = function(lay, prefix, count, start, ...) {
    calls <<- c(calls, sprintf("%s|%d|%d", prefix, count, start))
    n <- switch(prefix, "4" = 2500L, "41" = 1200L, "42" = 1L, 0L)
    if (count == 1L) return(geojson(character(), n))
    codes <- if (prefix == "41") {
      sprintf("41%06d", if (start == 0L) 1:3 else 4:5)
    } else "42000001"
    geojson(codes, n)
  })
  res <- admb_vworld("emd", code = "4", key = "k", quiet = TRUE, pause = 0)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 6L)
  expect_equal(sf::st_crs(res)$epsg, 4326L)
  # 41 은 1200건이라 두 쪽(0, 1000)으로 받는다
  expect_true(all(c("41|1000|0", "41|1000|1000", "42|1000|0") %in% calls))
  expect_false("4|1000|0" %in% calls)
})

test_that("해당 구역이 없으면 NULL", {
  local_mocked_bindings(.vw_call = function(...) geojson(character(), 0L))
  expect_null(admb_vworld("ri", code = "99", key = "k", quiet = TRUE))
})

test_that("중복 코드는 한 번만 남긴다", {
  local_mocked_bindings(.vw_call = function(lay, prefix, count, start, ...)
    geojson(c("11110101", "11110101", "11110102"), 3L))
  res <- admb_vworld("emd", key = "k", quiet = TRUE, pause = 0)
  expect_equal(res$emd_cd, c("11110101", "11110102"))
})
