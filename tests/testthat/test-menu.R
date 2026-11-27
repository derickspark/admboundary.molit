# 준비한 답을 차례로 돌려주는 readline 대역
answers <- function(...) {
  q <- c(...)
  i <- 0L
  function(prompt) { i <<- i + 1L; q[i] }
}

fake_codes <- function(level, code = NULL, ...) {
  if (level == "sido") {
    data.frame(code = c("11", "41"), name = c("서울특별시", "경기도"),
               full_name = c("서울특별시", "경기도"))
  } else {
    data.frame(code = c("41111", "41113"), name = c("장안구", "권선구"),
               full_name = c("경기도 수원시 장안구", "경기도 수원시 권선구"))
  }
}

run_menu <- function(...) {
  local_mocked_bindings(.is_interactive = function() TRUE,
                        .readline = answers(...),
                        admb_codes = fake_codes)
  out <- NULL
  capture.output(out <- .menu_select(key = "k"))
  out
}

test_that("대화형이 아니면 level 을 요구한다", {
  local_mocked_bindings(.is_interactive = function() FALSE)
  expect_error(admb_vworld(key = "k"), "level")
})

test_that("번호 선택: 잘못된 입력은 다시 묻고 0 은 취소", {
  local_mocked_bindings(.readline = answers("9", "abc", "2"))
  capture.output(n <- .menu_prompt(c("a", "b"), "? "))
  expect_equal(n, 2L)
  local_mocked_bindings(.readline = answers("0"))
  capture.output(n <- .menu_prompt(c("a", "b"), "? "))
  expect_equal(n, 0L)
})

test_that("읍면동 → 경기도 → 권선구 → SHP 기본 파일명", {
  expect_equal(run_menu("3", "3", "3", "2", ""),
               list(level = "emd", code = "41113", path = "emd_41113.shp"))
})

test_that("시군구 → 전국 → R 객체만", {
  expect_equal(run_menu("2", "1", "1"),
               list(level = "sigungu", code = NULL, path = NULL))
})

test_that("시도 경계는 시군구 단계를 건너뛴다", {
  expect_equal(run_menu("1", "2", "2", "out/seoul"),
               list(level = "sido", code = "11", path = "out/seoul.shp"))
})

test_that("리 → 경기도 전체", {
  expect_equal(run_menu("4", "3", "1", "1"),
               list(level = "ri", code = "41", path = NULL))
})

test_that("어느 단계에서든 0 이면 취소(NULL)", {
  expect_null(run_menu("0"))
  expect_null(run_menu("3", "0"))
  expect_null(run_menu("3", "3", "0"))
  expect_null(run_menu("3", "3", "2", "0"))
})

test_that("선택 결과로 받아 SHP 까지 저장한다", {
  path <- file.path(withr::local_tempdir(), "x.shp")
  local_mocked_bindings(
    .is_interactive = function() TRUE,
    .readline = answers("3", "3", "3", "2", path),
    admb_codes = fake_codes,
    .vw_call = function(lay, prefix, count, start, ...) {
      '{"type":"FeatureCollection","features":[{"type":"Feature","id":"x.1","geometry":{"type":"Polygon","coordinates":[[[127,37],[127.1,37],[127.1,37.1],[127,37]]]},"properties":{"emd_cd":"41113101"}}],"numberMatched":1}'
    })
  capture.output(res <- suppressMessages(admb_vworld(key = "k", pause = 0)))
  expect_equal(res$emd_cd, "41113101")
  expect_true(file.exists(path))
})
