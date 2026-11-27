# 선택창. 테스트에서 바꿔 끼울 수 있게 readline/interactive 를 감싼다.
.readline <- function(prompt) readline(prompt)
.is_interactive <- function() interactive()

# 번호 선택창. 반환값: 1..length(choices) 또는 0L (취소).
.menu_prompt <- function(choices, prompt, max_attempts = 5L) {
  for (i in seq_along(choices)) cat(sprintf("  %2d: %s\n", i, choices[i]))
  cat("   0: 취소\n")
  for (attempt in seq_len(max_attempts)) {
    val <- trimws(.readline(prompt))
    if (val == "" || val == "0") return(0L)
    n <- suppressWarnings(as.integer(val))
    if (!is.na(n) && n >= 1L && n <= length(choices)) return(n)
    cat(sprintf("  올바른 번호(1~%d, 0=취소)를 입력하세요.\n", length(choices)))
  }
  cat("  입력 횟수 초과 — 취소합니다.\n")
  0L
}

.menu_head <- function(step, title) {
  cat(sprintf("\n── [ %d / 4 ] %s ──\n", step, title))
}

# admb_vworld() 의 선택 흐름. 반환값: list(level, code, path) 또는 NULL (취소).
.menu_select <- function(code = NULL, ...) {
  if (!.is_interactive()) {
    stop("선택창은 대화형 세션에서만 쓸 수 있습니다. ",
         "`level` 을 직접 지정하세요. 예: admb_vworld(\"sigungu\")",
         call. = FALSE)
  }
  cancel <- function() { cat("취소되었습니다.\n"); NULL }

  # 1) 경계 종류
  .menu_head(1L, "경계 종류")
  lays <- .vw_layers
  pick <- .menu_prompt(
    sprintf("%s 경계  (%s)", lays$name, lays$level),
    sprintf("받을 경계 종류를 선택하세요 (1-%d, 0=취소): ", nrow(lays)))
  if (pick == 0L) return(cancel())
  level <- lays$level[pick]
  label <- lays$name[pick]
  area <- "전국"

  if (is.null(code)) {
    # 2) 시·도
    .menu_head(2L, "시·도")
    sido <- admb_codes("sido", ...)
    pick <- .menu_prompt(
      c("(전국)", sprintf("%s  (%s)", sido$name, sido$code)),
      sprintf("지역을 선택하세요 (1=전국, 2-%d=시·도, 0=취소): ",
              nrow(sido) + 1L))
    if (pick == 0L) return(cancel())
    if (pick > 1L) {
      code <- sido$code[pick - 1L]
      area <- sido$name[pick - 1L]

      # 3) 시·군·구 (시도 경계는 해당 없음)
      if (level != "sido") {
        .menu_head(3L, "시·군·구")
        sgg <- admb_codes("sigungu", code = code, ...)
        if (nrow(sgg) > 0L) {
          pick <- .menu_prompt(
            c(sprintf("(전체) %s 전체", area),
              sprintf("%s  (%s)", sgg$name, sgg$code)),
            sprintf("%s 의 시·군·구를 선택하세요 (1=전체, 2-%d=개별, 0=취소): ",
                    area, nrow(sgg) + 1L))
          if (pick == 0L) return(cancel())
          if (pick > 1L) {
            code <- sgg$code[pick - 1L]
            area <- sgg$full_name[pick - 1L]
          }
        }
      }
    }
  } else {
    area <- paste0("코드 ", code, "*")
  }

  # 4) 저장 방식
  .menu_head(4L, "저장 방식")
  pick <- .menu_prompt(
    c("R 객체(sf)로만 받기", "SHP 파일로도 저장"),
    "저장 방식을 선택하세요 (1-2, 0=취소): ")
  if (pick == 0L) return(cancel())
  path <- NULL
  if (pick == 2L) {
    default <- paste0(level, if (!is.null(code)) paste0("_", code), ".shp")
    ans <- trimws(.readline(sprintf(
      "저장할 파일 경로 (Enter = %s): ", default)))
    path <- if (nzchar(ans)) ans else default
    if (!grepl("\\.shp$", path, ignore.case = TRUE)) path <- paste0(path, ".shp")
  }
  cat(sprintf("\n%s · %s 경계를 받습니다.\n", area, label))
  list(level = level, code = code, path = path)
}
