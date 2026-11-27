.ua <- "admboundary.molit (R httr2)"

# library(admboundary.molit) 시 사용법 안내.
# suppressPackageStartupMessages() 로 끌 수 있다.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "# 1) 브이월드 인증키 등록 (세션 단위)",
    "admb_set_vworld_key(\"MY_VWORLD_KEY\")",
    "",
    "# 2) 법정구역 경계 받기: \"sido\", \"sigungu\", \"emd\", \"ri\"",
    "sgg <- admb_vworld(\"sigungu\")                    # sf 객체",
    "admb_vworld_shp(\"emd\", \"emd.shp\", code = \"11\")   # 서울 읍면동만 SHP 로",
    "",
    "# 3) 행정동 경계(SGIS, 키 불필요, 약 270MB)",
    "shp <- admb_sgis_download(\"sgis\")",
    sep = "\n"
  ))
}
