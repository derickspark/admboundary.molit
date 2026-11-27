# admboundary.molit

행정구역 경계 SHP 를 R 로 자동으로 내려받는 스크립트.

| 함수 | 출처 | 경계 | 키 |
|---|---|---|---|
| `vworld_to_shp()` | 브이월드 WFS (data.go.kr 15059008) | 법정구역: 시도·시군구·읍면동·리 | 브이월드 인증키 |
| `datagokr_shp_download()` | data.go.kr 파일데이터 15129688 (SGIS) | 행정구역(행정동) | 불필요 |

```r
source("행정구역_shp_다운로드.R")

# 브이월드: 인증키는 환경변수로 (~/.Renviron 에 VWORLD_KEY=... 권장, 코드에 적지 말 것)
vworld_to_shp("lt_c_adsido_info", "data/boundary/vworld/sido.shp")     # 시도
vworld_to_shp("lt_c_adsigg_info", "data/boundary/vworld/sigungu.shp")  # 시군구
vworld_to_shp("lt_c_ademd_info",  "data/boundary/vworld/emd.shp")      # 읍면동
vworld_to_shp("lt_c_adri_info",   "data/boundary/vworld/ri.shp")       # 리 (10분 안팎)

# SGIS: 키 없이 zip(약 270MB) 을 받아 압축을 풀고 .shp 경로를 돌려준다
shp <- datagokr_shp_download("15129688")
```

- 브이월드 WFS 는 한 조건으로 최대 2000건만 주므로, 코드 범위로 자동 분할해 받는다.
- 좌표계는 EPSG:4326, 속성 인코딩은 UTF-8.
- 일부 도형은 `sf::st_is_valid()` 가 FALSE 이므로 면적·중첩 계산 전에는 `sf::st_make_valid()` 를 권장.
- `data/` 는 `.gitignore` 로 제외한다. 출처 표시: 국토교통부 브이월드, 국가데이터처 SGIS.
