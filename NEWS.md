# admboundary.molit 0.1.0

* 첫 릴리스. 스크립트 `행정구역_shp_다운로드.R` 를 패키지로 옮김.
* `admb_vworld()` / `admb_vworld_shp()` — 브이월드 WFS 에서 법정구역 경계
  (시도·시군구·읍면동·리)를 받아 `sf` 객체 또는 SHP 로 저장. 2,000건 제한은
  코드 범위 자동 분할로 처리. `code` 로 일부 지역만 받을 수 있음.
* `admb_sgis_download()` / `admb_sgis_url()` — 공공데이터포털 SGIS 행정구역
  경계 zip 을 키 없이 내려받음.
* `admb_set_vworld_key()` / `admb_get_vworld_key()` — 인증키 관리.
