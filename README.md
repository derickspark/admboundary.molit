# admboundary.molit

> 행정구역 경계(시도 / 시군구 / 읍면동 / 리)를 R 에서 자동으로 내려받아
> SHP 로 저장하는 패키지

두 가지 출처를 지원합니다.

| 함수 | 출처 | 경계 | 인증키 |
| --- | --- | --- | --- |
| `admb_vworld()`, `admb_vworld_shp()`, `admb_codes()` | 브이월드 WFS — 공공데이터포털 [15059008](https://www.data.go.kr/data/15059008/openapi.do) | 법정구역: 시도·시군구·읍면동·리 | 브이월드 인증키 |
| `admb_sgis_download()` | 공공데이터포털 파일데이터 [15129688](https://www.data.go.kr/data/15129688/fileData.do) (SGIS) | 행정구역(행정동 기준) | 불필요 |

실거래가처럼 **법정동 코드**를 쓰는 자료와 결합하려면 브이월드 쪽을,
통계청 통계처럼 **행정동 코드**를 쓰는 자료와 결합하려면 SGIS 쪽을 쓰세요.

## 설치

```r
# install.packages("devtools")
devtools::install_github("derickspark/admboundary.molit")
library(admboundary.molit)
```

## 빠른 시작

```r
# 1) 브이월드 인증키 등록 (세션 단위)
admb_set_vworld_key("MY_VWORLD_KEY")

# 2) 선택창으로 골라 받기
x <- admb_vworld()

# 3) 또는 바로 sf 객체로 받기
sido <- admb_vworld("sido")       # 시도
sgg  <- admb_vworld("sigungu")    # 시군구
emd  <- admb_vworld("emd")        # 읍면동
ri   <- admb_vworld("ri")         # 리 (전국은 10분 이상)

# 4) 바로 SHP 로 저장
admb_vworld_shp("sigungu", "boundary/sigungu.shp")

# 5) 일부 지역만: 법정동 코드 앞자리 (admb_codes() 로 조회)
seoul_emd <- admb_vworld("emd", code = "11")      # 서울 읍면동
admb_vworld_shp("ri", "boundary/ri_47.shp", code = "47")  # 경북 리
```

인증키는 [브이월드](https://www.vworld.kr) 회원가입 후 오픈API 인증키를
발급받으면 됩니다. 세션마다 등록하지 않으려면 `~/.Renviron` 에
`ADMB_VWORLD_KEY=...` 한 줄을 넣으세요 (`usethis::edit_r_environ()`).
공유하는 스크립트에는 키를 적지 마세요.

## 선택창 4단계

`admb_vworld()` 를 인자 없이 실행하면 다음을 차례로 묻습니다. 어느 단계에서든
`0` 을 넣으면 취소됩니다.

1. **경계 종류** — 시도 / 시군구 / 읍면동 / 리
2. **시·도** — 전국 또는 시·도 하나
3. **시·군·구** — 고른 시·도 전체 또는 시·군·구 하나 (시도 경계는 건너뜀)
4. **저장 방식** — R 객체(sf)로만 받기 또는 SHP 파일로도 저장
   (경로를 비우면 `emd_41113.shp` 같은 기본 이름)

```
── [ 1 / 4 ] 경계 종류 ──
   1: 시도 경계  (sido)
   2: 시군구 경계  (sigungu)
   3: 읍면동 경계  (emd)
   4: 리 경계  (ri)
   0: 취소
받을 경계 종류를 선택하세요 (1-4, 0=취소):
```

스크립트처럼 선택창 없이 돌리려면 `admb_vworld("emd", code = "41113")` 처럼
`level` 을 직접 주세요. 코드는 `admb_codes("sigungu", code = "41")` 로
찾을 수 있습니다.

## 결과 컬럼

| `level` | 한글 | 코드 컬럼 | 코드 자릿수 | 그 밖의 컬럼 |
| --- | --- | --- | --- | --- |
| `"sido"` | 시도 | `ctprvn_cd` | 2 | `ctp_kor_nm`, `ctp_eng_nm` |
| `"sigungu"` | 시군구 | `sig_cd` | 5 | `sig_kor_nm`, `sig_eng_nm`, `full_nm` |
| `"emd"` | 읍면동 | `emd_cd` | 8 | `emd_kor_nm`, `emd_eng_nm`, `full_nm` |
| `"ri"` | 리 | `li_cd` | 10 | `li_kor_nm`, `li_eng_nm`, `full_nm` |

좌표계는 기본 EPSG:4326 (`crs` 인자로 변경), SHP 속성 인코딩은 UTF-8 입니다.

## 알아둘 점

- **2,000건 제한** — 브이월드 WFS 는 한 조건에 최대 2,000건만 돌려줍니다.
  패키지는 법정동 코드 범위로 조회하고, 2,000건이 넘는 범위는 코드 앞자리를
  한 자리씩 늘려 자동으로 나눠 받습니다.
- **도형 유효성** — 일부 도형은 `sf::st_is_valid()` 가 `FALSE` 입니다.
  면적·중첩 계산에 쓸 때는 `admb_vworld(..., make_valid = TRUE)` 를 쓰세요.
- **용량** — 전국 SHP 는 시도 12MB, 시군구 21MB, 읍면동 54MB, 리 95MB
  정도입니다 (2026-11 기준).

## SGIS 행정동 경계

```r
admb_sgis_url()                      # 다운로드 주소와 갱신일만 확인
shp <- admb_sgis_download("sgis")    # 약 270MB zip 을 받아 압축 해제
x <- sf::st_read(shp[1])
```

공공데이터포털은 짧은 시간에 여러 번 받으면 자동입력방지를 요구하므로,
반기에 한 번 갱신될 때 받는 용도로 쓰는 것이 안전합니다.

## 출처 표시

- 브이월드 행정구역도: 국토교통부, 공공저작물 출처표시(제1유형)
- SGIS 행정구역 통계 및 경계: 국가데이터처

## 라이선스

MIT © Derick S. Park
