test_that("상세 PK 를 HTML 에서 찾는다", {
  html <- "fn_fileDataDown('15129688', 'uddi:8bd14a24-ddb4-4c0c-9bc0-fc200b7ff729', '','1')"
  expect_equal(.sgis_detail_pk(html), "uddi:8bd14a24-ddb4-4c0c-9bc0-fc200b7ff729")
  expect_error(.sgis_detail_pk("<html></html>"), "uddi")
})
