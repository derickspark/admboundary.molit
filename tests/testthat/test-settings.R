test_that("브이월드 키 set/get round trips", {
  withr::with_envvar(c(ADMB_VWORLD_KEY = "", VWORLD_KEY = ""), {
    expect_equal(admb_get_vworld_key(), "")
    admb_set_vworld_key("abc-key")
    expect_equal(admb_get_vworld_key(), "abc-key")
  })
})

test_that("VWORLD_KEY 로 대체된다", {
  withr::with_envvar(c(ADMB_VWORLD_KEY = "", VWORLD_KEY = "fallback"), {
    expect_equal(admb_get_vworld_key(), "fallback")
  })
})

test_that("키 비어있는/잘못된 입력 거부", {
  expect_error(admb_set_vworld_key(""))
  expect_error(admb_set_vworld_key(NULL))
  expect_error(admb_set_vworld_key(123))
  expect_error(admb_set_vworld_key(c("a", "b")))
})
