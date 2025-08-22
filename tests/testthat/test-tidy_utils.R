

test_that("C/C++ call functions", {

  env <- rlang::current_env()
  mask <- rlang::new_data_mask(env)

  env$x <- 1
  env$y <- 2

  q1 <- quote(x + y)
  q2 <- quote(base::`+`(x, y))
  q3 <- quote(base::`+`(x + (y - base::`/`(x, y))))
  q4 <- quote(x + 1)

  expect_true(is_group_unaware_call(q1, env,  mask))
  expect_true(is_group_unaware_call(q2, env, mask))
  expect_true(is_group_unaware_call(q3, env, mask))

  # This returns false because at-the-moment group-unaware calls
  # must contain symbols present in the mask
  expect_false(is_group_unaware_call(q4, env, mask))

  `+` <- function(x, y) x + y

  # Since `+` overwrites base R `+`, this should
  # NOT be considered a group-unaware call unless
  # it is namespaced

  expect_false(is_group_unaware_call(q1, env, mask))
  expect_true(is_group_unaware_call(q2, env, mask))
  expect_false(is_group_unaware_call(q3, env, mask))

  rm(`+`)

  expect_false(is_nested_call(q1))
  expect_false(is_nested_call(q2))
  expect_true(is_nested_call(q3))

  expect_false(call_is_namespaced(q1))
  expect_true(call_is_namespaced(q2))
  expect_true(call_is_namespaced(q3))


  expect_equal(fun_ns(base::`+`, env), "base")
  expect_equal(fun_ns(`+`, env), "base")
  expect_equal(fun_ns("+", env), "base")
  expect_equal(fun_ns(as.symbol("+"), env), "base")

  `+` <- function(x, y) x + y

  expect_equal(fun_ns(base::`+`, env), "base")
  expect_equal(fun_ns(`+`, env), "")
  expect_equal(fun_ns("+", env), "")
  expect_equal(fun_ns(as.symbol("+"), env), "")

  rm(`+`)

  q4 <- quote(base::`+`(1 + (3 - base::`/`(5, dplyr::n()))))

  expect_true(cpp_any_quo_contains_dplyr_mask_call(list(
    rlang::new_quosure(q1, env),
    rlang::new_quosure(q2, env),
    rlang::new_quosure(q3, env),
    rlang::new_quosure(q4, env)
  )))

  expect_false(cpp_any_quo_contains_dplyr_mask_call(list(
    rlang::new_quosure(q1, env),
    rlang::new_quosure(q2, env),
    rlang::new_quosure(q3, env)
  )))


  expect_true(is_fn_call(q1, "+", NULL, env))
  expect_true(is_fn_call(q2, "+", NULL, env))
  expect_true(is_fn_call(q3, "+", NULL, env))

  expect_true(is_fn_call(q1, "+", "base", env))
  expect_true(is_fn_call(q2, "+", "base", env))
  expect_true(is_fn_call(q3, "+", "base", env))

  expect_false(is_fn_call(q1, "+", "base2", env))
  expect_false(is_fn_call(q2, "+", "base2", env))
  expect_false(is_fn_call(q3, "+", "base2", env))

  expect_false(is_fn_call(q1, "/", NULL, env))
  expect_false(is_fn_call(q2, "/", NULL, env))
  expect_false(is_fn_call(q3, "/", NULL, env))

  expect_false(is_fn_call(q1, "/", "base", env))
  expect_false(is_fn_call(q2, "/", "base", env))
  expect_false(is_fn_call(q3, "/", "base", env))

  `+` <- function(x, y) x + y


  expect_true(is_fn_call(q1, "+", NULL, env))
  expect_true(is_fn_call(q2, "+", NULL, env))
  expect_true(is_fn_call(q3, "+", NULL, env))

  expect_false(is_fn_call(q1, "+", "base", env))
  expect_true(is_fn_call(q2, "+", "base", env))
  expect_true(is_fn_call(q3, "+", "base", env))

  expect_false(is_fn_call(q1, "+", "base2", env))
  expect_false(is_fn_call(q2, "+", "base2", env))
  expect_false(is_fn_call(q3, "+", "base2", env))

  rm(`+`)

})
