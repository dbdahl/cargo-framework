library(cargo)
library(testthat)
# skip_on_cran()

Sys.setenv(R_CARGO_PKG_API = 3)

test_that("force", {
  f <- rust_fn("", force = TRUE, cached = FALSE)
  expect_identical(f(), NULL)
})

test_that("printing", {
  f <- rust_fn("
      rprintln!();
  ")
  expect_identical(capture.output(f()), c("", "NULL"))
  f <- rust_fn('
      reprintln!("error: there was a problem.");
  ')
  std_out <- capture.output(std_err <- capture.output(f(), type = "message"))
  expect_identical(std_err, "error: there was a problem.")
  expect_identical(std_out, "NULL")
  f <- rust_fn('
      rprintln!("Hi");
  ', invisible = TRUE)
  expect_identical(capture.output(f()), "Hi")
  f <- rust_fn('
      rprintln!("{} + 2 = {}", 1, 1+2);
  ', invisible = TRUE)
  expect_identical(capture.output(f()), "1 + 2 = 3")
  f <- rust_fn('
      rprint!("Hi");
      rprint!(" ");
      rprint!("{}{}","every","body");
  ', invisible = TRUE)
  expect_identical(capture.output(f()), "Hi everybody")
  f <- rust_fn('
      rprintln!("Washington");
      ()
  ')
  output <- capture.output(xx <- f())
  expect_identical(xx, NULL)
  expect_identical(output, "Washington")
})

test_that("longjmp", {
  expect_silent(f1 <- rust_fn('
      if true { panic!("Oops!"); }
      ()
  ', longjmp = TRUE, invisible = FALSE))
  expect_silent(f2 <- rust_fn('
      if true { panic!("Oops!"); }
      ()
  ', longjmp = TRUE, invisible = TRUE))
  expect_silent(f4 <- rust_fn('
      if true { panic!("Oops!"); }
      ()
  ', longjmp = FALSE, invisible = FALSE))
  expect_silent(f4 <- rust_fn('
      if true { panic!("Oops!"); }
      ()
  ', longjmp = FALSE, invisible = TRUE))
})

test_that("null", {
  f <- rust_fn(a, "
      a.is_null().to_r(pc)
  ")
  expect_true(f(NULL))
  expect_false(f(NaN))
  expect_false(f(NA))
  f <- rust_fn("")
  expect_null(f())
  f <- rust_fn("()")
  expect_null(f())
  f <- rust_fn("R::null()")
  expect_null(f())
})

test_that("na", {
  f <- rust_fn(a, "
      a.is_na().to_r(pc)
  ")
  expect_true(f(NA_real_))
  expect_true(f(NA_integer_))
  expect_true(f(NA))
  expect_true(f(NA_character_))
  expect_false(f(NaN))
  expect_false(f(2))
  expect_false(f(TRUE))
  expect_false(f(\() 3))
  f <- rust_fn("R::new_na_f64(pc)")
  expect_identical(f(), NA_real_)
  expect_false(identical(f(), NA_integer_))
  expect_false(identical(f(), NA))
  expect_false(identical(f(), NA_character_))
  f <- rust_fn("R::new_na_i32(pc)")
  expect_false(identical(f(), NA_real_))
  expect_identical(f(), NA_integer_)
  expect_false(identical(f(), NA))
  expect_false(identical(f(), NA_character_))
  f <- rust_fn("R::new_na_bool(pc)")
  expect_false(identical(f(), NA_real_))
  expect_false(identical(f(), NA_integer_))
  expect_identical(f(), NA)
  expect_false(identical(f(), NA_character_))
})

test_that("nan", {
  f <- rust_fn(a, "
      a.is_nan().to_r(pc)
  ")
  expect_true(f(NaN))
  expect_false(f(c(NaN, NaN)))
  expect_false(f(NA_integer_))
  expect_false(f(NA))
  expect_false(f(NA_character_))
  expect_false(f(2))
  expect_false(f(TRUE))
  expect_false(f(\() 3))
  f <- rust_fn("
      R::new_nan(pc)
  ")
  expect_identical(f(), NaN)
})

test_that("f64", {
  f <- rust_fn(a, "
      a.as_f64().stop_default().to_r(pc)
  ")
  expect_error(f(c(7, 6, 5, 4)))
  expect_identical(f(7L), 7)
  expect_identical(f(7), 7)
  expect_identical(f(c(7, 6, 5, 4)[2]), 6)
  expect_identical(f(TRUE), 1)
  expect_identical(f(FALSE), 0)
  expect_identical(f(Inf), Inf)
  expect_identical(f(-Inf), -Inf)
  expect_identical(f(NaN), NaN)
  expect_identical(f(NA), NA_real_)
  f <- rust_fn(a, "
      a.as_f64().stop_default().is_finite().to_r(pc)
  ")
  expect_false(f(Inf))
  expect_false(f(-Inf))
  expect_false(f(NaN))
  expect_false(f(NA))
  expect_true(f(0))
  expect_true(f(4^400))
  f <- rust_fn("
      R::infinity_positive().to_r(pc)
  ")
  expect_equal(f(), Inf)
  f <- rust_fn("
      R::infinity_negative().to_r(pc)
  ")
  expect_equal(f(), -Inf)
})

test_that("i32", {
  f <- rust_fn(a, "
      a.as_i32().stop_default().to_r(pc)
  ")
  expect_error(f(Inf))
  expect_error(f(-Inf))
  expect_error(f(NaN))
  expect_error(f(NA))
  expect_error(f(NULL))
  expect_error(f(-(.Machine$integer.max + 1)))
  expect_error(f(+(.Machine$integer.max + 1)))
  expect_identical(f(+.Machine$integer.max), +.Machine$integer.max)
  expect_identical(f(-.Machine$integer.max), -.Machine$integer.max)
  expect_error(f(c(7, 6, 5, 4)))
  expect_identical(f(7L), 7L)
  expect_identical(f(7), 7L)
  expect_identical(f(TRUE), 1L)
  expect_identical(f(FALSE), 0L)
  expect_error(f(NA))
})

test_that("usize", {
  f <- rust_fn(a, "
      let b1 = a.as_usize().stop_default();
      let b2 = a.as_f64().stop_default();
      let c = b2 - (b1 as f64);
      (c==0.0).to_r(pc)
  ")
  expect_error(f(-1))
  expect_true(f(0))
  expect_true(f(3))
  expect_true(f(.Machine$integer.max + 100))
  expect_error(f(.Machine$double.xmax))
  expect_error(f(Inf))
  expect_error(f(-Inf))
  expect_error(f(NaN))
  expect_error(f(NA))
  expect_error(f(integer(NA)))
  expect_error(f(NULL))
  expect_true(f(TRUE))
  expect_true(f(FALSE))
  expect_error(f(NA))
})

test_that("bool", {
  f <- rust_fn(a, "
      a.as_bool().stop_default().to_r(pc)
  ")
  expect_identical(f(-1), as.logical(-1))
  expect_identical(f(0), as.logical(0))
  expect_identical(f(1), as.logical(1))
  expect_identical(f(1.3), as.logical(1.3))
  expect_identical(f(TRUE), as.logical(TRUE))
  expect_identical(f(FALSE), as.logical(FALSE))
  expect_error(f(NA))
  expect_error(f(TRUE, TRUE))
  expect_equal(f(Inf), as.logical(Inf))
  expect_equal(f(-Inf), as.logical(-Inf))
})

test_that("random_bytes", {
  f <- rust_fn("
    let a: [u8; 1_000_000] = R::random_bytes();
    a.to_r(pc)
  ")
  x <- f()
  k <- 256
  expected <- length(x) / k
  observed <- table(as.character(x))
  statistic <- sum((observed - expected)^2 / expected)
  df <- k - 1
  p_value <- pchisq(statistic, df, lower.tail = FALSE)
  expect_gte(p_value, 0.0001)
})

test_that("f64 slice", {
  f1 <- rust_fn(a, "
    a.as_vector_f64().stop_default().slice().to_r(pc)
  ")
  f2 <- rust_fn(a, "
    a.as_vector_f64().stop_default().slice().iter().to_r(pc)
  ")
  f3 <- rust_fn(a, "
    a.as_vector_f64().stop_default().slice().iter().map(|x| x).to_r(pc)
  ")
  for (f in list(f1, f2, f3)) {
    x <- numeric(0)
    expect_identical(f(x), x)
    x <- 2
    expect_identical(f(x), x)
    x <- c(1, 2, 3)
    expect_identical(f(x), x)
    expect_error(f(as.integer(x)))
    expect_error(f(as.raw(x)))
  }
  f <- rust_fn("
    (&[1.0, 2.0, 3.0]).to_r(pc)
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn("
    [1.0, 2.0, 3.0].to_r(pc)
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn("
    [1.0, 2.0, 3.0].iter().to_r(pc)
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn("
    [1.0, 2.0, 3.0].iter_mut().to_r(pc)
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn("
    [1.0, 2.0, 3.0].into_iter().to_r(pc)
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn(a, "
    a.as_vector_f64().stop_default().slice().iter().map(|x| x + 1.0).to_r(pc)
  ")
  x <- numeric(0)
  expect_identical(f(x), x + 1)
  x <- 2
  expect_identical(f(x), x + 1)
  x <- c(1, 2, 3)
  expect_identical(f(x), x + 1)
  f <- rust_fn(a, "
    let v = a.to_vector_f64(pc).stop_default();
    assert!(v.is_vector_atomic());
    assert!(v.is_f64());
    v.slice().to_r(pc)
  ")
  x <- c(0, 1, 2, 3)
  expect_identical(f(as.double(x)), x)
  expect_identical(f(as.integer(x)), x)
  expect_identical(f(as.logical(x)), as.double(as.logical(x)))
  expect_identical(f(as.raw(x)), x)
})

test_that("i32 slice", {
  f1 <- rust_fn(a, "
    a.as_vector_i32().stop_default().slice().to_r(pc)
  ")
  f2 <- rust_fn(a, "
    a.as_vector_i32().stop_default().slice().iter().to_r(pc)
  ")
  f3 <- rust_fn(a, "
    a.as_vector_i32().stop_default().slice().iter().map(|x| x).to_r(pc)
  ")
  for (f in list(f1, f2, f3)) {
    x <- integer(0)
    expect_identical(f(x), x)
    x <- 2L
    expect_identical(f(x), x)
    x <- c(1L, 2L, 3L)
    expect_identical(f(x), x)
    expect_error(f(as.double(x)))
    expect_error(f(as.raw(x)))
  }
  f <- rust_fn("
    (&[1_i32, 2, 3]).to_r(pc)
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn("
    [1_i32, 2, 3].to_r(pc)
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn("
    [1_i32, 2, 3].iter().to_r(pc)
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn("
    [1_i32, 2, 3].iter_mut().to_r(pc)
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn("
    [1_i32, 2, 3].into_iter().to_r(pc)
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn(a, "
    a.as_vector_i32().stop_default().slice().iter().map(|x| x + 1).to_r(pc)
  ")
  x <- integer(0)
  expect_identical(f(x), x + 1L)
  x <- 2L
  expect_identical(f(x), x + 1L)
  x <- c(1L, 2L, 3L)
  expect_identical(f(x), x + 1L)
  f <- rust_fn(a, "
    let v = a.to_vector_bool(pc).stop_default();
    assert!(v.is_vector_atomic());
    assert!(v.is_bool());
    v.slice().to_r(pc)
  ")
  x <- c(0, 1, 2, 3)
  expect_identical(f(as.double(x)), as.integer(as.logical(x)))
  expect_identical(f(as.integer(x)), as.integer(as.logical(x)))
  expect_identical(f(as.logical(x)), as.integer(as.logical(x)))
  expect_identical(f(as.raw(x)), as.integer(as.logical(x)))
})

test_that("bool slice", {
  f3 <- rust_fn(a, "
    a.as_vector_bool().stop_default().slice().iter().map(|&x| x != 0).to_r(pc)
  ")
  for (f in list(f3)) {
    x <- logical(0)
    expect_identical(f(x), x)
    x <- TRUE
    expect_identical(f(x), x)
    x <- c(TRUE, FALSE, TRUE)
    expect_identical(f(x), x)
    expect_error(f(as.double(x)))
    expect_error(f(as.integer(x)))
  }
  f <- rust_fn("
    (&[true, false, true]).to_r(pc)
  ")
  expect_identical(f(), c(TRUE, FALSE, TRUE))
  f <- rust_fn("
    [true, false, true].to_r(pc)
  ")
  expect_identical(f(), c(TRUE, FALSE, TRUE))
  f <- rust_fn("
    (&[true, false, true]).to_r(pc)
  ")
  expect_identical(f(), c(TRUE, FALSE, TRUE))
  f <- rust_fn("
    [true, false, true].iter().to_r(pc)
  ")
  expect_identical(f(), c(TRUE, FALSE, TRUE))
  f <- rust_fn("
    [true, false, true].iter_mut().to_r(pc)
  ")
  expect_identical(f(), c(TRUE, FALSE, TRUE))
  f <- rust_fn("
    [true, false, true].into_iter().to_r(pc)
  ")
  expect_identical(f(), c(TRUE, FALSE, TRUE))
  f <- rust_fn(a, "
    let v = a.to_vector_i32(pc).stop_default();
    assert!(v.is_vector_atomic());
    assert!(v.is_i32());
    v.slice().to_r(pc)
  ")
  x <- as.integer(c(0, 1, 2, 3))
  expect_identical(f(as.double(x)), x)
  expect_identical(f(as.integer(x)), x)
  expect_identical(f(as.logical(x)), as.integer(as.logical(x)))
  expect_identical(f(as.raw(x)), x)
})

test_that("u8 slice", {
  f1 <- rust_fn(a, "
    a.as_vector_u8().stop_default().slice().to_r(pc)
  ")
  f2 <- rust_fn(a, "
    a.as_vector_u8().stop_default().slice().iter().to_r(pc)
  ")
  f3 <- rust_fn(a, "
    a.as_vector_u8().stop_default().slice().iter().map(|x| x).to_r(pc)
  ")
  for (f in list(f1, f2, f3)) {
    x <- raw(0)
    expect_identical(f(x), x)
    x <- as.raw(2L)
    expect_identical(f(x), x)
    x <- as.raw(c(1L, 2L, 3L))
    expect_identical(f(x), x)
    expect_error(f(as.double(x)))
    expect_error(f(as.integer(x)))
  }
  f <- rust_fn("
    (&[1_u8, 2, 3]).to_r(pc)
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn("
    [1_u8, 2, 3].to_r(pc)
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn("
    [1_u8, 2, 3].iter().to_r(pc)
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn("
    [1_u8, 2, 3].iter_mut().to_r(pc)
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn("
    [1_u8, 2, 3].into_iter().to_r(pc)
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn(a, "
    a.as_vector_u8().stop_default().slice().iter().map(|x| x + 1).to_r(pc)
  ")
  x <- raw(0)
  expect_identical(f(x), as.raw(as.integer(x) + 1))
  x <- as.raw(2L)
  expect_identical(f(x), as.raw(as.integer(x) + 1))
  x <- as.raw(c(1L, 2L, 3L))
  expect_identical(f(x), as.raw(as.integer(x) + 1))
  f <- rust_fn(a, "
    let v = a.to_vector_u8(pc).stop_default();
    assert!(v.is_vector_atomic());
    assert!(v.is_u8());
    v.slice().to_r(pc)
  ")
  x <- as.raw(c(0, 1, 2, 3))
  expect_identical(f(as.double(x)), x)
  expect_identical(f(as.integer(x)), x)
  expect_identical(f(as.logical(x)), as.raw(as.logical(x)))
  expect_identical(f(as.raw(x)), x)
})

test_that("attributes", {
  a <- 1
  f <- rust_fn(a, '
    a.set_attribute("bill", &"bob".to_r(pc), pc);
    a
  ')
  expect_identical(attr(f(a), "bill"), "bob")
  f <- rust_fn(a, '
    a.get_attribute("dim", pc)
  ')
  a <- c(1, 2)
  attr(a, "dim") <- c(2, 1)
  expect_identical(f(a), c(2L, 1L))
  f <- rust_fn(a, "
    let b = a.to_matrix_f64(pc).stop_default().dim();
    [b[0] as i32, b[1] as i32].to_r(pc)
  ")
  expect_identical(f(a), c(2L, 1L))
  f <- rust_fn(a, "
    a.to_matrix_f64(pc).stop_default().get_class()
  ")
  class(a) <- c("billy", "bob")
  expect_identical(f(a), c("billy", "bob"))
  f <- rust_fn(a, '
    let a = a.to_matrix_f64(pc).stop_default();
    a.set_class(&"asdf".to_r(pc));
    a
  ')
  expect_identical(class(f(a)), "asdf")
})

test_that("new vectors with names", {
  f <-  rust_fn('
    let a = R::new_vector_f64(3, pc);
    let slice = a.slice();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as f64;
    }
    let _ = a.set_names(&["a", "b", "c"].to_r(pc));
    a
  ')
  expect_identical(f(), c(a = 0, b = 1, c = 2))
  f <-  rust_fn('
    let a = R::new_vector_i32(3, pc);
    let slice = a.slice();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as i32;
    }
    let _ = a.set_names(&["a", "b", "c"].to_r(pc));
    a
  ')
  expect_true(identical(f(), c(a = 0L, b = 1L, c = 2L)))
  f <-  rust_fn('
    let a = R::new_vector_u8(3, pc);
    let slice = a.slice();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as u8;
    }
    let _ = a.set_names(&["a", "b", "c"].to_r(pc));
    a
  ')
  b <- as.raw(c(0, 1, 2))
  names(b) <- c("a", "b", "c")
  expect_true(identical(f(), b))
  f <-  rust_fn('
    let a = R::new_vector_bool(3, pc);
    let slice = a.slice();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = if i != 0 { 1 } else { 0 };
    }
    let _ = a.set_names(&["a", "b", "c"].to_r(pc));
    a
  ')
  expect_true(identical(f(), c(a = FALSE, b = TRUE, c = TRUE)))
  f <-  rust_fn('
    let a = R::new_vector_i32(3, pc);
    a.set_names(&["a", "b"].to_r(pc)).stop_default();
  ')
  expect_error(f())
})

test_that("new matrix with names", {
  f <-  rust_fn('
    let a = R::new_matrix_f64(1, 3, pc);
    let slice = a.slice();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as f64;
    }
    let dimnames = R::new_vector_list(2, pc);
    dimnames.set(0, &["row1"].to_r(pc));
    dimnames.set(1, &["col1", "col2", "col3"].to_r(pc));
    a.set_dimnames(&dimnames).stop("Problem setting dimnames");
    a
  ')
  expect_no_error(f())
  f <-  rust_fn("
    let a = R::new_matrix_i32(1, 3, pc);
    let slice = a.slice();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as i32;
    }
  ")
  expect_no_error(f())
  f <-  rust_fn("
    let a = R::new_matrix_u8(1, 3, pc);
    let slice = a.slice();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as u8;
    }
  ")
  expect_no_error(f())
  f <-  rust_fn("
    let a = R::new_matrix_bool(1, 3, pc);
    let slice = a.slice();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = if i != 0 { 1 } else { 0 };
    }
  ")
  expect_no_error(f())
})

test_that("new arrary", {
  f <-  rust_fn("
    let _: &[f64] = R::new_matrix_f64(1, 3, pc).slice();
    let _: &[i32] = R::new_matrix_i32(1, 3, pc).slice();
    let _: &[u8] = R::new_matrix_u8(1, 3, pc).slice();
    let _: &[i32] = R::new_matrix_bool(1, 3, pc).slice();
  ")
  expect_no_error(f())
})

test_that("matrix", {
  f <-  rust_fn(a, "
    let a = a.to_matrix_f64(pc).stop_default();
    let b = a.dim();
    [b[0] as i32, b[1] as i32].to_r(pc)
  ")
  a <- matrix(1:8, nrow = 2)
  expect_identical(f(a), c(2L, 4L))
  f <- rust_fn(a, "
    a.to_matrix_f64(pc).stop_default().to_vector()
  ")
  expect_identical(f(a), as.double(a))
  expect_true(is.matrix(a))
  f <- rust_fn(a, '
    let a = a.duplicate(pc);
    a.set_attribute("dim", &R::null(), pc);
    a
  ')
  expect_false(is.matrix(f(a)))
  f <-  rust_fn(a, "
    let a = a.to_matrix_f64(pc).stop_default();
    a.to_i32(pc)
  ")
  b <- a
  storage.mode(b) <- "integer"
  expect_true(identical(f(a), b))
  f <-  rust_fn(a, "
    let a = a.to_matrix_i32(pc).stop_default();
    a.to_f64(pc)
  ")
  b <- a
  storage.mode(b) <- "double"
  expect_true(identical(f(a), b))
  f <-  rust_fn(a, "
    let a = a.to_matrix_u8(pc).stop_default();
    a.to_bool(pc)
  ")
  b <- a
  storage.mode(b) <- "logical"
  expect_true(identical(f(a), b))
  f <-  rust_fn(a, "
    let a = a.to_matrix_bool(pc).stop_default();
    a.to_u8(pc)
  ")
  b <- a
  storage.mode(b) <- "logical"
  storage.mode(b) <- "raw"
  expect_true(identical(f(a), b))
})

test_that("array", {
  f <-  rust_fn(a, "
    let a = a.as_array_i32().stop_default();
    let b = a.dim();
    (&b[..]).to_r(pc)
  ")
  a <- array(1:24, dim = c(2L, 3L, 4L))
  expect_identical(f(a), c(2L, 3L, 4L))
  f <-  rust_fn(a, "
    let a = a.as_array_f64().stop_default();
    let b = a.dim();
    (&b[..]).to_r(pc)
  ")
  expect_error(f(a))
})

test_that("symbol", {
  f <- rust_fn('
    R::new_symbol("bill", pc)
  ')
  expect_identical(class(f()), "name")
})

test_that("external_ptr", {
  f <- rust_fn("
    let a = vec![10_i32, 11, 13];
    let b = R::encode(a, &R::null());
    let d = b.decode_as_val();
    let e: i32 = d[1];
    e.to_r(pc)
  ")
  expect_identical(f(), 11L)
  f1_i32 <- rust_fn('
    let a = vec![10_i32, 11, 13];
    R::encode(a, &"vec_i32".to_r(pc))
  ')
  f2_i32 <- rust_fn(b, "
    let b = b.as_external_ptr().stop_default();
    let c = b.set_type::<Vec<i32>>();
    let d = c.decode_as_ref();
    let e: i32 = d[1];
    e.to_r(pc)
  ")
  expect_identical(f2_i32(f1_i32()), 11L)
  f1_f64 <- rust_fn('
    let a = vec![10.0, 11.0, 13.0];
    R::encode(a, &"vec_f64".to_r(pc))
  ')
  f1_u8 <- rust_fn('
    let a = vec![10_u8, 11, 13];
    R::encode(a, &"vec_u8".to_r(pc))
  ')
  f3 <- rust_fn(b, '
    let b = b.as_external_ptr().stop_default();
    let tag = b.tag().as_vector_str().stop_default();
    let s = tag.get(0).unwrap();
    let result: RObject = if s == "vec_i32" {
      let c = b.set_type::<Vec<i32>>();
      let d = c.decode_as_ref();
      let e: i32 = d[1];
      e.to_r(pc).into()
    } else if s == "vec_f64" {
      let c = b.set_type::<Vec<f64>>();
      let d = c.decode_as_ref();
      let e: f64 = d[1];
      e.to_r(pc).into()
    } else {
      stop!("Unsupported type")
    };
    result
  ')
  expect_identical(f3(f1_i32()), 11L)
  expect_false(identical(f3(f1_i32()), 11))
  expect_identical(f3(f1_f64()), 11)
  expect_error(f3(f1_u8()))
  expect_error(f3(3))
})

test_that("data frame", {
  f <- rust_fn(a, "
    let b = a.as_data_frame().stop_default();
    b.get_names()
  ")
  a <- data.frame(a = 1:3, b = c(10, 11, 12))
  expect_identical(f(a), c("a", "b"))
  f <- rust_fn(a, "
    let b = a.as_data_frame().stop_default();
    b.get(0)
  ")
  expect_identical(f(a), 1:3)
  f <- rust_fn(a, "
    let b = a.as_data_frame().stop_default();
    b.get(1)
  ")
  expect_identical(f(a), c(10, 11, 12))
})
















test_that("vectors", {
  f <- rust_fn(len, "RVector::new_double(len.as_usize(), pc).0")
  expect_true(is.double(f(4)))
  expect_equal(length(f(0)), 0)
  expect_equal(length(f(5)), 5)
  f <- rust_fn(len, "RVector::new_integer(len.as_usize(), pc).0")
  expect_true(is.integer(f(4)))
  expect_equal(length(f(0)), 0)
  expect_equal(length(f(5)), 5)
  f <- rust_fn(len, "RVector::new_logical(len.as_usize(), pc).0")
  expect_true(is.logical(f(4)))
  expect_equal(length(f(0)), 0)
  expect_equal(length(f(5)), 5)
  f <- rust_fn(len, "RVector::new_raw(len.as_usize(), pc).0")
  expect_true(is.raw(f(4)))
  expect_equal(length(f(0)), 0)
  expect_equal(length(f(5)), 5)
  f <- rust_fn(len, "RVectorCharacter::new(len.as_usize(), pc)")
  expect_true(is.character(f(4)))
  expect_equal(length(f(0)), 0)
  expect_equal(length(f(5)), 5)
  f <- rust_fn('
      let a = RVectorCharacter::new(1, pc);
      let _ = a.set(0, "David", pc);
      if a.set(1, "Lisa", pc).is_err() {
          panic!("Oh no!");
      }
      a
  ')
  expect_error(f())
  f <- rust_fn('
      let a = RVectorCharacter::new(2, pc);
      let _ = a.set(0, "David", pc);
      let _ = a.set(1, "Lisa", pc);
      a
  ')
  expect_equal(f(), c("David", "Lisa"))
  f <- rust_fn('rstr!(["David", "Lisa"])')
  expect_equal(f(), c("David", "Lisa"))
  f <- rust_fn('let a = ["David", "Lisa"]; rstr!(&a[..])')
  expect_equal(f(), c("David", "Lisa"))
})

test_that("data.frame", {
  f <- rust_fn(x, "rvec!(x.is_data_frame())")
  expect_true(f(data.frame(x = 1:2, y = 4:5)))
  expect_false(f(matrix(1:4, nrow = 2)))
})

test_that("matrices", {
  f <- rust_fn(nrow, ncol, "RMatrix::new_double(nrow.as_usize(),ncol.as_usize(), pc).0")
  expect_equal(dim(f(2, 3)), c(2, 3))
  expect_equal(dim(f(-2, 3)), c(0, 3))
  expect_true(is.double(f(2, 3)))
  f <- rust_fn(nrow, ncol, "RMatrix::new_integer(nrow.as_usize(),ncol.as_usize(), pc).0")
  expect_equal(dim(f(2, 3)), c(2, 3))
  expect_equal(dim(f(-2, -3)), c(0, 0))
  expect_true(is.integer(f(2, 3)))
  f <- rust_fn(nrow, ncol, "RMatrix::new_logical(nrow.as_usize(),ncol.as_usize(), pc).0")
  expect_equal(dim(f(2, 3)), c(2, 3))
  expect_equal(dim(f(2, -3)), c(2, 0))
  expect_true(is.logical(f(2, 3)))
  f <- rust_fn(x, "x.as_matrix().unwrap().transpose(pc)")
  x <- matrix(1:6, nrow = 3)
  expect_identical(t(x), f(x))
  x <- matrix(as.character(1:12), nrow = 3)
  expect_identical(t(x), f(x))
})

test_that("lists", {
  f <- rust_fn(len, "RList::new(len.as_usize(), pc)")
  expect_true(is.list(f(4)))
  expect_equal(length(f(0)), 0)
  expect_equal(length(f(5)), 5)
  f <- rust_fn('
      let result = RList::new(1, pc);
      if result.set(1, rvec!(1)).is_err() {
         panic!("Oops!");
      }
      result
  ')
  expect_error(f())
})

test_that("attr", {
  f <- rust_fn(a, '
      a.set_attribute("names", RVectorCharacter::allocate(["name","age"], pc), pc);
      a
  ')
  expect_equal(f(c("Sally", "17")), c(name = "Sally", age = "17"))
  f <- rust_fn(a, '
      a.get_attribute("names", pc)
  ')
  x <- c(name = "Sally", age = "17")
  expect_equal(f(x), names(x))
})

test_that("names_gets", {
  f <- rust_fn(a, '
      if a.as_vector().unwrap().names_gets(rstr!(["name","age"])).is_err() {
        panic!("No way.")
      }
      a
  ')
  expect_equal(f(c("Sally", "17")), c(name = "Sally", age = "17"))
  expect_equal(f(list("Sally", 17)), list(name = "Sally", age = 17))
  expect_error(f(list("Sally")))
  expect_error(f(function(x) x))
})

test_that("class_gets", {
  f <- rust_fn(a, '
      let _ = a.class_gets(rstr!(["dog","animal"]));
      a
  ')
  expect_true(inherits(f(c("Sally", "17")), "dog"))
  expect_true(inherits(f(c("Sally", "17")), "animal"))
  expect_false(inherits(f(c("Sally", "17")), "cat"))
})

test_that("len", {
  f <- rust_fn(a, "
      RVector::try_allocate(a.len(), pc).unwrap()
  ")
  expect_equal(f(NA), 1)
  expect_equal(f(c()), 0)
  expect_equal(f(new.env()), 0)
  expect_equal(f(function(x, y) x + y), 1)
})

test_that("call", {
  f <- rust_fn(a, "a.as_function().unwrap().call0(pc).unwrap()")
  expect_equal(f(function() 4), 4)
  expect_false(f(function() 4) == 3)
  expect_false(f(function() 4) == 3)
  f <- rust_fn(a, "rvec!(a.as_function().unwrap().call0(pc).is_err())")
  errfn <- function() stop("An error was thrown!")
  expect_true(f(errfn))
  okfn <- function() 1 + 2
  expect_false(f(okfn))
})

test_that("string", {
  f <- rust_fn(a, "
      rstr!(a.as_string().unwrap())
  ")
  expect_equal(f(c("David")), "David")
  expect_equal(f(c("David", "Lisa")), "David")
  expect_equal(f(1L), "1")
  expect_equal(f(c(1L, 3L)), "1")
  expect_equal(f(1), "1")
  expect_equal(f(NA), "NA")
  expect_equal(f(c()), "NA")
  expect_equal(f(new.env()), "NA")
  expect_equal(f(function(x, y) x + y), "NA")
})

test_that("verbose", {
  expect_silent(rust_fn(a, "a", verbose = FALSE))
  expect_output(rust_fn(a, "a", verbose = TRUE))
  expect_silent(rust_fn(a, "a", verbose = "never"))
  expect_output(rust_fn(a, "a", cached = FALSE, verbose = FALSE))
  expect_output(rust_fn(a, "a", cached = FALSE, verbose = TRUE))
  expect_silent(rust_fn(a, "a", cached = FALSE, verbose = "never"))
})
