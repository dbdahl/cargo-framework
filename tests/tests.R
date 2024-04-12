library(cargo)
library(testthat)
# skip_on_cran()

test_that("force", {
  f <- rust_fn("", force = TRUE, cached = FALSE)
  expect_identical(f(), NULL)
})

test_that("verbose / cached", {
  expect_silent(rust_fn(a, "a", verbose = FALSE))
  expect_output(rust_fn(a, "a", verbose = TRUE))
  expect_silent(rust_fn(a, "a", verbose = "never"))
  expect_output(rust_fn(a, "a", cached = FALSE, verbose = FALSE))
  expect_output(rust_fn(a, "a", cached = FALSE, verbose = TRUE))
  expect_silent(rust_fn(a, "a", cached = FALSE, verbose = "never"))
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
      a.is_null()
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
  f <- rust_fn("a: &RScalar", "
      a.is_na()
  ")
  expect_true(f(NA_real_))
  expect_true(f(NA_integer_))
  expect_true(f(NA))
  expect_true(f(NA_character_))
  expect_false(f(NaN))
  expect_false(f(2))
  expect_false(f(TRUE))
  expect_error(f(\() 3))
  f <- rust_fn("R::na_f64()")
  expect_identical(f(), NA_real_)
  expect_false(identical(f(), NA_integer_))
  expect_false(identical(f(), NA))
  expect_false(identical(f(), NA_character_))
  f <- rust_fn("R::na_i32()")
  expect_false(identical(f(), NA_real_))
  expect_identical(f(), NA_integer_)
  expect_false(identical(f(), NA))
  expect_false(identical(f(), NA_character_))
  f <- rust_fn("R::na_bool().to_r(pc).as_vector().to_bool(pc)")
  expect_false(identical(f(), NA_real_))
  expect_false(identical(f(), NA_integer_))
  expect_identical(f(), NA)
  expect_false(identical(f(), NA_character_))
})

test_that("nan", {
  f <- rust_fn("a: &RScalar", "
      a.is_nan()
  ")
  expect_true(f(NaN))
  expect_error(f(c(NaN, NaN)))
  expect_false(f(NA_integer_))
  expect_false(f(NA))
  expect_false(f(NA_character_))
  expect_false(f(2))
  expect_false(f(TRUE))
  expect_error(f(\() 3))
  f <- rust_fn("
      R::nan()
  ")
  expect_identical(f(), NaN)
})

test_that("f64", {
  f <- rust_fn("a: f64", "
      a
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
  f <- rust_fn("a: f64", "
      a.is_finite()
  ")
  expect_false(f(Inf))
  expect_false(f(-Inf))
  expect_false(f(NaN))
  expect_false(f(NA))
  expect_true(f(0))
  expect_true(f(4^400))
  f <- rust_fn("
      R::positive_infinity()
  ")
  expect_equal(f(), Inf)
  f <- rust_fn("
      R::negative_infinity()
  ")
  expect_equal(f(), -Inf)
})

test_that("i32", {
  f <- rust_fn("a: i32", "
      a
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
  f <- rust_fn("a: usize", "
      i32::try_from(a).unwrap()
  ")
  expect_error(f(-1))
  expect_identical(f(0), 0L)
  expect_identical(f(3), 3L)
  expect_error(f(.Machine$integer.max + 100))
  expect_error(f(.Machine$double.xmax))
  expect_error(f(Inf))
  expect_error(f(-Inf))
  expect_error(f(NaN))
  expect_error(f(NA))
  expect_error(f(integer(NA)))
  expect_error(f(NULL))
  expect_equal(f(TRUE), 1L)
  expect_equal(f(FALSE), 0L)
  expect_error(f(NA))
})

test_that("bool", {
  f <- rust_fn("a: bool", "
      a
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
    a
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
    a.as_vector().stop().to_f64(pc).slice()
  ")
  f2 <- rust_fn(a, "
    a.as_vector().stop().to_f64(pc).slice().iter().to_r(pc)
  ")
  f3 <- rust_fn(a, "
    a.as_vector().stop().to_f64(pc).slice().iter().map(|x| x).to_r(pc)
  ")
  for (f in list(f1, f2, f3)) {
    x <- numeric(0)
    expect_identical(f(x), x)
    x <- 2
    expect_identical(f(x), x)
    x <- c(1, 2, 3)
    expect_identical(f(x), x)
  }
  f <- rust_fn("
    &[1.0, 2.0, 3.0]
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn("
    [1.0, 2.0, 3.0]
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn("
    [1.0, 2.0, 3.0].iter()
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn("
    [1.0_f64, 2.0, 3.0].iter().to_r(pc)
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn("
    [1.0_f64, 2.0, 3.0].into_iter()
  ")
  expect_identical(f(), c(1, 2, 3))
  f <- rust_fn(a, "
    a.as_vector().stop().as_f64().stop().slice().iter().map(|x| x + 1.0).to_r(pc)
  ")
  x <- numeric(0)
  expect_identical(f(x), x + 1)
  x <- 2
  expect_identical(f(x), x + 1)
  x <- c(1, 2, 3)
  expect_identical(f(x), x + 1)
  f <- rust_fn(a, "
    let v = a.as_vector().stop().to_f64(pc);
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
    a.as_vector().stop().to_i32(pc).slice()
  ")
  f2 <- rust_fn(a, "
    a.as_vector().stop().to_i32(pc).slice().iter().to_r(pc)
  ")
  f3 <- rust_fn(a, "
    a.as_vector().stop().to_i32(pc).slice().iter().map(|x| x)
  ")
  for (f in list(f1, f2, f3)) {
    x <- integer(0)
    expect_identical(f(x), x)
    x <- 2L
    expect_identical(f(x), x)
    x <- c(1L, 2L, 3L)
    expect_identical(f(x), x)
  }
  f <- rust_fn("
    &[1_i32, 2, 3]
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn("
    [1_i32, 2, 3]
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn("
    [1_i32, 2, 3].iter()
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn("
    [1_i32, 2, 3].iter().to_r(pc)
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn("
    [1_i32, 2, 3].into_iter()
  ")
  expect_identical(f(), c(1L, 2L, 3L))
  f <- rust_fn(a, "
    a.as_vector().stop().to_i32(pc).slice().iter().map(|x| x + 1)
  ")
  x <- integer(0)
  expect_identical(f(x), x + 1L)
  x <- 2L
  expect_identical(f(x), x + 1L)
  x <- c(1L, 2L, 3L)
  expect_identical(f(x), x + 1L)
  f <- rust_fn(a, "
    let v = a.as_vector().stop().to_bool(pc);
    assert!(v.is_bool());
    v.slice()
  ")
  x <- c(0, 1, 2, 3)
  expect_identical(f(as.double(x)), as.integer(as.logical(x)))
  expect_identical(f(as.integer(x)), as.integer(as.logical(x)))
  expect_identical(f(as.logical(x)), as.integer(as.logical(x)))
  expect_identical(f(as.raw(x)), as.integer(as.logical(x)))
})

test_that("bool slice", {
  f3 <- rust_fn(a, "
    a.as_vector().stop().as_bool().stop().slice().iter().map(|&x| x + 10).to_r(pc)
  ")
  for (f in list(f3)) {
    x <- logical(0)
    expect_identical(f(x), integer(0))
    x <- TRUE
    expect_identical(f(x), 11L)
    x <- c(TRUE, FALSE, TRUE)
    expect_identical(f(x), c(11L, 10L ,11L))
    expect_error(f(as.double(x)))
    expect_error(f(as.integer(x)))
  }
  f <- rust_fn("
    &[true, false, true]
  ")
  expect_identical(f(), c(TRUE, FALSE, TRUE))
  f <- rust_fn("
    [true, false, true]
  ")
  expect_identical(f(), c(TRUE, FALSE, TRUE))
  f <- rust_fn(a, "
    let v = a.as_vector().stop().to_bool(pc);
    assert!(v.is_bool());
    v.slice().to_r(pc)
  ")
  x <- c(0, 1, 2, 3)
  expect_identical(f(x), as.integer(as.logical(x)))
  x <- c(0L, 1L, 2L, 3L)
  expect_identical(f(x), as.integer(as.logical(x)))
  x <- c(TRUE, FALSE, FALSE)
  expect_identical(f(x), as.integer(as.logical(x)))
  x <- c("1", "2", "3", "4")
  expect_identical(f(x), as.integer(as.logical(x)))
})

test_that("u8 slice", {
  f1 <- rust_fn(a, "
    a.as_vector().stop().as_u8().stop().slice()
  ")
  f2 <- rust_fn(a, "
    a.as_vector().stop().as_u8().stop().slice().iter()
  ")
  f3 <- rust_fn(a, "
    a.as_vector().stop().as_u8().stop().slice().iter().map(|x| x)
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
    &[1_u8, 2, 3]
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn("
    [1_u8, 2, 3]
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn("
    [1_u8, 2, 3].iter()
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn("
    [1_u8, 2, 3].iter().to_r(pc)
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn("
    [1_u8, 2, 3].into_iter()
  ")
  expect_identical(f(), as.raw(c(1L, 2L, 3L)))
  f <- rust_fn(a, "
    a.as_vector().stop().as_u8().stop().slice().iter().map(|x| x + 1).to_r(pc)
  ")
  x <- raw(0)
  expect_identical(f(x), as.raw(as.integer(x) + 1))
  x <- as.raw(2L)
  expect_identical(f(x), as.raw(as.integer(x) + 1))
  x <- as.raw(c(1L, 2L, 3L))
  expect_identical(f(x), as.raw(as.integer(x) + 1))
  f <- rust_fn(a, "
    let v = a.as_vector().stop().to_u8(pc);
    assert!(v.is_u8());
    v.slice()
  ")
  x <- as.raw(c(0, 1, 2, 3))
  expect_identical(f(as.double(x)), x)
  expect_identical(f(as.integer(x)), x)
  expect_identical(f(as.logical(x)), as.raw(as.logical(x)))
  expect_identical(f(as.raw(x)), x)
})

test_that("attributes", {
  a <- 1
  f <- rust_fn("a: &mut RObject", '
    a.set_attribute(RSymbol::from("bill").unwrap(), "bob".to_r(pc));
    a
  ')
  expect_identical(attr(f(a), "bill"), "bob")
  f <- rust_fn(a, '
    a.get_attribute(RSymbol::dim())
  ')
  a <- c(1, 2)
  attr(a, "dim") <- c(2, 1)
  expect_identical(f(a), c(2L, 1L))
  f <- rust_fn(a, "
    let b = a.as_matrix().stop().dim();
    [b[0] as i32, b[1] as i32]
  ")
  expect_identical(f(a), c(2L, 1L))
  f <- rust_fn(a, "
    a.as_matrix().stop().get_class()
  ")
  class(a) <- c("billy", "bob")
  expect_identical(f(a), c("billy", "bob"))
  f <- rust_fn("a: &mut RObject", '
    let a = a.as_matrix_mut().stop();
    a.set_class(["asdf"].to_r(pc));
  ')
  f(a)
  expect_identical(class(a), "asdf")
})

test_that("new vectors with names", {
  f <-  rust_fn('
    let a = RVector::<f64>::new(3, pc);
    let slice = a.slice_mut();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as f64;
    }
    a.set_names(["a", "b", "c"].to_r(pc)).stop();
    a
  ')
  expect_identical(f(), c(a = 0, b = 1, c = 2))
  f <-  rust_fn('
    let a = RVector::<f64>::new(3, pc);
    a.set_names(["a", "b", "c", "d"].to_r(pc)).stop();
    a
  ')
  expect_error(f())
  f <-  rust_fn('
    let a = RVector::<f64>::new(3, pc);
    a.set_names(["a", "b"].to_r(pc)).stop();
    a
  ')
  expect_error(f())
  f <-  rust_fn('
    let a = RVector::<i32>::new(3, pc);
    let slice = a.slice_mut();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as i32;
    }
    a.set_names(["a", "b", "c"].to_r(pc)).stop();
    a
  ')
  expect_true(identical(f(), c(a = 0L, b = 1L, c = 2L)))
  f <-  rust_fn('
    let a = RVector::<u8>::new(3, pc);
    let slice = a.slice_mut();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as u8;
    }
    a.set_names(["a", "b", "c"].to_r(pc)).stop();
    a
  ')
  b <- as.raw(c(0, 1, 2))
  names(b) <- c("a", "b", "c")
  expect_true(identical(f(), b))
  f <-  rust_fn('
    let a = RVector::<bool>::new(3, pc);
    let slice = a.slice_mut();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = if i != 0 { 1 } else { 0 };
    }
    a.set_names(["a", "b", "c"].to_r(pc)).stop();
    a
  ')
  expect_true(identical(f(), c(a = FALSE, b = TRUE, c = TRUE)))
  f <-  rust_fn('
    let a = RVector::<i32>::new(3, pc);
    a.set_names(["a", "b"].to_r(pc)).stop();
  ')
  expect_error(f())
})

test_that("index into vector", {
  f <- rust_fn("
    let a = RVector::<f64>::new(3, pc);
    a.set(2, 4.0).stop();
    a.get(2).stop()
  ")
  expect_identical(f(), 4)
  f <- rust_fn("
    let a = RVector::<f64>::new(3, pc);
    a.set(3, 4.0).stop();
    a.get(3).stop()
  ")
  expect_error(f())
  f <- rust_fn("
    let a = RVector::<f64>::new(3, pc);
    a.set(2, 4.0).stop();
    a.get(3).stop()
  ")
  expect_error(f())
  f <- rust_fn("
    let a = RVector::<i32>::new(3, pc);
    a.set(2, 4).stop();
    a.get(2).stop()
  ")
  expect_identical(f(), 4L)
  f <- rust_fn("
    let a = RVector::<u8>::new(3, pc);
    a.set(2, 4).stop();
    a.get(2).stop()
  ")
  expect_identical(f(), as.raw(4))
  f <- rust_fn("
    let a = RVector::<bool>::new(3, pc);
    a.set(0, false).stop();
    a.set_i32(2, 1).stop();
    assert!(a.get(2).stop() == true);
    a.set(1, true).stop();
    assert!(a.get_i32(1).stop() == 1);
    a
  ")
  expect_equal(f(), c(FALSE, TRUE, TRUE))
})

test_that("index into matrix", {
  f <- rust_fn("
    let a = RMatrix::<f64>::new(2, 3, pc);
    a.set(1, 2, 4.0).stop();
    a.get(1, 2).stop()
  ")
  expect_identical(f(), 4)
  f <- rust_fn("
    let a = RMatrix::<f64>::new(2, 3, pc);
    a.set(2, 2, 4.0).stop();
    a.get(2, 2).stop()
  ")
  expect_error(f())
  f <- rust_fn("
    let a = RMatrix::<f64>::new(2, 3, pc);
    a.set(1, 2, 4.0).stop();
    a.get(2, 2).stop()
  ")
  expect_error(f())
})

test_that("index into list", {
  f <- rust_fn("index: usize", '
    let a = RList::new(3, pc);
    a.set(1, "bill".to_r(pc)).stop();
    a.set(2, 4.0.to_r(pc)).stop();
    a.get(index).stop()
  ')
  expect_identical(f(1), "bill")
  expect_identical(f(2), 4)
  expect_error(f(4))
  f <- rust_fn("
    let a = RList::new(3, pc);
    a.set(3, 4.0.to_r(pc)).stop();
    a.get(3).stop()
  ")
  expect_error(f())
  f <- rust_fn("
    let a = RList::new(3, pc);
    a.set(2, 4.0.to_r(pc)).stop();
    a.get(3).stop()
  ")
  expect_error(f())
})

test_that("new matrix with names", {
  f <-  rust_fn('
    let a = RMatrix::<f64>::new(1, 3, pc);
    let slice = a.slice_mut();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as f64;
    }
    let dimnames = RList::new(2, pc);
    dimnames.set(0, ["row1"].to_r(pc)).stop();
    dimnames.set(1, ["col1", "col2", "col3"].to_r(pc)).stop();
    a.set_dimnames(dimnames).stop();
    a
  ')
  expect_no_error(f())
  f <-  rust_fn("
    let a = RMatrix::<i32>::new(1, 3, pc);
    let slice = a.slice_mut();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as i32;
    }
  ")
  expect_no_error(f())
  f <-  rust_fn("
    let a = RMatrix::<u8>::new(1, 3, pc);
    let slice = a.slice_mut();
    for (i, x) in slice.iter_mut().enumerate() {
      *x = i as u8;
    }
  ")
  expect_no_error(f())
})

test_that("new arrary", {
  f <-  rust_fn("
    let _: &[f64] = RMatrix::<f64>::new(1, 3, pc).slice();
    let _: &[i32] = RMatrix::<i32>::new(1, 3, pc).slice();
    let _: &[u8] =  RMatrix::<u8>::new(1, 3, pc).slice();
  ")
  expect_no_error(f())
})

test_that("matrix", {
  a <- matrix(1:8, nrow = 2)
  f <- rust_fn(a, 'a.as_matrix().stop().transpose(pc)')
  expect_identical(f(a), t(a))
  f <-  rust_fn(a, "
    let a = a.as_matrix().stop();
    let b = a.dim();
    [b[0] as i32, b[1] as i32]
  ")
  expect_identical(f(a), c(2L, 4L))
  f <- rust_fn("a", "
    let b = a.as_matrix().stop();
    b.clone(pc).to_vector_mut()
  ")
  expect_identical(f(a), as.integer(a))
  expect_true(is.vector(f(a)))
  f <- rust_fn(a, '
    let a = a.clone(pc);
    a.set_attribute(RSymbol::dim(), R::null());
    a
  ')
  expect_false(is.matrix(f(a)))
  f <-  rust_fn(a, "
    let a = a.as_matrix().stop();
    a.to_i32(pc);
    a
  ")
  b <- a
  storage.mode(b) <- "integer"
  expect_true(identical(f(a), b))
  f <-  rust_fn(a, "
    let a = a.as_matrix().stop();
    a.to_f64(pc)
  ")
  b <- a
  storage.mode(b) <- "double"
  expect_true(identical(f(a), b))
  f <-  rust_fn(a, "
    let a = a.as_matrix().stop();
    a.to_bool(pc)
  ")
  b <- a
  storage.mode(b) <- "logical"
  expect_true(identical(f(a), b))
  f <-  rust_fn(a, "
    let a = a.as_matrix().stop();
    a.to_u8(pc)
  ")
  b <- a
  storage.mode(b) <- "raw"
  expect_true(identical(f(a), b))
})

test_that("array", {
  f <-  rust_fn(a, "
    let a = a.as_array().stop().as_i32().stop();
    let b = a.dim();
    b.iter().map(|x| i32::try_from(*x).unwrap()).to_r(pc)
  ")
  a <- array(1:24, dim = c(2L, 3L, 4L))
  expect_identical(f(a), c(2L, 3L, 4L))
  f <-  rust_fn(a, "
    let a = a.as_array().stop().as_f64().stop();
    a.dim().iter().map(|x| *x as i32).to_r(pc)
  ")
  expect_error(f(a))
})

test_that("symbol", {
  f <- rust_fn('
    RSymbol::from("bobby").unwrap()
  ')
  expect_identical(class(f()), "name")
})

test_that("external_ptr", {
  f <- rust_fn("
    let a = vec![10_i32, 11, 13];
    let b = RExternalPtr::encode_full(a, R::null(), false, pc);
    let d = b.decode_val::<Vec<i32>>().stop();
    d[1]
  ")
  expect_identical(f(), 11L)
  f1_i32 <- rust_fn('
    let a = vec![10_i32, 11, 13];
    RExternalPtr::encode_full(a, "vec_i32".to_r(pc), false, pc)
  ')
  f2_i32 <- rust_fn(b, "
    let b = b.as_external_ptr().stop();
    let d = b.decode_ref::<Vec<i32>>();
    let e: i32 = d[1];
    e
  ")
  expect_identical(f2_i32(f1_i32()), 11L)
  f1_f64 <- rust_fn('
    let a = vec![10.0, 11.0, 13.0];
    RExternalPtr::encode(a, "vec_f64", pc)
  ')
  f1_raw <- rust_fn('
    let a = vec![10_u8, 11, 13];
    RExternalPtr::encode(a, "vec_u8", pc)
  ')
  f3 <- rust_fn(b, '
    let b = b.as_external_ptr().stop();
    let tag = b.tag().as_vector().stop().as_char().stop();
    let s = tag.get(0).stop();
    let result: &RObject = if s == "vec_i32" {
      let d = b.decode_ref::<Vec<i32>>();
      let e: i32 = d[1];
      e.to_r(pc).as_robject()
    } else if s == "vec_f64" {
      let d = b.decode_ref::<Vec<f64>>();
      let e: f64 = d[1];
      e.to_r(pc).as_robject()
    } else {
      stop!("Unsupported type")
    };
    result
  ')
  expect_identical(f3(f1_i32()), 11L)
  expect_false(identical(f3(f1_i32()), 11))
  expect_identical(f3(f1_f64()), 11)
  expect_error(f3(f1_raw()))
  expect_error(f3(3))
})

test_that("string", {
  f <- rust_fn("a: &str", "
    let c = a.to_uppercase();
    (&c[..]).to_r(pc)
  ")
  expect_identical(f("billy"), "BILLY")
  expect_identical(f(1), "1")
  f <- rust_fn('
    "adf"
  ')
  expect_identical(f(), "adf")
  f <- rust_fn('
    ["adf", "billy", "bob"]
  ')
  expect_identical(f(), c("adf", "billy", "bob"))
  f <- rust_fn(a, "
    let b = a.as_vector().stop().as_char().stop();
    b.get(1).stop()
  ")
  expect_identical(f(c("adf", "billy", "bob")), "billy")
  f <- rust_fn('
    let mut a = Vec::new();
    a.push("adf");
    a.push("billy");
    a.push("bob");
    (&a[..]).to_r(pc)
  ')
  expect_identical(f(), c("adf", "billy", "bob"))
})

test_that("stop", {
  f <- rust_fn(a, "
    a.as_function().stop();
    ()
  ")
  expect_error(f(1))
  f <- rust_fn(a, '
    a.as_function().stop_str("apple");
    ()
  ')
  expect_identical(as.character(capture_error(f(1))), "Error in f(1): apple\n")
  f <- rust_fn(a, "
    a.as_function().unwrap();
    ()
  ")
  expect_error(f(1))
})

test_that("call", {
  f <- rust_fn(a, "a.as_function().stop().call0(pc).stop()")
  expect_error(f(3))
  expect_identical(f(function() 4), 4)
  expect_false(f(function() 4) == 3)
  f <- rust_fn(a, "a.as_function().stop().call0(pc).is_err().to_r(pc)")
  errfn <- function() stop("An error was thrown!")
  expect_true(f(errfn))
  okfn <- function() 1 + 2
  expect_false(f(okfn))
  f <- rust_fn(a, b, "a.as_function().stop().call1(b, pc).stop()")
  expect_identical(f(\(x) 2 * x, 10), 20)
})

test_that("data.frame", {
  f <- rust_fn(a, "
    let b = a.as_list().stop();
    b.get_names()
  ")
  expect_error(f(1))
  a <- data.frame(a = 1:3, b = c(10, 11, 12))
  expect_identical(f(a), c("a", "b"))
  f <- rust_fn(a, "index: usize", "
    let b = a.as_list().stop();
    b.get(index).stop()
  ")
  expect_error(f(a, 3))
  expect_identical(f(a, 0), 1:3)
  expect_identical(f(a, 1), c(10, 11, 12))
  f <- rust_fn("a: &mut RList", "index: usize", "
    a.set(index, [0, -1, -22].to_r(pc)).stop();
  ")
  f(a, 1)
  expect_identical(a[, 2], c(0L, -1L, -22L))
  f <- rust_fn('
    let a = RList::with_names(&["i32", "f64"], pc);
    let _ = a.set(0, [   1,    2,    3].to_r(pc));
    let _ = a.set(1, [10.0, 20.0, 30.0].to_r(pc));
    a
  ')
  a <- list(i32 = 1:3, f64 = c(10, 20, 30))
  expect_identical(f(), a)
})
