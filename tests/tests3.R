library(cargo)
library(testthat)
# skip_on_cran()

Sys.setenv(R_CARGO_PKG_API = 3)

test_that("force", {
  f <- rust_fn("", force = TRUE, cached = FALSE)
  expect_equal(f(), NULL)
})

test_that("printing", {
  f <- rust_fn("
      rprintln!();
  ")
  expect_equal(capture.output(f()), c("", "NULL"))
  f <- rust_fn('
      reprintln!("error: there was a problem.");
  ')
  std.out <- capture.output(std.err <- capture.output(f(), type = "message"))
  expect_equal(std.err, "error: there was a problem.")
  expect_equal(std.out, "NULL")
  f <- rust_fn('
      rprintln!("Hi");
  ', invisible = TRUE)
  expect_equal(capture.output(f()), "Hi")
  f <- rust_fn('
      rprintln!("{} + 2 = {}", 1, 1+2);
  ', invisible = TRUE)
  expect_equal(capture.output(f()), "1 + 2 = 3")
  f <- rust_fn('
      rprint!("Hi");
      rprint!(" ");
      rprint!("{}{}","every","body");
  ', invisible = TRUE)
  expect_equal(capture.output(f()), "Hi everybody")
  f <- rust_fn('
      rprintln!("Washington");
      ()
  ')
  output <- capture.output(xx <- f())
  expect_equal(xx, NULL)
  expect_equal(output, "Washington")
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
  expect_true(identical(f(), NA_real_))
  expect_false(identical(f(), NA_integer_))
  expect_false(identical(f(), NA))
  expect_false(identical(f(), NA_character_))
  f <- rust_fn("R::new_na_i32(pc)")
  expect_false(identical(f(), NA_real_))
  expect_true(identical(f(), NA_integer_))
  expect_false(identical(f(), NA))
  expect_false(identical(f(), NA_character_))
  f <- rust_fn("R::new_na_bool(pc)")
  expect_false(identical(f(), NA_real_))
  expect_false(identical(f(), NA_integer_))
  expect_true(identical(f(), NA))
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
  expect_equal(f(), NaN)
})

test_that("f64", {
  f <- rust_fn(a, "
      a.as_f64().stop_default().to_r(pc)
  ")
  expect_error(f(c(7, 6, 5, 4)))
  expect_equal(f(7L), 7)
  expect_equal(f(7L), 7L)
  expect_equal(f(7), 7)
  expect_equal(f(7), 7L)
  expect_equal(f(c(7, 6, 5, 4)[2]), 6)
  expect_equal(f(TRUE), 1)
  expect_equal(f(FALSE), 0)
  expect_equal(f(Inf), Inf)
  expect_equal(f(-Inf), -Inf)
  expect_equal(f(NaN), NaN)
  expect_equal(f(NA), NA_real_)
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
  expect_equal(f(+.Machine$integer.max), +.Machine$integer.max)
  expect_equal(f(-.Machine$integer.max), -.Machine$integer.max)
  expect_error(f(c(7, 6, 5, 4)))
  expect_equal(f(7L), 7)
  expect_equal(f(7L), 7L)
  expect_equal(f(7), 7)
  expect_equal(f(7), 7L)
  expect_equal(f(TRUE), 1)
  expect_equal(f(FALSE), 0)
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
  expect_equal(f(-1), as.logical(-1))
  expect_equal(f(0), as.logical(0))
  expect_equal(f(1), as.logical(1))
  expect_equal(f(1.3), as.logical(1.3))
  expect_equal(f(TRUE), as.logical(TRUE))
  expect_equal(f(FALSE), as.logical(FALSE))
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
  f <- rust_fn(a, "
    a.as_vector_f64().stop_default().slice().to_r(pc)
  ")
  x <- numeric(0)
  expect_equal(f(x), x)
  x <- 2
  expect_equal(f(x), x)
  x <- c(1, 2, 3)
  expect_equal(f(x), x)
  expect_error(f(as.integer(x)))
})

test_that("i32 slice", {
  f <- rust_fn(a, "
    a.as_vector_i32().stop_default().slice().to_r(pc)
  ")
  x <- integer(0)
  expect_equal(f(x), x)
  x <- 2L
  expect_equal(f(x), x)
  x <- c(1L, 2L, 3L)
  expect_equal(f(x), x)
  expect_error(f(as.double(x)))
})

test_that("bool slice", {
  f <- rust_fn(a, "
    a.as_vector_bool().stop_default().slice().iter().map(|&x| x != 0).to_r(pc)
  ")
  x <- logical(0)
  expect_equal(f(x), x)
  x <- 2L
  expect_equal(f(x), x)
  x <- c(1L, 2L, 3L)
  expect_equal(f(x), x)
  expect_error(f(as.double(x)))
})







test_that("f64 slice", {
  f1 <- rust_fn(a, "
      use std::convert::TryInto;
      let b: &[f64] = a.as_vector().unwrap().try_into().unwrap();
      rvec!(b)
  ")
  f2 <- rust_fn(a, '
      use std::convert::TryInto;
      let b: Result<&[f64],_> = a.as_vector().and_then(|x| x.try_into());
      if b.is_err() {
        panic!("Oops, not a double vector")
      }
      rvec!(b.unwrap())
  ')
  f3 <- rust_fn(a, "
      use std::convert::TryFrom;
      let b = <&[f64]>::try_from(a.as_vector().unwrap()).unwrap();
      rvec!(b)
  ")
  f4 <- rust_fn(a, "
      rvec!(a.as_vector().and_then(|x| x.slice_double()).unwrap())
  ")
  for (f in list(f1, f2, f3, f4)) {
    expect_equal(f(7), 7)
    expect_equal(f(c(7, 6, 5, 4)), c(7, 6, 5, 4))
    expect_equal(f(numeric()), numeric())
    expect_error(f(c()))
    expect_error(f(c(1L, 2L)))
  }
  f1 <- rust_fn(a, "
      let b = a.as_vector().and_then(|x| Ok(x.coerce_double(pc))).unwrap().1;
      rvec!(b)
  ")
  f2 <- rust_fn(a, "
      a.as_vector().and_then(|x| Ok(x.coerce_double(pc))).unwrap().0
  ")
  for (f in list(f1, f2)) {
    expect_equal(f(7), 7)
    expect_equal(f(c(7, 6, 5, 4)), c(7, 6, 5, 4))
    expect_equal(f(numeric()), numeric())
    expect_equal(f(c(1L, 2L)), f(c(1, 2)))
  }
  f1 <- rust_fn("
      let data = [3.0, 4.5, 6.1];
      let (rval, slice) = RVector::new_double(data.len(), pc);
      slice.copy_from_slice(&data[..]);
      rval
  ")
  f <- f1
  expect_equal(f(), c(3, 4.5, 6.1))
  f1 <- rust_fn("
      rvec!([3.0, 4.5, 6.1])
  ")
  f <- f1
  expect_equal(f(), c(3, 4.5, 6.1))
  f1 <- rust_fn("
      let a = [3.0, 4.5, 6.1];
      rvec!(&a[..])
  ")
  f <- f1
  f1 <- rust_fn("
      let a = [3.0, 4.5, 6.1];
      rvec!(a)
  ")
  f <- f1
  expect_equal(f(), c(3, 4.5, 6.1))
})

test_that("i32 slice", {
  f1 <- rust_fn(a, "
      use std::convert::TryInto;
      let b: &[i32] = a.as_vector().unwrap().try_into().unwrap();
      rvec!(b)
  ")
  f2 <- rust_fn(a, '
      use std::convert::TryInto;
      let b: Result<&[i32],_> = a.as_vector().unwrap().try_into();
      if b.is_err() {
          panic!("Oops, not an integer vector")
      }
      rvec!(b.unwrap())
  ')
  f3 <- rust_fn(a, "
      use std::convert::TryFrom;
      let b = <&[i32]>::try_from(a.as_vector().unwrap()).unwrap();
      rvec!(b)
  ")
  f4 <- rust_fn(a, "
      rvec!(a.as_vector().unwrap().slice_integer().unwrap())
  ")
  for (f in list(f1, f2, f3, f4)) {
    expect_equal(f(7L), 7L)
    expect_equal(f(c(7L, 6L, 5L, 4L)), c(7L, 6L, 5L, 4L))
    expect_equal(f(integer()), integer())
    expect_error(f(c()))
    expect_error(f(c(1, 2)))
  }
  f1 <- rust_fn(a, "
      let b = a.as_vector().unwrap().coerce_integer(pc).1;
      rvec!(b)
  ")
  f2 <- rust_fn(a, "
      a.as_vector().unwrap().coerce_integer(pc).0
  ")
  for (f in list(f1, f2)) {
    expect_equal(f(7), 7L)
    expect_equal(f(c(7, 6, 5, 4)), c(7L, 6L, 5L, 4L))
    expect_equal(f(numeric()), integer())
    expect_equal(c(), NULL)
    expect_equal(f(c(1L, 2L)), f(c(1L, 2L)))
  }
  f1 <- rust_fn("
      let data = [3, 4, 6];
      let (rval, slice) = RVector::new_integer(data.len(), pc);
      slice.copy_from_slice(&data[..]);
      rval
  ")
  f <- f1
  expect_equal(f(), c(3L, 4L, 6L))
  f1 <- rust_fn("
      rvec!([3, 4, 6])
  ")
  f <- f1
  expect_equal(f(), c(3L, 4L, 6L))
  f1 <- rust_fn("
      let a = [3, 4, 6];
      rvec!(&a[..])
  ")
  f <- f1
  expect_equal(f(), c(3L, 4L, 6L))
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
