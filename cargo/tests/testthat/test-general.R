skip_on_cran()

test_that("printing", {
  f <- rust_fn('
      rprintln!();
      Rval::nil()
  ')
  expect_equal(capture.output(f()),c("","NULL"))
  f <- rust_fn('
      reprintln!("error: there was a problem.");
      Rval::nil()
  ')
  std.out <- capture.output(std.err <- capture.output(f(),type="message"))
  expect_equal(std.err,"error: there was a problem.")
  expect_equal(std.out,"NULL")
  f <- rust_fn('
      rprintln!("Hi");
      Rval::nil()
  ', invisible=TRUE)
  expect_equal(capture.output(f()),"Hi")
  f <- rust_fn('
      rprintln!("{} + 2 = {}", 1, 1+2);
      Rval::nil()
  ', invisible=TRUE)
  expect_equal(capture.output(f()),"1 + 2 = 3")
  f <- rust_fn('
      rprint!("Hi");
      rprint!(" ");
      rprint!("{}{}","every","body");
      Rval::nil()
  ', invisible=TRUE)
  expect_equal(capture.output(f()),"Hi everybody")
  f <- rust_fn('
      Rval::new(rprintln!("Washington"), &mut pc)
  ')
  output <- capture.output(interrupted <- f())
  expect_equal(interrupted,FALSE)
  expect_equal(output,"Washington")
})

test_that("longjmp", {
  expect_silent(f1 <- rust_fn('
      panic!("Oops!");
  ', longjmp=TRUE, invisible=FALSE))
  expect_silent(f2 <- rust_fn('
      panic!("Oops!");
  ', longjmp=TRUE, invisible=TRUE))
  expect_silent(f3 <- rust_fn('
      panic!("Oops!");
  ', longjmp=FALSE, invisible=FALSE))
  expect_silent(f4 <- rust_fn('
      panic!("Oops!");
  ', longjmp=FALSE, invisible=TRUE))
})

test_that("f64", {
  f1 <- rust_fn(a, '
      Rval::new(f64::from(a), &mut pc)
  ')
  f2 <- rust_fn(a, '
      Rval::new(f64::from(a), &mut pc)
  ')
  f3 <- rust_fn(a, '
      let b: f64 = a.into();
      Rval::new(b, &mut pc)
  ')
  f4 <- rust_fn(a, '
      Rval::new(a.as_f64(), &mut pc)
  ')
  for ( f in list(f1,f2,f3,f4) ) {
    expect_equal(f(7), 7)
    expect_equal(f(c(7,6,5,4)), 7)
    expect_equal(f(c()), as.double(NA))
    expect_equal(f(7L), 7)
    expect_equal(f(TRUE), 1)
    expect_equal(f("7"), 7)
    expect_equal(f(Inf), Inf)
    expect_equal(f(-Inf), -Inf)
    expect_equal(f(NaN), NaN)
    expect_equal(f(NA), as.double(NA))
    expect_equal(f(function() {}), as.double(NA))
    expect_warning(expect_equal(f("sdf"), as.double(NA)))
  }
})

test_that("i32", {
  f1 <- rust_fn(a, '
      Rval::new(i32::from(a), &mut pc)
  ')
  f2 <- rust_fn(a, '
      Rval::new(i32::from(a), &mut pc)
  ')
  f3 <- rust_fn(a, '
      let b: i32 = a.into();
      Rval::new(b, &mut pc)
  ')
  f4 <- rust_fn(a, '
      Rval::new(a.as_i32(), &mut pc)
  ')
  for ( f in list(f1,f2,f3) ) {
    expect_equal(f(7), 7L)
    expect_equal(f(c(7,6,5,4)), 7L)
    expect_equal(f(c()), as.integer(NA))
    expect_equal(f(7L), 7L)
    expect_equal(f(TRUE), 1L)
    expect_equal(f("7"), 7L)
    expect_warning(expect_equal(f(Inf), as.integer(NA)))
    expect_equal(f(NaN), NaN)
    expect_equal(f(NA), as.integer(NA))
    expect_equal(f(function() {}), as.integer(NA))
    expect_warning(expect_equal(f("sdf"), as.integer(NA)))
  }
})

test_that("bool", {
  f1 <- rust_fn(a, '
      use std::convert::TryFrom;
      Rval::new(bool::try_from(a).unwrap(), &mut pc)
  ')
  f2 <- rust_fn(a, '
      Rval::new(a.is_true(), &mut pc)
  ')
  f3 <- rust_fn(a, '
      Rval::new(a.is_true(), &mut pc)
  ')
  f4 <- rust_fn(a, '
      Rval::new(a.as_bool(), &mut pc)
  ')
  for ( f in list(f1,f2,f3) ) {
    expect_equal(f(7), TRUE)
    expect_equal(f(c(7,6,5,4)), TRUE)
    expect_equal(f(0L), FALSE)
    expect_equal(f(TRUE), TRUE)
  }
  expect_error(f1(c()))
  expect_error(f1("7"))
  for ( f in list(f2,f3,f4) ) {
    expect_equal(f(c()),FALSE)
    expect_equal(f("7"),FALSE)
  }
})

test_that("usize", {
  f1 <- rust_fn(a, '
      use std::convert::TryFrom;
      Rval::try_new(usize::try_from(a).unwrap(), &mut pc).unwrap()
  ')
  f2 <- rust_fn(a, '
      use std::convert::TryInto;
      let b: usize = a.try_into().unwrap();
      Rval::try_new(b.try_into().unwrap(), &mut pc).unwrap()
  ')

  for ( f in list(f1,f2) ) {
    expect_equal(f(7), 7L)
    expect_equal(f(c(7,6,5,4)), 7L)
    expect_error(f(c()))
    expect_equal(f(7L), 7L)
    expect_equal(f(TRUE), 1L)
    expect_equal(f("7"), 7L)
    expect_equal(f(0L), 0L)
    expect_equal(f(TRUE), 1L)
  }
  f <- rust_fn(a, '
      Rval::try_new(a.as_usize(), &mut pc).unwrap()
  ')
  expect_equal(f(5), 5L)
  expect_equal(f(TRUE), 1L)
  expect_equal(f(-5), 0L)
  f <- rust_fn(a, '
    Rval::try_new(a.as_usize(), &mut pc).unwrap()
  ')
  expect_equal(f(7), 7L)
  expect_equal(f(-7), 0L)
  expect_equal(f(c(7,6,5,4)), 7L)
  expect_equal(f(c()),0)
  expect_equal(f(7L), 7L)
  expect_equal(f(TRUE), 1L)
  expect_equal(f("7"), 7L)
  expect_equal(f(0L), 0L)
  expect_equal(f(TRUE), 1L)
})

test_that("Special values", {
  f <- rust_fn('
    Rval::nil()
  ')
  expect_equal(f(), NULL)
  f <- rust_fn('
    Rval::new(Rval::nan(), &mut pc)
  ')
  expect_equal(f(), NaN)
  f <- rust_fn('
    Rval::new(Rval::na_double(), &mut pc)
  ')
  expect_equal(f(), NA_real_)
  f <- rust_fn('
    Rval::new(Rval::na_integer(), &mut pc)
  ')
  expect_equal(f(), NA_integer_)
  f <- rust_fn('
    Rval::new(Rval::na_logical(), &mut pc)
  ')
  expect_equal(f(), NA_integer_)
  f <- rust_fn(b, '
    Rval::new(Rval::na_character().0 == b.get_character_element(0).0, &mut pc)
  ')
  expect_true(f(NA_character_))
  f <- rust_fn('
    Rval::new(Rval::infinity_positive(), &mut pc)
  ')
  expect_equal(f(), Inf)
  expect_true(is.infinite(f()))
  f <- rust_fn('
    Rval::new(Rval::infinity_negative(), &mut pc)
  ')
  expect_equal(f(), -Inf)
  expect_true(is.infinite(f()))
  f <- rust_fn(a, '
    Rval::new(Rval::is_finite(a.as_f64()), &mut pc)
  ')
  expect_true(f(1.0))
  expect_false(f(Inf))
  expect_false(f(-Inf))
  f <- rust_fn(a, '
    Rval::new(Rval::is_nan(a.as_f64()), &mut pc)
  ')
  expect_true(f(NaN))
  expect_false(f(1.0))
  expect_false(f(NA))
  f <- rust_fn(a, '
    Rval::new(Rval::is_na_double(a.as_f64()), &mut pc)
  ')
  expect_false(f(NaN))
  expect_false(f(1.0))
  expect_true(f(NA))
  f <- rust_fn(a, '
    Rval::new(Rval::is_na_integer(a.as_i32()), &mut pc)
  ')
  expect_false(f(1L))
  expect_true(f(NA))
  f <- rust_fn(a, '
    let b = a.slice_logical().unwrap();
    Rval::new(Rval::is_na_logical(b[0]), &mut pc)
  ')
  expect_true(f(NA))
  expect_false(f(TRUE))
  expect_false(f(FALSE))
  f <- rust_fn(a, '
    Rval::new(Rval::is_na_character(a.get_character_element(0)), &mut pc)
  ')
  a <- NA
  storage.mode(a) <- "character"
  expect_true(f(a))
  expect_false(f("asdf"))
})

test_that("f64 slice", {
  f1 <- rust_fn(a, '
      use std::convert::TryInto;
      let b: &[f64] = a.try_into().unwrap();
      Rval::new(b, &mut pc)
  ')
  f2 <- rust_fn(a, '
      use std::convert::TryInto;
      let b: Result<&[f64],_> = a.try_into();
      if b.is_err() {
        panic!("Oops, not a double vector")
      }
      Rval::new(b.unwrap(), &mut pc)
  ')
  f3 <- rust_fn(a, '
      use std::convert::TryFrom;
      let b = <&[f64]>::try_from(a).unwrap();
      Rval::new(b, &mut pc)
  ')
  f4 <- rust_fn(a, '
      Rval::new(a.slice_double().unwrap(), &mut pc)
  ')
  for ( f in list(f1,f2,f3,f4) ) {
    expect_equal(f(7), 7)
    expect_equal(f(c(7,6,5,4)), c(7,6,5,4))
    expect_equal(f(numeric()), numeric())
    expect_error(f(c()))
    expect_error(f(c(1L,2L)))
  }
  f1 <- rust_fn(a, '
      let b = a.coerce_double(&mut pc).unwrap().1;
      Rval::new(b, &mut pc)
  ')
  f2 <- rust_fn(a, '
      a.coerce_double(&mut pc).unwrap().0
  ')
  for ( f in list(f1, f2) ) {
    expect_equal(f(7), 7)
    expect_equal(f(c(7,6,5,4)), c(7,6,5,4))
    expect_equal(f(numeric()), numeric())
    expect_equal(f(c()), numeric())
    expect_equal(f(c(1L,2L)),f(c(1,2)))
  }
  f1 <- rust_fn('
      let data = [3.0, 4.5, 6.1];
      let (rval, slice) = Rval::new_vector_double(data.len(), &mut pc);
      slice.copy_from_slice(&data[..]);
      rval
  ')
  f <- f1
  expect_equal(f(), c(3, 4.5, 6.1))
  f1 <- rust_fn('
      Rval::new([3.0, 4.5, 6.1], &mut pc)
  ')
  f <- f1
  expect_equal(f(), c(3, 4.5, 6.1))
  f1 <- rust_fn('
      let a = [3.0, 4.5, 6.1];
      Rval::new(&a[..], &mut pc)
  ')
  f <- f1
  expect_equal(f(), c(3, 4.5, 6.1))
})

test_that("i32 slice", {
  f1 <- rust_fn(a, '
      use std::convert::TryInto;
      let b: &[i32] = a.try_into().unwrap();
      Rval::new(b, &mut pc)
  ')
  f2 <- rust_fn(a, '
      use std::convert::TryInto;
      let b: Result<&[i32],_> = a.try_into();
      if b.is_err() {
          panic!("Oops, not an integer vector")
      }
      Rval::new(b.unwrap(), &mut pc)
  ')
  f3 <- rust_fn(a, '
      use std::convert::TryFrom;
      let b = <&[i32]>::try_from(a).unwrap();
      Rval::new(b, &mut pc)
  ')
  f4 <- rust_fn(a, '
      Rval::new(a.slice_integer().unwrap(), &mut pc)
  ')
  for ( f in list(f1,f2,f3,f4) ) {
    expect_equal(f(7L), 7L)
    expect_equal(f(c(7L,6L,5L,4L)), c(7L,6L,5L,4L))
    expect_equal(f(integer()), integer())
    expect_error(f(c()))
    expect_error(f(c(1, 2)))
  }
  f1 <- rust_fn(a, '
      let b = a.coerce_integer(&mut pc).unwrap().1;
      Rval::new(b, &mut pc)
  ')
  f2 <- rust_fn(a, '
      a.coerce_integer(&mut pc).unwrap().0
  ')
  for ( f in list(f1, f2) ) {
    expect_equal(f(7), 7L)
    expect_equal(f(c(7,6,5,4)), c(7L,6L,5L,4L))
    expect_equal(f(numeric()), integer())
    expect_equal(f(c()), integer())
    expect_equal(f(c(1L,2L)),f(c(1L,2L)))
  }
  f1 <- rust_fn('
      let data = [3, 4, 6];
      let (rval, slice) = Rval::new_vector_integer(data.len(), &mut pc);
      slice.copy_from_slice(&data[..]);
      rval
  ')
  f <- f1
  expect_equal(f(), c(3L, 4L, 6L))
  f1 <- rust_fn('
      Rval::new([3, 4, 6], &mut pc)
  ')
  f <- f1
  expect_equal(f(), c(3L, 4L, 6L))
  f1 <- rust_fn('
      let a = [3, 4, 6];
      Rval::new(&a[..], &mut pc)
  ')
  f <- f1
  expect_equal(f(), c(3L, 4L, 6L))
})

test_that("vectors", {
  f <- rust_fn(len, 'Rval::new_vector_double(len.as_usize(), &mut pc).0')
  expect_true(is.double(f(4)))
  expect_equal(length(f(0)),0)
  expect_equal(length(f(5)),5)
  f <- rust_fn(len, 'Rval::new_vector_integer(len.as_usize(), &mut pc).0')
  expect_true(is.integer(f(4)))
  expect_equal(length(f(0)),0)
  expect_equal(length(f(5)),5)
  f <- rust_fn(len, 'Rval::new_vector_logical(len.as_usize(), &mut pc).0')
  expect_true(is.logical(f(4)))
  expect_equal(length(f(0)),0)
  expect_equal(length(f(5)),5)
  f <- rust_fn(len, 'Rval::new_vector_raw(len.as_usize(), &mut pc).0')
  expect_true(is.raw(f(4)))
  expect_equal(length(f(0)),0)
  expect_equal(length(f(5)),5)
  f <- rust_fn(len, 'Rval::new_vector_character(len.as_usize(), &mut pc)')
  expect_true(is.character(f(4)))
  expect_equal(length(f(0)),0)
  expect_equal(length(f(5)),5)
  f <- rust_fn('
      let a = Rval::new_vector_character(1, &mut pc);
      a.set_character_element(0, "David");
      a.set_character_element(1, "Lisa");
      a
  ')
  expect_error(f())
  f <- rust_fn('
      let a = Rval::new_vector_character(2, &mut pc);
      a.set_character_element(0, "David");
      a.set_character_element(1, "Lisa");
      a
  ')
  expect_equal(f(),c("David","Lisa"))
  f <- rust_fn('Rval::new(["David", "Lisa"], &mut pc)')
  expect_equal(f(),c("David","Lisa"))
  f <- rust_fn('let a = ["David", "Lisa"]; Rval::new(&a[..], &mut pc)')
  expect_equal(f(),c("David","Lisa"))
})

test_that("data.frame", {
  f <- rust_fn(x, 'Rval::new(x.is_data_frame(), &mut pc)')
  expect_true(f(data.frame(x=1:2,y=4:5)))
  expect_false(f(matrix(1:4, nrow=2)))
})

test_that("matrices", {
  f <- rust_fn(nrow, ncol, 'Rval::new_matrix_double(nrow.as_usize(),ncol.as_usize(), &mut pc).0')
  expect_equal(dim(f(2,3)),c(2,3))
  expect_equal(dim(f(-2,3)),c(0,3))
  expect_true(is.double(f(2,3)))
  f <- rust_fn(nrow, ncol, 'Rval::new_matrix_integer(nrow.as_usize(),ncol.as_usize(), &mut pc).0')
  expect_equal(dim(f(2,3)),c(2,3))
  expect_equal(dim(f(-2,-3)),c(0,0))
  expect_true(is.integer(f(2,3)))
  f <- rust_fn(nrow, ncol, 'Rval::new_matrix_logical(nrow.as_usize(),ncol.as_usize(), &mut pc).0')
  expect_equal(dim(f(2,3)),c(2,3))
  expect_equal(dim(f(2,-3)),c(2,0))
  expect_true(is.logical(f(2,3)))
  f <- rust_fn(nrow, ncol, 'Rval::new_matrix_character(nrow.as_usize(),ncol.as_usize(), &mut pc)')
  expect_equal(dim(f(2,3)),c(2,3))
  expect_equal(dim(f(-2,3)),c(0,3))
  expect_true(is.character(f(2,3)))
  f <- rust_fn(x, 'x.transpose(&mut pc)')
  x <- matrix(1:6, nrow=3)
  expect_identical(t(x), f(x))
  x <- matrix(as.character(1:12), nrow=3)
  expect_identical(t(x), f(x))
})

test_that("lists", {
  f <- rust_fn(len, 'Rval::new_list(len.as_usize(), &mut pc)')
  expect_true(is.list(f(4)))
  expect_equal(length(f(0)),0)
  expect_equal(length(f(5)),5)
  f <- rust_fn('
      let result = Rval::new_list(1, &mut pc);
      result.set_list_element(1, Rval::new(1, &mut pc));
      result
  ')
  expect_error(f())
})

test_that("attr", {
  f <- rust_fn(a,'
      a.set_attribute("names", Rval::new(["name","age"], &mut pc), &mut pc);
      a
  ')
  expect_equal(f(c("Sally","17")),c(name="Sally", age="17"))
  f <- rust_fn(a,'
      a.get_attribute("names", &mut pc)
  ')
  x <- c(name="Sally", age="17")
  expect_equal(f(x),names(x))
})

test_that("names_gets", {
  f <- rust_fn(a,'
      a.names_gets(Rval::new(["name","age"], &mut pc));
      a
  ')
  expect_equal(f(c("Sally","17")),c(name="Sally", age="17"))
  expect_equal(f(list("Sally",17)),list(name="Sally", age=17))
  expect_error(f(list("Sally")))
  expect_error(f(function(x) x))
})

test_that("class_gets", {
  f <- rust_fn(a,'
      a.class_gets(Rval::new(["dog","animal"], &mut pc));
      a
  ')
  expect_true(inherits(f(c("Sally","17")), "dog"))
  expect_true(inherits(f(c("Sally","17")), "animal"))
  expect_false(inherits(f(c("Sally","17")), "cat"))
  f <- rust_fn(a,'
      a.class_gets(Rval::new([234], &mut pc));
      a
  ')
  expect_error(f(list("Sally",17)))
  expect_error(f(list("Sally")))
  expect_error(f(function(x) x))
})

test_that("len", {
  f <- rust_fn(a,'
      Rval::try_new(a.len(), &mut pc).unwrap()
  ')
  expect_equal(f(NA),1)
  expect_equal(f(c()),0)
  expect_equal(f(new.env()),0)
  expect_equal(f(function(x,y) x+y),1)
})

test_that("call", {
  f <- rust_fn(a,'a.call0(&mut pc).unwrap()')
  expect_equal(f(function() 4),4)
  expect_false(f(function() 4) == 3)
  expect_false(f(function() 4) == 3)
  f <- rust_fn(a,'Rval::new(a.call0(&mut pc).is_err(), &mut pc)')
  errfn <- function() stop("An error was thrown!")
  expect_true(f(errfn))
})

test_that("string", {
  f <- rust_fn(a,'
      Rval::new(a.as_string(), &mut pc)
  ')
  expect_equal(f(c("David")),"David")
  expect_equal(f(c("David","Lisa")),"David")
  expect_equal(f(1L),"1")
  expect_equal(f(c(1L,3L)),"1")
  expect_equal(f(1),"1")
  expect_equal(f(NA),"NA")
  expect_equal(f(c()),"NA")
  expect_equal(f(new.env()),"NA")
  expect_equal(f(function(x,y) x+y),"NA")
})

test_that("verbose", {
  expect_silent(rust_fn(a,'a',verbose=FALSE))
  expect_output(rust_fn(a,'a',verbose=TRUE))
  expect_silent(rust_fn(a,'a',verbose="never"))
  expect_silent(rust_fn(a,'a',cached=FALSE,verbose="never"))
  expect_output(rust_fn(a,'a',cached=FALSE,verbose=FALSE))
})
