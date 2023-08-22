library(cargo)

Sys.setenv(R_CARGO_PKG_API = "3")

memleak1 <- rust_fn("
  let mut a: Vec<usize> = vec![0; 1024*1024];
  for i in 0..a.capacity() {
    a.push(i)
  }
  let lapse = std::time::Duration::from_millis(10);
  std::thread::sleep(lapse);
  let b = Box::new(a);
  Box::leak(b);
")

for (i in 1:500) memleak1()



memleak2 <- rust_fn("
  use roxido::rbindings::*;
  let mut a: Vec<usize> = vec![0; 1024*1024];
  for i in 0..a.capacity() {
    a.push(i)
  }
  let lapse = std::time::Duration::from_millis(10);
  std::thread::sleep(lapse);
  unsafe {
    let sexp = Rf_allocVector(RAWSXP, 1);
    REAL(sexp);  // Causes longjmp!
  }
")

for (i in 1:500) tryCatch(memleak2(), error = \(e) e)



no_memleak <- rust_fn(b, "
  let mut a: Vec<usize> = vec![0; 1024*1024];
  for i in 0..a.capacity() {
    a.push(i)
  }
  let lapse = std::time::Duration::from_millis(10);
  std::thread::sleep(lapse);
  let _ = b.as_vector_bool().stop_default();  // Causes error
")

for (i in 1:500000) tryCatch(no_memleak(), error = \(e) e)

