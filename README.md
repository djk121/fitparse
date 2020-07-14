## Fitparse
Fitparse is a Rust library which implements a parser for [Garmin's FIT file format](https://www.thisisant.com/developer/ant/ant-fs-and-fit1/).

## Example

```rust
use fitparse::fitfile::FitFile;

let mut f = match File::open("/path/to/fit_file.fit").unwrap(); 

let mut fit_file = FitFile::new(
    1024 * 1024 * 10, // maximum file size, here 10 Mb
    true              // retain the raw bytes for per-message inspection
);

match ff.parse(&mut f) {
    Err(e) => panic!("failed to parse file: {:?}", e),
    _ => (),
}

println!("Parsed num messages: {}", ff.messages.len());

println!("Message #42:");
println!("{}", ff.messages[42]);
```