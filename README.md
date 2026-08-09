## Fitparse
Fitparse is a Rust library which implements a parser for [Garmin's FIT file format](https://www.thisisant.com/developer/ant/ant-fs-and-fit1/).

## Capabilities
  - Implments most of the FIT standard, including:
    - Basic parsing of Definition and Data messages
    - Developer Fields
    - Unknown (i.e., not in FIT profile) fields
    - Subfields
    - Components
  - Not yet supported:
    - Accumulators
    - Chained FIT files
    - File CRC verification

## Regenerating the FIT profile

`src/fittypes.rs` is generated from Garmin's FIT profile by
`tools/dump_fit_messages.py`. To pick up a newer FIT SDK:

```sh
tools/regen_fittypes.sh            # latest FIT SDK release
tools/regen_fittypes.sh 21.212.0   # a specific release
cargo build && cargo test
```

The script creates `py-venv/`, downloads `Profile.xlsx` from the
[fit-sdk-tools](https://github.com/garmin/fit-sdk-tools) releases, converts its
Types and Messages sheets into `fit-profile/{Types,Messages}.csv`, and runs the
generator. Both `py-venv/` and `fit-profile/` are ignored - the profile is a
build input, not source.

Note that the import preamble emitted by `dump_fit_messages.py` is part of the
generated file, so changes to `src/fittypes.rs`'s imports have to be made in the
generator's `special_types` template rather than in the generated output.

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