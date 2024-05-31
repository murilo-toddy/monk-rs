build:
	cargo build

test: build
	cargo test

repl: build
	cargo run
