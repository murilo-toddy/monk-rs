build:
	rustc --edition=2021 src/main.rs

test:
	cargo test

repl: build
	./main
