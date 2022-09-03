.PHONY: all rust

all: rust

rust:
	docker run --rm -it -v $(shell pwd):/usr/src/app -w /usr/src/app rust:1.63-alpine cargo build