.PHONY: build run

all: build

build:
	stack build
	stack install

run:
	bin/P1.exe
