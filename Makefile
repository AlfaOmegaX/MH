DIR = resultados bin 

.PHONY: build run

all: build

build:
	mkdir -p $(DIR)
	stack build
	stack install --local-bin-path bin/


run:
	./P1.exe
