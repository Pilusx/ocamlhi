IMAGE_NAME="ocamlhi:1.0"
build:
	cabal v1-configure --enable-benchmarks --enable-tests
	cabal v1-build
	cp dist/build/ocamlhi/ocamlhi ocamlhi
	cp dist/build/TestUnit/TestUnit TestUnit

build_docker:
	(docker build -f docker/haskell9.dockerfile -t ${IMAGE_NAME} .)

run_docker:
	(docker run -it --entrypoint=/bin/bash --name ocamlhi --mount type=bind,source=$(shell pwd),target=/app ${IMAGE_NAME})

start_docker:
	(docker container start ocamlhi && docker attach ocamlhi)

test:
	python3 unittests.py

test_failed:
	python3 unittests.py --print_failed

hs_test:
	cabal v1-test

test_style:
	./test_style.sh

bnfc:
	./preprocessor.sh bnfc

clean:
	-cabal v1-clean
	-rm -rf dist
	-rm -rf tests/out
	-rm -rf style/
	-rm -f ocamlhi TestUnit
	-rm -f src/Library/ParOcaml.hs src/Library/LexOcaml.hs DocOcaml.html

reset: clean bnfc doc
