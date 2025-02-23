OCAMLHI_VERSION=1.1
OCAML_VERSION=ocaml-base-compiler.4.05.0
OCAMLFORMAT_OCAML_VERSION=5.2.1
build:
	cabal v1-configure --enable-benchmarks --enable-tests
	cabal v1-build
	cp dist/build/ocamlhi/ocamlhi ocamlhi

build_docker:
	(docker build \
		-f docker/haskell9.dockerfile \
		-t ocamlhi:${OCAMLHI_VERSION} \
		. \
	)

run_docker:
# [ISSUE] There are some issues with running opam inside of the
# docker container. A quick fix is to use --privileged, but it
# should not be used in production.
	(docker run \
		--privileged \
		--interactive \
		--tty \
		--entrypoint=/bin/bash \
		--name ocamlhi \
		--mount type=bind,source=$(shell pwd),target=/app \
		ocamlhi:${OCAMLHI_VERSION} \
	)

start_docker:
	(docker container start ocamlhi && docker attach ocamlhi)

test:
	python3 unittests.py

test_failed:
	python3 unittests.py --print_failed

test_haskell:
	cabal v1-test

stdlib.cma: tests/stdlib/stdlib.c tests/stdlib/stdlib.ml
	gcc -c -fPIC -I `opam exec --switch ${OCAML_VERSION} -- ocamlc -where` tests/stdlib/stdlib.c -o stdlib.o
	opam exec --switch ${OCAML_VERSION} -- ocamlmklib stdlib.o tests/stdlib/stdlib.ml -o stdlib

install_ocaml:
	-opam init -y --compiler=${OCAML_VERSION}
	-opam switch create ${OCAML_VERSION}
	-opam switch create ${OCAMLFORMAT_OCAML_VERSION}
	opam exec --switch ${OCAMLFORMAT_OCAML_VERSION} -- opam install -y ocamlformat

__test_ocaml:
	for f in $(shell find . -regextype egrep -regex ".*/(bad|good)/.*\.ml$$" -type f | sort); do \
		LOG_FILE=tests/out/$$(basename -s .ml $$f).log; \
		echo $$f; \
		echo 'open Stdlib;;\n#use "'$$f'";;' \
		| ocaml -nostdlib -no-version -I tests/stdlib stdlib.cma \
		| ansi2txt > $$LOG_FILE; \
		chmod 777 $$LOG_FILE; \
	done

test_ocaml: stdlib.cma
	mkdir -p tests/out
	opam exec --switch ${OCAML_VERSION} -- make __test_ocaml

__format_ocaml:
	for f in $(shell find . -regextype egrep -regex ".*/(stdlib|bad|good)/.*\.ml$$" -type f | sort); do \
		ocamlformat -i $$f; \
	done

format_ocaml:
	opam exec --switch ${OCAMLFORMAT_OCAML_VERSION} -- make __format_ocaml

format_haskell:
	hindent $(shell find src tests -regextype egrep -regex ".*\.hs$$" ! -regex ".*(Lexer|Parser|Print).*" -type f)
	chmod 666 $(shell find src tests -name "*.hs" -type f)

install_haskell:
	cabal v1-install hindent-6.2.1 hlint-3.8

install_asciinema:
	apt-get install -y asciinema ffmpeg
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
	source "${HOME}/.cargo/env"
	cargo install --git https://github.com/asciinema/agg

record:
# Check https://nixdaily.com/how-to/record-your-terminal-session-and-convert-it-to-a-gif-image-asciinema/
	rm -f assets/demo.cast
	asciinema rec -c "./ocamlhi -f --trace-input --trace-output --trace-hello -I tests/stdlib -I tests/good 09_List.ml" assets/demo.cast
	(cd assets \
		&& agg --cols 80 --rows 30 --speed 0.0005 --theme dracula demo.cast demo.gif \
		&& ffmpeg -f gif -i demo.gif -y demo_long.mp4 \
		&& ffmpeg -ss 00:00:04 -i demo_long.mp4 -y demo.mp4 \
		&& rm -f demo_long.mp4 \
	)


test_style:
	-cabal check
	-cloc HEAD
	-hlint --cross $(shell find src tests -regextype egrep -regex ".*\.hs$$" ! -regex ".*(Lexer|Parser|Print).*" -type f)
#	-homplexity $(shell find src/*.hs -type f) # Does not work with base-4.19
	-python3 -m pylint $(shell find *.py -type f)

bnfc:
	-bnfc -m --haskell --functor ocaml.fc
	-rm DocOcaml.txt SkelOcaml.hs TestOcaml.hs
	-chmod 666 AbsOcaml.hs ErrM.hs PrintOcaml.hs ParOcaml.y LexOcaml.x
	-mv AbsOcaml.hs src/Autogen/Grammar.hs
	-rm ErrM.hs
#	-mv ErrM.hs src/ErrM.hs
	-mv PrintOcaml.hs src/Autogen/Print.hs
	-mv ParOcaml.y src/Autogen/Parser.y
	-mv LexOcaml.x src/Autogen/Lexer.x
	-mv Makefile.bak Makefile
	-chmod 666 Makefile
	-sed -i -e 's/AbsOcaml/Grammar/g' \
		-e 's/LexOcaml/Lexer/g' \
		-e 's/ParOcaml/Parser/g' \
		-e 's/PrintOcaml/Print/g' \
		-e 's/SkelOcaml/Interpreter/g' \
		$(shell find src -type f)
	-alex -g src/Autogen/Lexer.x
	-happy -gca src/Autogen/Parser.y --info=ocaml.reg
	-chmod 666 ocaml.reg
	-make format_haskell

clean:
	-cabal v1-clean
	-rm -rf tests/out
	-rm -f DocOcaml.html ocaml.reg ocamlhi src/Lexer.hs src/Parser.hs
	-rm -f $(shell find . -regextype egrep -regex ".*\.(o|a|so|cma|cmxa|cmi|cmo|cmx)$$" -type f)

reset: clean bnfc
