FROM haskell:9.8.2

RUN apt-get update 
RUN apt-get install -y cloc colorized-logs ocaml pylint python3 vim
RUN cabal update
RUN cabal v1-install happy-1.20.1.1 alex-3.5.1.0 
RUN cabal v1-install BNFC-2.9.5 
RUN cabal v1-install hlint-3.8

COPY . /app
WORKDIR /app

RUN cabal v1-install --only-dependencies --enable-benchmarks --enable-tests
# Check correctness...
RUN make build

CMD ["/bin/bash"]
