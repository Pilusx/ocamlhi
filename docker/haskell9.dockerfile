FROM haskell:9.8.2

RUN apt-get update 
RUN apt-get install -y cloc colorized-logs opam python3 python3-pip vim

RUN pip3 install pylint

RUN cabal update
RUN cabal v1-install alex-3.5.1.0 happy-1.20.1.1 
RUN cabal v1-install BNFC-2.9.5
RUN cabal v1-install hindent-6.2.1 hlint-3.8 
# RUN cabal v1-install homplexity # Does not work with base-4.19

COPY . /app
WORKDIR /app
RUN git config --global --add safe.directory /app

RUN cabal v1-install --only-dependencies --enable-benchmarks --enable-tests
# Check correctness...
RUN make build

CMD ["/bin/bash"]
