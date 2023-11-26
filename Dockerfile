FROM haskell:8.10.7

WORKDIR /app
RUN cabal update

COPY index.cabal Setup.hs ./

RUN cabal build --only-dependencies -j4

COPY app        ./app/
COPY src        ./src/

RUN cabal build

COPY README.md ./

RUN cabal install exe:index

ENTRYPOINT ["index"]
