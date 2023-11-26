FROM haskell:8.10.7

WORKDIR /app
RUN cabal update

COPY index.cabal ./

RUN cabal build --only-dependencies -j4

COPY app        ./app/
COPY src        ./src/

RUN cabal install

ENTRYPOINT ["index"]
