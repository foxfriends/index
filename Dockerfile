FROM haskell:9.8.1

WORKDIR /app
RUN cabal update

COPY index.cabal package.yaml Setup.hs ./

RUN cabal build --only-dependencies -j4

COPY app        ./app/
COPY src        ./src/

RUN cabal install

ENTRYPOINT ["index"]
