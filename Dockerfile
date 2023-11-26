FROM haskell:9

WORKDIR /app

COPY index.cabal package.yaml stack.yaml stack.yaml.lock Setup.hs ./
COPY app        ./app/
COPY src        ./src/

RUN stack install

ENTRYPOINT ["index"]
