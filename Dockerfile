FROM haskell:8

RUN apt-get update && \
    apt-get install libpq-dev -y
