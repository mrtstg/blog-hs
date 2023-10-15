FROM haskell:9.6.3-buster AS builder
RUN stack setup --install-ghc
WORKDIR /usr/src/app
COPY . .
RUN stack install

FROM debian:trixie-slim AS runner
ENV TZ="Europe/Moscow"
RUN apt-get update && apt-get install ca-certificates -y
WORKDIR /usr/src/app
COPY --from=builder /root/.local/bin/blog-hs .
CMD /usr/src/app/blog-hs
