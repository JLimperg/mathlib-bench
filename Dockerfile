################### Builder ###################

FROM haskell:8.10.7-buster AS builder
WORKDIR /source

# build system dependencies
RUN apt-get update && apt-get install -y libpq-dev

# build Haskell dependencies
COPY LICENSE README.md Setup.hs mathlib-bench.cabal stack.yaml stack.yaml.lock /source/
RUN stack build --no-interleaved-output --only-dependencies

# build executables
COPY lib /source/lib/
COPY runner /source/runner/
COPY supervisor /source/supervisor/
COPY static /source/static/
RUN stack build --no-interleaved-output
RUN mkdir /binaries
RUN cp $(stack path --local-install-root)/bin/* /binaries/


#################### Runner ####################

FROM debian:buster-20220125 AS runner
RUN mkdir -p /mathlib-bench/runner
RUN apt-get update && apt-get install -y git libpq-dev cloc
COPY docker/elan-x86_64-unknown-linux-gnu.tar.gz /tmp/elan-init.tar.gz
RUN tar -C /tmp -xf /tmp/elan-init.tar.gz && \
    /tmp/elan-init --no-modify-path -y --default-toolchain none
COPY --from=builder /binaries/mathlib-bench-runner /binaries/
ENTRYPOINT ["/binaries/mathlib-bench-runner"]


################### Supervisor ###################

FROM debian:buster-20220125 AS supervisor
RUN mkdir -p /mathlib-bench/supervisor
RUN apt-get update && apt-get install -y git libpq-dev
COPY --from=builder /binaries/mathlib-bench-supervisor /binaries/
ENTRYPOINT ["/binaries/mathlib-bench-supervisor"]
