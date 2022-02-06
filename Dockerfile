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
RUN apt-get update && apt-get install -y git libpq-dev
COPY docker/elan-init-0.10.2.gz /tmp/elan-init.gz
RUN gunzip /tmp/elan-init.gz && \
    /tmp/elan-init --no-modify-path -y --default-toolchain none
COPY --from=builder /binaries/mathlib-bench-runner /binaries/
ENTRYPOINT ["/binaries/mathlib-bench-runner"]


################### Supervisor ###################

FROM debian:buster-20220125 AS supervisor
RUN mkdir -p /mathlib-bench/supervisor
RUN apt-get update && apt-get install -y git libpq-dev
COPY --from=builder /binaries/mathlib-bench-supervisor /binaries/
ENTRYPOINT ["/binaries/mathlib-bench-supervisor"]
