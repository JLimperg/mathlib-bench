#!/bin/bash

base_dir="$(dirname "$0")"
systemd_dir="$base_dir/systemd"
bin_dir="$(stack --allow-different-user path --local-install-root)/bin"

id -u mathlib-bench &> /dev/null
if test $? -ne 0; then
    useradd --system --create-home --user-group mathlib-bench
fi

install -o mathlib-bench -g mathlib-bench -m 700 "$bin_dir/mathlib-bench-backend"  /home/mathlib-bench/
install -o mathlib-bench -g mathlib-bench -m 700 "$bin_dir/mathlib-bench-frontend" /home/mathlib-bench/

if test -d /etc/systemd/system; then
    install -o 0 -g 0 -m 600 "$systemd_dir/mathlib-bench-backend.service" /etc/systemd/system
    install -o 0 -g 0 -m 600 "$systemd_dir/mathlib-bench-frontend.service" /etc/systemd/system
    systemctl daemon-reload
fi
