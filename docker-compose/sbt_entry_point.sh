#!/bin/bash -eu

main() {
    sbt "-Dsbt.boot.directory=/root/.sbt/boot/" $@
}

main $@
