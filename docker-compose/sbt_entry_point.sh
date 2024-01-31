#!/bin/bash -eu

main() {
    if [[ -n "${JOCHRE3_DOCKER_REGISTRY:-}" ]] && [[ -n "${JOCHRE3_DOCKER_USERNAME:-}" ]] && [[ -n "${JOCHRE3_DOCKER_PASSWORD:-}" ]]; then
        docker login "${JOCHRE3_DOCKER_REGISTRY}" -u "${JOCHRE3_DOCKER_USERNAME}" -p "${JOCHRE3_DOCKER_PASSWORD}"
    elif [[ -n "${JOCHRE3_DOCKER_REGISTRY:-}" ]] || [[ -n "${JOCHRE3_DOCKER_USERNAME:-}" ]] || [[ -n "${JOCHRE3_DOCKER_PASSWORD:-}" ]]; then
        >&2 echo "You set at least one of the following env var: JOCHRE3_DOCKER_REGISTRY, JOCHRE3_DOCKER_USERNAME, JOCHRE3_DOCKER_PASSWORD"
        >&2 echo "Please set all of them or none of them if you don't need to publish"
        if [[ -z "${JOCHRE3_DOCKER_REGISTRY:-}" ]]; then
            >&2 echo "JOCHRE3_DOCKER_REGISTRY not set"
        fi
        if [[ -z "${JOCHRE3_DOCKER_USERNAME:-}" ]]; then
            >&2 echo "JOCHRE3_DOCKER_USERNAME not set"
        fi
        if [[ -z "${JOCHRE3_DOCKER_PASSWORD:-}" ]]; then
            >&2 echo "JOCHRE3_DOCKER_PASSWORD not set"
        fi
        exit 1
    elif [[ -z "${JOCHRE3_DOCKER_REGISTRY:-}" ]] && [[ -z "${JOCHRE3_DOCKER_USERNAME:-}" ]] && [[ -z "${JOCHRE3_DOCKER_PASSWORD:-}" ]]; then
        >&2 echo "WARNING: None of the docker registry env are set (JOCHRE3_DOCKER_REGISTRY, JOCHRE3_DOCKER_USERNAME, JOCHRE3_DOCKER_PASSWORD)"
        >&2 echo "Therefore docker:publish can't work"
    fi
    sbt "-Dsbt.boot.directory=/root/.sbt/boot/" $@
}

main $@
