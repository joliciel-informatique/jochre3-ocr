#!/bin/bash -eu

echoerr() { echo "$@" 1>&2; }

SCRIPTDIR=$(dirname "$0")

main() {
    export JOCHRE3_OCR_VERSION=$(git tag -l --contains HEAD | cat | { grep "^[0-9]\+\.[0-9]\+\.[0-9]\+\(-SNAPSHOT\)\?" || true;} )

    if [[ -z JOCHRE3_OCR_VERSION  ]] ; then
        echo "No tag corresponding to a matching semver among current tags:"
        git tag -l --contains HEAD | cat
        exit 1
    fi

    if  [[ $(echo "JOCHRE3_OCR_VERSION" | wc -l) != 1 ]] ; then
        echo "More than one tag corresponding to a matching semver":
        echo "JOCHRE3_OCR_VERSION"
        exit 2
    fi

    docker-compose -p ${COMPOSE_PROJECT} -f docker-compose/runner.yml build publisher
    docker-compose -p ${COMPOSE_PROJECT} -f docker-compose/runner.yml run -T publisher
}

main "$@"
