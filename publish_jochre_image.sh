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
    local docker_registry=${JOCHRE3_DOCKER_REGISTRY:-registry.gitlab.com}
    echo "Using registry: $docker_registry"

    local docker_username=${JOCHRE3_DOCKER_USERNAME:-deploy}
    echo "Using username: $docker_username"

    if [[ -z ${JOCHRE3_DOCKER_PASSWORD:-} ]]; then
        echoerr "JOCHRE3_DOCKER_PASSWORD not set, trying to use \`pass show jochre/sonatype_deploy\` instead"
        if ! pass show sd/sonatype_deploy > /dev/null; then
            exit 1
        fi
    fi
    local docker_password=${JOCHRE3_DOCKER_PASSWORD:-$(pass show jochre/sonatype_deploy)}
    docker_image="${docker_registry}/jochre3-ocr:${JOCHRE3_OCR_VERSION}"

    echo "DOCKER_IMAGE=${docker_image}" >> /tmp/deploy.env

    echo ${docker_password} | docker login ${docker_registry} -u ${docker_username} --password-stdin

    if  DOCKER_CLI_EXPERIMENTAL=enabled docker manifest inspect $docker_image > /dev/null; then
        echo "$docker_image already released"
        if [[ ${FORCE_REDEPLOY_IMAGE:-} == "true" ]] ; then
            echo "FORCE_REDEPLOY_IMAGE set, lets rebuild and redeploy the migration image "
        elif echo $JOCHRE3_OCR_VERSIONN | grep "\-SNAPSHOT$" ;then
            echo "$JOCHRE3_OCR_VERSION is a SNAPSHOT, lets repush it"
        else
            echo -e "\nIf you want to redeploy you semver must finish by -SNAPSHOT"
            echo "or you can force repdeploy setting FORCE_REDEPLOY_IMAGE=true"
            exit 0
        fi
    fi

    export JOCHRE3_DOCKER_REGISTRY=${docker_registry}
    export JOCHRE3_DOCKER_USERNAME=${docker_username}
    export JOCHRE3_DOCKER_PASSWORD=${docker_password}

    echo "About to build publisher"
    docker-compose -p ${COMPOSE_PROJECT} -f docker-compose/runner.yml build publisher

    echo "About to run publisher"
    docker-compose -p ${COMPOSE_PROJECT} -f docker-compose/runner.yml run -T publisher
}

main "$@"
