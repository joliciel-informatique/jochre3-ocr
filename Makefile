COMPOSE_PROJECT ?= "JOCHRE_OCR_TEST"

.PHONY: test start-dep stop-dep
.ONESHELL:

test: start-dep-with-ports-binding
	@ cd "${CURDIR}"
	JOCHRE3_OCR_DIRECTORY="${CURDIR}" sbt test

start-dep-with-ports-binding:
	@ cd "${CURDIR}"
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/deps.yml build
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/deps.yml -f docker-compose/bind-ports.yml up -d

init-dev-env: erase-dep-state start-dep-with-ports-binding
	@ cd "${CURDIR}"
	@sleep 1

start-dep-without-ports-binding:
	@ cd "${CURDIR}"
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/deps.yml up -d

stop-dep:
	@ cd "${CURDIR}"
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/deps.yml -f docker-compose/bind-ports.yml down

erase-dep-state: stop-dep
	@ cd "${CURDIR}"
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/deps.yml -f docker-compose/bind-ports.yml rm

test-ci: erase-dep-state start-dep-without-ports-binding
	@ cd "${CURDIR}"
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/runner.yml build test
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/runner.yml run -T test

debug-test-ci: erase-dep-state start-dep-without-ports-binding
	@ cd "${CURDIR}"
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/runner.yml run --entrypoint /bin/bash test

simulate-ci:
	docker run -v /var/run/docker.sock:/var/run/docker.sock \
		-v $(CURDIR):/app:ro \
    -w /app \
    tiangolo/docker-with-compose:2021-09-18 \
		make test-ci
run:
	@ cd "${CURDIR}"
	JOCHRE3_OCR_DIRECTORY="${CURDIR}" sbt run

clean-docker-compose:
	@ cd "${CURDIR}"
	docker-compose -p $(COMPOSE_PROJECT) -f docker-compose/deps.yml -f docker-compose/bind-ports.yml -f docker-compose/runner.yml -f docker-compose/jochre.yml down --remove-orphans --rmi local


