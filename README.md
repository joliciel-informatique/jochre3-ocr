# Jochre3 OCR

OCR analysis for images, with implementation for Yiddish.

To run locally, first run:
```shell
make init-dev-env
```

To publish a snapshot, run:
```shell
export JOCHRE3_OCR_VERSION=[YOUR VERSION]-SNAPSHOT
sbt publishSigned
```

To publish a release, run:
```shell
export JOCHRE3_OCR_VERSION=[YOUR VERSION]
sbt publishSigned
sbt sonatypeBundleRelease
```

## Running the server locally

Navigate to the project directory, and run the application as follows:

```shell
make init-dev-env
sbt
project api 
run
```

You can then navigate to the Swagger documentation as follows: http://localhost:3434/docs/

See above for authorization.

## Building the docker image

Create your environment variables:
```shell
export JOCHRE3_DOCKER_REGISTRY=registry.gitlab.com
export JOCHRE3_DOCKER_USERNAME=assafurieli@gmail.com
```

Either create an additional JOCHRE3_DOCKER_PASSWORD variable, or (more secure) add the password to pass:
```shell
pass insert jochre/sonatype_deploy
```

Run the publish script
```shell
make publish-image
```

## Continuous integration on GitLab

For the CI script in [.gitlab-ci.yml](.gitlab-ci.yml) to work, you first need to set up a runner with `gitlab-runner` (the docker image below needs to correspond to the one in the yml file):

```shell
sudo gitlab-runner register \
  --non-interactive \
  --url "https://gitlab.com/" \
  --registration-token $REGISTRATION_TOKEN \
  --executor "docker" \
  --description "Docker runner" \
  --docker-image "docker:24" \
  --docker-privileged
```

It will automatically deploy a docker image on tags, on the condition that you add two variables to the Gitlab CI settings: `JOCHRE3_DOCKER_USERNAME` and `JOCHRE3_DOCKER_PASSWORD`.