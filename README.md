# Jochre3 OCR

OCR software using neural networks for page layout analysis and character recognition with implementation for Yiddish.

Input is either a PDF file or an image.

Output is possible in the following formats:

- `Alto4`: the full [Alto4 XML](https://www.loc.gov/standards/alto/) output down to the glyph level.
- `Text`: the original text, with physical line breaks preserved.
- `ProcessedText`: the text with end-of-line line breaks removed, hyphenated words combined "intelligently" (with hard vs soft hyphen handling), and line breaks between paragraphs.

Yiddish implementation capabilities (possible to apply to other languages):

- Increase probability of known words
- Multi-alphabet management (text with Hebrew, Latin and Cyrillic characters)
- Correct handling of bidi (for numbers)
- Ability to recognize "wide spacing" emphasis as a single word
- Nikkud is currently handled by learning to ignore non-YIVO nikkud.
- Training corpus covering a wide range of page layouts and historical fonts.

If you use this software in your studies, please cite the [following article](https://arxiv.org/abs/2501.08442):

```bibtex
@misc{urieli2025jochre3yiddishocr,
      title={Jochre 3 and the Yiddish OCR corpus},
      author={Assaf Urieli and Amber Clooney and Michelle Sigiel and Grisha Leyfer},
      year={2025},
      eprint={2501.08442},
      archivePrefix={arXiv},
      primaryClass={cs.CL},
      url={https://arxiv.org/abs/2501.08442},
}
```

## Running Jochre 3 OCR locally

Install [Docker Compose](https://docs.docker.com/compose/install/).

In a directory of your choice, create the following two files.

`docker-compose.yml` (replace `1.1.0` below with the latest tag):

```yml
version: "3.7"
services:
  jochre3-ocr:
    image: registry.gitlab.com/jochre/jochre3-ocr:1.1.0
    command: -Dconfig.file=/opt/docker/etc/jochre.conf
    ports:
      - 3434:3434
    volumes:
      - ./jochre.conf:/opt/docker/etc/jochre.conf
    environment:
      DOCUMENT_LAYOUT_ANALYSIS_URL: http://jochre3-dla:8444
      JOCHRE3_OCR_VERSION: 1.1.0
      JOCHRE3_OCR_DIRECTORY: /opt/docker
    logging:
      driver: "json-file"
      options:
        max-size: "5m"
        max-file: "10"
    restart: on-failure
  jochre3-dla:
    image: registry.gitlab.com/jochre/jochre3-dla-server:1.0.5
    logging:
      driver: "json-file"
      options:
        max-size: "5m"
        max-file: "10"
    restart: on-failure
```

`jochre.conf`:

```conf
include "/application"

jochre {
  ocr {
    yiddish {
      glyph-guesser {
        model-path = ${jochre.ocr.directory.core}"/models"
      }
    }
  }
}
```

Next, launch your docker compose file as follows (for Linux command line, may vary for other systems):

```shell
cd [your-directory]
docker compose -f docker-compose.yml up -d
```

If the launch functioned correctly, you should be able to navigate to the Swagger page at the following address: http://localhost:3434/docs/

You can run Jochre from the Swagger page above, or else from the command line (instructions below for Linux using curl).

Command line for running OCR analysis on a local file:

```shell
curl -X 'POST' \
  'http://localhost:3434/ocr/file/zip' \
  -H 'accept: application/zip' \
  -H 'Content-Type: multipart/form-data' \
  -F 'image=@/path/to/nybc200089-11-12.pdf;type=application/pdf' \
  -F 'outputFormats=Alto4,Text,ProcessedText' > nybc200089-11-12.zip
```

Command line for running OCR analysis on a URL:

```shell
curl -X 'POST' \
  'http://localhost:3434/ocr/url/zip' \
  -H 'accept: application/zip' \
  -H 'Content-Type: application/json' \
  -d '{
  "url": "https://iiif.archive.org/iiif/nybc200058$8/full/1800,/0/default.jpg",
  "fileName": "nybc200058_0008.jpg",
  "outputFormats": "Alto4,Text,ProcessedText"
}' > nybc200058_0008.zip
```

If you want to be sure Jochre OCR is working while waiting for a response, you can use the Docker logs command as follows, in a separate command line:

```shell
docker logs --follow --since 2m docker-compose_jochre3-ocr_1
```

To stop the Jochre server, run the following command:

```shell
cd [your-directory]
docker compose -f docker-compose.yml down
```

## Running from source code as a server

Navigate to the project directory, and run the application as follows:

```shell
make init-dev-env
sbt
project api
run
```

You can then navigate to the Swagger documentation as follows: http://localhost:3434/docs/

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

Note that the GitLab CI script automatically builds and publishes a docker image for all tags.

## Building and publishing as a sonatype library

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
