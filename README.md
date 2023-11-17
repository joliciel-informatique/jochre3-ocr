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