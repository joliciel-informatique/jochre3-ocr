FROM sbtscala/scala-sbt:eclipse-temurin-focal-17.0.10_7_1.9.9_3.3.3
RUN apt update
RUN apt install -y apt-transport-https ca-certificates curl gnupg2 software-properties-common
RUN curl -fsSL "https://download.docker.com/linux/$(lsb_release -is | tr '[:upper:]' '[:lower:]')/gpg" | apt-key add -
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/$(lsb_release -is | tr '[:upper:]' '[:lower:]') $(lsb_release -cs) stable"
RUN apt update
RUN apt install -y docker-ce-cli
COPY . /jochre
RUN mkdir -p /jochre/modules/yiddish/resources/lexicons
RUN mkdir -p /jochre/modules/yiddish/resources/models
RUN mv /jochre/docker-compose/sbt_entry_point.sh /bin/
WORKDIR /jochre
ENV JOCHRE3_OCR_DIRECTORY=/jochre
ENTRYPOINT ["/bin/sbt_entry_point.sh", "-Dsbt.boot.directory=/root/.sbt/boot/"]
