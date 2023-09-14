FROM sbtscala/scala-sbt:eclipse-temurin-focal-17.0.5_8_1.9.3_2.13.11
ENV GPG_TTY /dev/console
RUN apt update
RUN apt install -y apt-transport-https ca-certificates curl gnupg2 software-properties-common
RUN curl -fsSL "https://download.docker.com/linux/$(lsb_release -is | tr '[:upper:]' '[:lower:]')/gpg" | apt-key add -
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/$(lsb_release -is | tr '[:upper:]' '[:lower:]') $(lsb_release -cs) stable"
RUN apt update
RUN apt install -y docker-ce-cli
COPY . /jochre
RUN gpg --passphrase $GPG_PASSWORD --batch --import /jochre/.secure_files/code-signing-private.pgp
RUN mkdir -p /jochre/modules/yiddish/resources
RUN mv /jochre/docker-compose/sbt_entry_point.sh /bin/
WORKDIR /jochre
ENV JOCHRE3_OCR_DIRECTORY=/jochre
RUN echo $GPG_TTY
RUN gpg -K
RUN gpg --detach-sign --armor --use-agent --output /jochre/LICENSE.sig /jochre/LICENSE
RUN gpg --verify /jochre/LICENSE.sig /jochre/LICENSE
ENTRYPOINT ["/bin/sbt_entry_point.sh", "-Dsbt.boot.directory=/root/.sbt/boot/"]
