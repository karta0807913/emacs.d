FROM openjdk:18 as java

FROM localhost/my_editor:base

ENV JAVA_HOME /usr/java/openjdk-18
ENV PATH $JAVA_HOME/bin:$PATH

COPY --from=java /usr/java/openjdk-18 /usr/java/openjdk-18
COPY --from=java /etc/pki/ca-trust/extracted/java/cacerts $JAVA_HOME/lib/security/cacerts
RUN mkdir -p .mvn/wrapper && \
    curl https://raw.githubusercontent.com/emacs-lsp/lsp-java/master/install/.mvn/wrapper/MavenWrapperDownloader.java > .mvn/wrapper/MavenWrapperDownloader.java && \
    curl https://raw.githubusercontent.com/emacs-lsp/lsp-java/master/install/.mvn/wrapper/maven-wrapper.properties > .mvn/wrapper/maven-wrapper.properties && \
    curl https://raw.githubusercontent.com/emacs-lsp/lsp-java/master/install/mvnw > mvnw && \
    curl https://raw.githubusercontent.com/emacs-lsp/lsp-java/master/install/pom.xml > pom.xml && \
    mv mvnw /usr/bin && \
    chmod 755 /usr/bin/mvnw && \
    mvnw -Djdt.js.server.root=/home/code/.emacs.d/.cache/lsp/eclipse.jdt.ls/ \
            -Djunit.runner.root=/home/code/.emacs.d/eclipse.jdt.ls/test-runner/ \
            -Djunit.runner.fileName=junit-platform-console-standalone.jar \ 
            -Djava.debug.root=/home/code/.emacs.d/.cache/lsp/eclipse.jdt.ls/bundles \
            clean package -Djdt.download.url=https://download.eclipse.org/jdtls/milestones/1.2.0/jdt-language-server-1.2.0-202106301459.tar.gz && \
    rm -rf mvnm pom.xml target /tmp/* && \
    chmod -R 777 /home/code/.emacs.d/.cache
