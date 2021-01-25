FROM ubuntu:20.04

WORKDIR /root

ENV DEBIAN_FRONTEND=noninteractive

RUN sed -i 's/archive/tw.archive/' /etc/apt/sources.list

RUN apt-get update && \
    apt-get install -y clangd-10 && \
    cp /usr/bin/clangd-10 /usr/bin/clangd && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN apt-get update && \
    apt-get install -y wget software-properties-common &&\
    add-apt-repository -y ppa:kelleyk/emacs && \
    apt-get update && \
    apt-get install -y emacs27 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY . /root/.emacs.d/
COPY .custom.el /root/

RUN [ "emacs", "--script", "~/.emacs.d/init.el" ]

ENV TERM=xterm-256color

RUN mkdir /root/libs && printf '\
\n\
if ! [ "$#" = "1" ]; then\n\
    echo "usage $0 <executable>";\n\
    exit 1; \n \
fi;\n\
\n\
ldd "$1" | awk %s/lib/{ print $3 }%s | grep "lib" | xargs -i cp {} /root/libs;\n' "'" "'" > packet && \
chmod 544 packet && \
cp /usr/bin/clangd-10 /usr/bin/clangd

CMD [ "emacs", "-nw" ]
