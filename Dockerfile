FROM ubuntu:20.04

WORKDIR /root

ENV DEBIAN_FRONTEND=noninteractive

# install emacs and utils
RUN apt-get update && \
    apt-get install -y wget software-properties-common wget curl git &&\
    add-apt-repository -y ppa:kelleyk/emacs && \
    apt-get update && \
    apt-get install -y emacs27 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# chinese support and sudo command
RUN apt-get update && \
    apt-get install -y language-pack-en language-pack-zh-hant language-selector-common sudo && \
    apt-get -y install $(check-language-support) && \
    rm -rf /var/lib/apt/lists/*

# clangd 12
RUN apt-get update && \
    apt-get install -y clangd-12 && \
    cp /usr/bin/clangd-12 /usr/bin/clangd && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# texlab
RUN wget "https://github.com/latex-lsp/texlab/releases/download/v2.2.2/texlab-x86_64-linux.tar.gz" -O texlab.tar.gz && tar -xf texlab.tar.gz && mv texlab /usr/bin && rm -f texlab.tar.gz

# nodejs 14 and theia-ide
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - && apt-get update && apt-get install -y nodejs && npm i -g typescript-language-server && npm i -g typescript && rm -rf /var/lib/apt/lists/*

ENV GOPATH="/go"

# gopls and golang-1.16
RUN wget https://golang.org/dl/go1.16.6.linux-amd64.tar.gz -O golang.tar.gz && \
    tar -C /usr/local -xzf golang.tar.gz && \
    rm -rf golang.tar.gz && \
    apt-get clean && \
    /usr/local/go/bin/go install golang.org/x/tools/gopls@latest && \
    rm -rf /var/lib/apt/lists/* /go/cache /go/pkg/mod/cache

WORKDIR /home/code

COPY . .emacs.d
COPY .custom.el .

RUN emacs --script .emacs.d/init.el && chmod 777 . .emacs.d

ENV PATH $PATH:/go/bin:/usr/local/go/bin
ENV TERM xterm-256color
ENV HOME /home/code

ENV LANG C.UTF-8

CMD [ "emacs", "-nw" ]
