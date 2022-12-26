FROM ubuntu:20.04

WORKDIR /root

ENV DEBIAN_FRONTEND=noninteractive

# install emacs and utils
RUN apt-get update && \
    apt-get install -y wget software-properties-common wget curl git && \
    add-apt-repository -y ppa:kelleyk/emacs && \
    apt-get update && \
    apt-get install -y emacs28 xclip && \
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
RUN wget "https://github.com/latex-lsp/texlab/releases/download/v3.3.1/texlab-x86_64-linux.tar.gz" -O texlab.tar.gz && \
    tar -xf texlab.tar.gz && \
    mv texlab /usr/bin && \
    rm -f texlab.tar.gz

# nodejs 17 and theia-ide
RUN curl -sL https://deb.nodesource.com/setup_17.x | bash - && \
     apt-get update && apt-get install -y nodejs && \
     npm i -g typescript-language-server && \
     npm i -g typescript && \
     npm cache clean --force && \
     rm -rf /var/lib/apt/lists/*

ENV GOPATH="/go" \
    PATH=$PATH:/go/bin:/usr/local/go/bin

# gopls and golang-1.19.4
RUN wget https://golang.org/dl/go1.19.4.linux-amd64.tar.gz -O golang.tar.gz && \
    tar -C /usr/local -xzf golang.tar.gz && \
    rm -rf golang.tar.gz && \
    apt-get clean && \
    /usr/local/go/bin/go install golang.org/x/tools/gopls@latest && \
    /usr/local/go/bin/go clean -modcache -testcache -cache -x && \
    rm -rf /var/lib/apt/lists/* /go/cache /go/pkg/mod/cache

# python stuff
RUN apt-get update && \
    apt-get install -y python3-pip && \
    pip install --no-cache-dir jedi rope yapf pycodestyle pydocstyle python-lsp-server && \
    rm -rf /var/lib/apt/lists/*

# rust 1.66.0
ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > rustup-init && \
    chmod +x rustup-init && \
    ./rustup-init -y --no-modify-path --profile minimal --default-toolchain 1.66.0 && \
    rm rustup-init && \
    curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - > /usr/local/cargo/bin/rust-analyzer && \
    chmod +x /usr/local/cargo/bin/rust-analyzer && \
    rustup component add rustfmt && \
    apt-get remove -y --auto-remove && \
    rm -rf /var/lib/apt/lists/*;

WORKDIR /home/code

ENV TERM=xterm-256color \
    HOME=/home/code \
    LANG=C.UTF-8

# font
RUN curl -L https://github.com/microsoft/cascadia-code/releases/download/v2111.01/CascadiaCode-2111.01.zip > /tmp/CascadiaCode.zip && \
    mkdir -p ~/.local/share/fonts/ /tmp/fonts/ && \
    cd /tmp/fonts && \
    unzip -u ../CascadiaCode.zip && \
    mv /tmp/fonts/ttf/CascadiaCode.ttf ~/.local/share/fonts && \
    rm -rf /tmp/fonts/ /tmp/CascadiaCode.zip

COPY . .emacs.d
COPY .custom.el .

RUN emacs --script .emacs.d/init.el && chmod 777 . .emacs.d

CMD [ "emacs" ]
