ARG BASE_IMAGE=ubuntu:24.04

FROM ubuntu:20.04 as webkit2gtk
ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /
RUN apt update && apt install -y libwebkit2gtk-4.0-dev

# emacs 30
FROM $BASE_IMAGE as emacs
ENV DEBIAN_FRONTEND=noninteractive
RUN sed -i 's/^Types: deb/Types: deb deb-src/' /etc/apt/sources.list.d/ubuntu.sources
RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get build-dep -y emacs
RUN apt-get update && apt-get install -y libjansson-dev xaw3dg-dev libc6-dev libjpeg-turbo8-dev libncurses5-dev libpng-dev git libtiff5-dev libgif-dev xaw3dg-dev zlib1g-dev libx11-dev libsqlite3-dev libwebp-dev libxpm-dev libxft-dev libotf-dev libfreetype-dev libgccjit-11-dev libxi-dev curl mailutils libwebkit2gtk-4.1-dev

WORKDIR /root
RUN git clone --depth 1 --single-branch https://github.com/tree-sitter/tree-sitter.git
WORKDIR /root/tree-sitter
RUN make install && cp /usr/local/lib/libtree-sitter.so /usr/local/lib/libtree-sitter.so.* /lib/x86_64-linux-gnu/
WORKDIR /root

RUN git clone --depth 1 --single-branch --branch=emacs-30.0.93 https://github.com/emacs-mirror/emacs.git emacs-30

# get old webkit from ubuntu20.04
COPY ./docker/load-deps.sh .
RUN --mount=type=bind,source=/usr/lib/x86_64-linux-gnu,target=/ubuntu20,from=webkit2gtk \
    bash ./load-deps.sh "/ubuntu20/libwebkit2gtk-4.0.so.37";

COPY --from=webkit2gtk /usr/include/webkitgtk-4.0 /usr/include/webkitgtk-4.0
COPY --from=webkit2gtk /usr/include/libsoup-2.4/ /usr/include/libsoup-2.4/

WORKDIR /root/emacs-30
RUN ./autogen.sh
RUN ./configure --with-mailutils --with-json --with-rsvg --with-cairo --with-xwidgets --with-gconf --with-native-compilation --with-xinput2 --with-png --with-jpeg --with-mailutils
RUN make -j$(nproc)
RUN make install

FROM $BASE_IMAGE

WORKDIR /root

ENV DEBIAN_FRONTEND=noninteractive

# install utils
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get update && apt-get install -y curl git unzip wget xclip binutils libgccjit-11-dev

# chinese support and sudo command
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get install -y language-pack-en language-pack-zh-hant language-selector-common sudo curl && \
    apt-get -y install $(check-language-support)

# clangd 17
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get install -y clangd-17 && \
    cp /usr/bin/clangd-17 /usr/bin/clangd

# texlab
RUN wget "https://github.com/latex-lsp/texlab/releases/download/v3.3.1/texlab-x86_64-linux.tar.gz" -O texlab.tar.gz && \
    tar -xf texlab.tar.gz && \
    mv texlab /usr/bin && \
    rm -f texlab.tar.gz

# nodejs 20 and theia-ide
# see https://github.com/nodesource/distributions#ubuntu-versions
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get install -y gpg && \
    curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_20.x nodistro main" | \
        sudo tee /etc/apt/sources.list.d/nodesource.list && \
    apt-get update && \
    apt-get install nodejs -y

ENV GOPATH="/go" \
    PATH=$PATH:/go/bin:/usr/local/go/bin

# gopls and golang-1.22.1
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    wget https://go.dev/dl/go1.23.2.linux-amd64.tar.gz -O golang.tar.gz && \
    tar -C /usr/local -xzf golang.tar.gz && \
    rm -rf golang.tar.gz && \
    /usr/local/go/bin/go install golang.org/x/tools/gopls@latest && \
    /usr/local/go/bin/go clean -modcache -testcache -cache -x && \
    rm -rf /go/cache /go/pkg/mod/cache

# python stuff
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get install -y python3-pip && \
    pip install --break-system-packages --no-cache-dir jedi rope yapf pycodestyle pydocstyle uv black ruff pyright

# rust 1.66.0
ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y && \
    curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - > /tmp/rust-analyzer && \
    mv /tmp/rust-analyzer /usr/local/cargo/bin/rust-analyzer && \
    chmod +x /usr/local/cargo/bin/rust-analyzer

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

# install emacs
RUN --mount=type=bind,source=/root/emacs-30,target=/home/code,from=emacs \
    --mount=type=bind,source=/root/emacs-30,target=/root/emacs-30,from=emacs \
    --mount=type=bind,source=/usr/bin/,target=/usr/bin1/,from=emacs,readwrite \
    --mount=type=bind,source=/lib/x86_64-linux-gnu/,target=/lib/emacs/,from=emacs \
    --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    --mount=type=bind,source=/usr/local/share/emacs/,target=/usr/local/share/emacs-30/,from=emacs \
    mv /usr/bin /usr/bin.original && \
    /usr/bin.original/ln -s /usr/bin1 /usr/bin && \
    while ldd ./src/emacs | grep 'not found'; \
    do \
        ldd ./src/emacs | grep 'not found' | awk '{print $1}' | \
            xargs -t -I{} cp /lib/emacs/{} /lib/x86_64-linux-gnu/; \
    done && \
    cp -f /usr/lib/emacs/glib-2.0/glib-compile-schemas /usr/bin/glib-compile-schemas && \
    cp -r /usr/local/share/emacs-30/ /usr/local/share/emacs/ && \
    make install && rm /usr/bin && /usr/bin.original/mv /usr/bin.original /usr/bin

COPY . .emacs.d
COPY .custom.el .


RUN emacs --script .emacs.d/init.el && bash -c "emacs --script <(echo \"(package-initialize)(require 'all-the-icons)(all-the-icons-install-fonts t)\")" && chmod 777 . .emacs.d

CMD [ "emacs" ]
