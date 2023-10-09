ARG BASE_IMAGE=ubuntu:22.04

FROM $BASE_IMAGE as emacs
ENV DEBIAN_FRONTEND=noninteractive
RUN sed -i 's/^# deb-src/deb-src/' /etc/apt/sources.list
RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get build-dep -y emacs
RUN apt-get install -y libgtk-3-dev libwebkit2gtk-4.0-dev libtree-sitter-dev libjansson-dev xaw3dg-dev libc6-dev libjpeg-turbo8-dev libncurses5-dev libpng-dev git libtiff5-dev libgif-dev xaw3dg-dev zlib1g-dev libx11-dev libsqlite3-dev libwebp-dev libxpm-dev libxft-dev libotf-dev libfreetype-dev libgconf2-dev libgconf2-dev libgccjit-11-dev libxi-dev curl
WORKDIR /root
RUN git clone https://github.com/tree-sitter/tree-sitter.git
RUN git clone --depth 1 --single-branch --branch=emacs-29 git://git.sv.gnu.org/emacs.git emacs-29
WORKDIR /root/emacs-29
RUN ./autogen.sh
RUN ./configure --with-mailutils --with-json --with-rsvg --with-cairo --with-xwidgets --with-gconf --with-native-compilation --with-xinput2
RUN make -j$(nproc)
RUN make install

RUN ldd /usr/local/bin/emacs | sed 's/.*=> \(.*\) .*/\1/' | grep -v '(' | tee libs.txt

FROM $BASE_IMAGE

WORKDIR /root

ENV DEBIAN_FRONTEND=noninteractive

# install emacs
RUN --mount=type=bind,source=/root/emacs-29,target=/root,from=emacs \
    --mount=type=bind,source=/usr/bin/,target=/usr/bin/,from=emacs,readwrite \
    --mount=type=bind,source=/lib/x86_64-linux-gnu/,target=/lib/emacs/,from=emacs \
    --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    sed 's/\/lib\/x86_64-linux-gnu\//\/lib\/emacs\//' libs.txt | xargs -I{} cp --dereference -f {} /lib/x86_64-linux-gnu/ && \
    rm -rf /usr/bin/glib-compile-schemas && \
    cp -f /usr/lib/emacs/glib-2.0/glib-compile-schemas /usr/bin/glib-compile-schemas && \
    mkdir -p /usr/local/share/emacs/29.1.50/etc/charsets && \
    make install

# install utils
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get install -y curl git unzip wget xclip binutils libgccjit-11-dev imagemagick

# chinese support and sudo command
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get install -y language-pack-en language-pack-zh-hant language-selector-common sudo curl && \
    apt-get -y install $(check-language-support)

# clangd 12
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get install -y clangd-12 && \
    cp /usr/bin/clangd-12 /usr/bin/clangd

# texlab
RUN wget "https://github.com/latex-lsp/texlab/releases/download/v3.3.1/texlab-x86_64-linux.tar.gz" -O texlab.tar.gz && \
    tar -xf texlab.tar.gz && \
    mv texlab /usr/bin && \
    rm -f texlab.tar.gz

# nodejs 17 and theia-ide
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    curl -sL https://deb.nodesource.com/setup_18.x | bash - && \
     apt-get update && apt-get install -y nodejs && \
     npm i -g typescript-language-server && \
     npm i -g typescript && \
     npm cache clean --force

ENV GOPATH="/go" \
    PATH=$PATH:/go/bin:/usr/local/go/bin

# gopls and golang-1.20
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    wget https://go.dev/dl/go1.20.linux-amd64.tar.gz -O golang.tar.gz && \
    tar -C /usr/local -xzf golang.tar.gz && \
    rm -rf golang.tar.gz && \
    /usr/local/go/bin/go install golang.org/x/tools/gopls@latest && \
    /usr/local/go/bin/go clean -modcache -testcache -cache -x && \
    rm -rf /go/cache /go/pkg/mod/cache

# python stuff
RUN --mount=type=bind,source=/var/lib/apt/lists/,target=/var/lib/apt/lists/,from=emacs,rw \
    apt-get install -y python3-pip && \
    pip install --no-cache-dir jedi rope yapf pycodestyle pydocstyle python-lsp-server

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

COPY . .emacs.d
COPY .custom.el .

RUN emacs --script .emacs.d/init.el && chmod 777 . .emacs.d

CMD [ "emacs" ]
