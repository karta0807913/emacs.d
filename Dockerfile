FROM ubuntu:20.04

WORKDIR /root

RUN sed -i 's/archive/tw.archive/' /etc/apt/sources.list

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y wget software-properties-common && \
    wget https://github.com/llvm/llvm-project/releases/download/llvmorg-11.0.1/clang+llvm-11.0.1-$(uname -m)-linux-gnu-ubuntu-20.10.tar.xz && \
    tar -xvf clang+llvm-11.0.1-$(uname -m)-linux-gnu-ubuntu-20.10.tar.xz && \
    rm clang+llvm-11.0.1-$(uname -m)-linux-gnu-ubuntu-20.10.tar.xz && \
    cd clang+llvm-11.0.1-$(uname -m)-linux-gnu-ubuntu-20.10 && \
    cp -R . /usr && \
    cd ../ && rm -rf clang+llvm-11.0.1-$(uname -m)-linux-gnu-ubuntu-20.10 && \
    add-apt-repository -y ppa:kelleyk/emacs && \
    apt-get install -y emacs27 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY . /root/.emacs.d/
COPY .custom.el /root/

RUN [ "emacs", "--script", "~/.emacs.d/init.el" ]

ENV TERM=xterm-256color

CMD [ "emacs", "-nw" ]
