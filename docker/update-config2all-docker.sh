#!/bin/sh

containers="$(docker container ls -a --format '{{ .Names }} {{ .Image }}' | grep 'my_editor' | awk '{print $1}')"

for container in ${containers};
do
    echo "updating ${container}";
    docker cp "${HOME}/.emacs.d/init.el" "${container}:/home/code/.emacs.d";
    docker cp "${HOME}/.emacs.d/lisp" "${container}:/home/code/.emacs.d";
done
