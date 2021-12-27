#!/bin/sh

CONTAINERD=""
if which podman 2>/dev/null >/dev/null; then
    CONTAINERD="podman"
elif which docker 2>/dev/null >/dev/null; then
    CONTAINERD="docker"
else
    echo "container manager not found"
    exit 127
fi

echo "Updating exists container..."

containers="$(${CONTAINERD} container ls -a --format '{{ .Names }} {{ .Image }}' | grep 'my_editor' | awk '{print $1}')"

for container in ${containers};
do
    echo "updating ${container}";
    ${CONTAINERD} cp "${HOME}/.emacs.d/init.el" "${container}:/home/code/.emacs.d";
    ${CONTAINERD} cp "${HOME}/.emacs.d/lisp" "${container}:/home/code/.emacs.d";
done

echo "
Updating images..."

images="$(${CONTAINERD} image list --format '{{ .Repository }}:{{ .Tag }}' | grep 'my_editor')"
for image in ${images};
do
    echo "updating ${image}"
    ${CONTAINERD} run --rm -it -d --name updating_container ${image} emacs -nw
    ${CONTAINERD} cp "${HOME}/.emacs.d/init.el" "updating_container:/home/code/.emacs.d";
    ${CONTAINERD} cp "${HOME}/.emacs.d/lisp" "updating_container:/home/code/.emacs.d";
    ${CONTAINERD} commit updating_container ${image}
    ${CONTAINERD} stop updating_container
done
