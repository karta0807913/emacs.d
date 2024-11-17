set -x;
TARGET_DIR="/usr/lib/x86_64-linux-gnu";

function copy_items() {
    local base_dir="$(dirname "$1")"
    local lib="$(basename "$1")"
    (
        set -x;
        cp "$base_dir/$lib" "$TARGET_DIR";
        cd "$TARGET_DIR";
        BASE_NAME="$(echo "$lib" | sed 's/lib\(.*\)\.so.*/\1/')";
        if [ "$BASE_NAME" = "$lib" ]; then
            return;
        fi;
        if [ "lib${BASE_NAME}.so" = "$lib" ]; then
            return;
        fi;
        ln -s -f "$lib" "lib${BASE_NAME}.so";
        if [ -f "$base_dir/pkgconfig/$BASE_NAME.pc" ]; then
            cp "$base_dir/pkgconfig/$BASE_NAME.pc" "$TARGET_DIR/pkgconfig/";
        fi
        if [ -f "$base_dir/pkgconfig/lib$BASE_NAME.pc" ]; then
            cp "$base_dir/pkgconfig/lib$BASE_NAME.pc" "$TARGET_DIR/pkgconfig/";
        fi
    )
}

function get_deps() {
    local base_dir="$(dirname "$1")"
    MISSING_LIBRARIES="$(ldd "$1" | grep 'not found' | awk '{print $1}')";
    if ! [ "$MISSING_LIBRARIES" = "" ]; then
        echo "$MISSING_LIBRARIES" | while read -r lib; do
            set -x;
            get_deps "$base_dir/$lib";
            copy_items "$base_dir/$lib";
        done;
    fi
}

get_deps "$1";
copy_items "$1";
