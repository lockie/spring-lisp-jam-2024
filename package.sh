#!/usr/bin/env bash

set -euo pipefail

if [ $# -ne 1 ]; then
    echo "USAGE: $0 <package flavor>"
    exit 1
fi

export VERSION=${GITHUB_REF_NAME:-$(git describe --always --tags --dirty=+ --abbrev=6)}

function do_build () {
    CL_SOURCE_REGISTRY=$(pwd) sbcl --dynamic-space-size 4096 --disable-debugger --quit --load package/build.lisp
}

case $1 in
    linux)
        do_build
        linuxdeploy --appimage-extract-and-run --executable=bin/cycle-of-evil \
                    --custom-apprun=package/AppRun \
                    --icon-file=package/icon.png \
                    --desktop-file=package/cycle-of-evil.desktop \
                    --appdir=appimage $(find bin -name "lib*" -printf "-l%p ")
        cp bin/cycle-of-evil appimage/usr/bin
        cp -R Resources appimage/usr
        appimagetool --appimage-extract-and-run --comp xz -g appimage "cycle-of-evil-${VERSION}.AppImage"
        ;;

    windows)
        do_build
        
        ntldd -R bin/* | grep ucrt64 | awk -F '=> ' '{ print $2 }' | awk '{ print $1 }' | xargs -I deps cp deps bin

        convert package/icon.png -define icon:auto-resize=16,32,48,64,256 "$TEMP/icon.ico"
        convert -resize 150x57 -extent 150x57 -gravity center -background white -alpha remove -alpha off package/icon.png "BMP2:$TEMP/icon.bmp"
        makensis package/installer.nsi
        ;;

    macos)
        do_build
        bundle="Cycle of Evil.app"
        contents=$bundle/Contents
        mkdir -p "$contents/MacOS"
        cp -r Resources "$contents"
        cp package/Info.plist "$contents"
        cp package/icon.png "$contents/Resources"
        for library in bin/*.dylib; do
            dylibbundler -of -cd -b -p '@loader_path' -x "$library" -d "$contents/MacOS"
            cp "$library" "$contents/MacOS"
        done
        mv "$contents"/MacOS/libzstd* "$contents/MacOS/libzstd.1.dylib"

        # https://bugs.launchpad.net/sbcl/+bug/1869401
        replace_fr=$(echo -n  "/opt/local/lib/libzstd.1.dylib" | xxd -ps -c1 | tr -d '\n')
        replace_to=$(echo -en "@loader_path/libzstd.1.dylib\x00\x00" | xxd -ps -c1 | tr -d '\n')
        xxd -ps -c1 bin/cycle-of-evil | tr -d '\n' | sed "s/$replace_fr/$replace_to/" | fold -w 2 | xxd -r -p > "$contents/MacOS/cycle-of-evil"
        chmod +x "$contents/MacOS/cycle-of-evil"

        hdiutil create -quiet -srcfolder "$bundle" out.dmg
        # NOTE: ULMO = lzma compression = Catalina+ only
        hdiutil convert -quiet out.dmg -format ULMO -o "cycle-of-evil-${VERSION}.dmg"
        rm out.dmg
        ;;

    *)
        echo "Uknown package flavor: $1"
        exit 1
        ;;
esac
