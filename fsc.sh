#!/usr/bin/env

MONO=$(which mono)
FSC=$(which fsharpc)

compile() {

}

run() {
  mono $2
}

shift 1

case $1 in
  run)
    $(MONO) $2
    ;;

  build)
    $(FSC) --nologo --target:exe -o $2 $2.fs && chmod +x $2
    ;;

  *)
    echo 'Invalid command.'
    ;;
esac
