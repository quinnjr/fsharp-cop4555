#!/usr/bin/env sh

if [ $# -lt 1 ]; then
  echo 'No valid input was specified.'
elif [ $# -gt 2 ]; then
  echo 'Too many inputs.'
else
  $(which fsharpc) --nologo --target:exe -o:${1%.fs} $1 && chmod +x ${1%.fs}
  $(which mono) ${1%.fs}
fi
