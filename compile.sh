#!/usr/bin/env sh

set -e

usage() {
  printf \
"Compiler script. Usage:

\t(1) Compile markdown to pdf.

\t\033[1m$0 \033[1;3m[OPTIONS]...\033[0m

\t\t\033[1;3m-w | --watch\033[0m     - recompile when source files change
\t\t\033[1;3m-c | --codebraid\033[0m - use codebraid
\t\t\033[1;3m-v | --verbose\033[0m   - print additional information

\t\tShort options can be collated, e.g. \033[1;3m-cv\033[0m or \033[1;3m-wvc\033[0m.


\t(2) Clear cache and remove temporaries.

\t\033[1m$0 clean\033[0m


\t(3) Show usage.

\t\033[1m$0 help\033[0m
\t\033[1m$0 --help\033[0m
\t\033[1m$0 -h\033[0m
"
}

compile() {
  if [ $CODEBRAID ]; then
    pre="codebraid"
    post="--overwrite"
  fi
  if [ ! "$VERBOSE" ]; then
    export PYTHONWARNINGS=ignore
  fi
 $pre pandoc -t latex --pdf-engine=xelatex --variable "geometry:margin=1cm" -o "$OUTFILE" "$INFILE" $post 2>&1
}

cleanup() {
  kill "$SPIN_PID" >/dev/null 2>&1
}

togglespinner() {
  [ "$SPIN" = "1" ] && SPIN=0 || SPIN=1

  if [ "$SPIN" = "1" ]; then
    (
    chars='-/|\'
    while :; do
      for ii in $(seq 1 4); do
        printf "\010"
        printf -- "$chars" | cut -b "$ii" | tr -d '\n'
        sleep "0.1s"
      done
    done
    ) &
    SPIN_PID=$!
  else
    printf "\010 "
    kill $SPIN_PID
  fi
}

[ ! "$INFILE" ] && INFILE="notes.md"
[ ! "$OUTFILE" ] && OUTFILE="notes.pdf"

trap cleanup 0 1 2 15

case "$1" in
  -h|--help|help)
    usage
    ;;
  clean)
    rm -rf "$OUTFILE" "_codebraid"
    ;;
  __compile)
    timestamp="[$(date '+%H:%M:%S')]:"
    printf "\033[1m$timestamp Compiling (w/ opt:"
    [ $CODEBRAID ] && printf " -c"
    [ $VERBOSE ] && printf " -v"
    printf ") ... "
    togglespinner
    if output=$(compile); then
      togglespinner
      printf "success \033[1;32m✓\033[0;1m File: '\033[3m$OUTFILE\033[0m'\n"
    else
      err=1
      togglespinner
      printf "fail \033[1;31m✘\033[0;1m\n"
    fi
    if [ "$output" ]; then
      printf "$timestamp Produced the following ouput:\n\033[0m"
      printf "$output" | sed 's/^/'"$timestamp"' /g'
    fi
    printf "\033[0m"
    if [ "$err" ]; then exit 1; fi
    ;;
  __watch)
    echo "$INFILE\n$0" | CODEBRAID=$CODEBRAID VERBOSE=$VERBOSE entr -cr ./compile.sh __compile
    ;;
  *)
    for opt in "$@"; do
      [ "$opt" = "--watch" ] && opt=-w
      [ "$opt" = "--codebraid" ] && opt=-c
      [ "$opt" = "--verbose" ] && opt=-v
      printf -- "$opt" | grep -qE '^-.*c.*$' && CODEBRAID=1
      printf -- "$opt" | grep -qE '^-.*v.*$' && VERBOSE=1
      printf -- "$opt" | grep -qE '^-.*w.*$' && WATCH=1
    done
    if [ "$WATCH" ]; then
      CODEBRAID=$CODEBRAID VERBOSE=$VERBOSE $0 __watch
    else
      CODEBRAID=$CODEBRAID VERBOSE=$VERBOSE $0 __compile
    fi
    ;;
esac
