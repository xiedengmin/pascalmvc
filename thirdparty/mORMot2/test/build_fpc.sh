#!/bin/bash

# Build a mORMot2 tests
# Require an FPC3.2 compiler to be installed:
#   wget -O fpc-laz_3.2.0-1_amd64.deb https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/fpc-laz_3.2.0-1_amd64.deb/download
#   sudo apt install ./fpc-laz_3.2.0-1_amd64.deb
#
# Caller may have defined the following variables:
# TARGET=linux - compile target (win32, win64 etc. in case cross compiler is installed). Default is `linux`
# ARCH=x86_64 - compile arch(i386, arm etc.). Default is `x86_64`
# BIN=/tmp/mormot2 - output folder. Default is `/tmp/mormot2`
#
# Call example to cross compile from linux to win64:
# TARGET=win64 ./build_fpc.sh


# Used fpc command line switches:
# -Scgi 	- Support operators like C; Enable LABEL and GOTO(default for -MDelphi); Inlining
# -Cg PIC code 	- for Linux library only (slowed code for program)
# -Ci 		- IO checking
# -O2 		- optimization level
# -g -gl -gw2 -Xg- Generate debug information; Use line info unit (show more info with backtraces); DWARFv2 debug info; debug info in separate file
# -k'-rpath=$ORIGIN' - link to a library in the same folder as program
# -veiq -vw-n-h - verbose(errors, info, message numbers) no warnings, no notes, no hints
# -B 		- build all
# -Se10 	- halts after 10 error 
# to switch to x64MM -dFPC_SYNCMEM should be removed and -dFPC_X64MM -dFPCMM_SERVER added

if [ -t 1 ] ; then #stdout is a terminal
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  NC='\033[0m' # No Color
else
  RED=''
  GREEN=''
  NC=''
fi
script_successful(){
  echo -e "$GREEN Build for $ARCH_TG success. Tests can be executed from\n $BIN/fpc-$ARCH_TG/$dest_fn$NC"
  exit 0
}
script_aborted() {
  echo -e "$RED******Build for $ARCH_TG fail******$NC"
  exit 1
}
# On error
err_report() {
  >&2 echo "Error in $0 on line $1"
  script_aborted
}
trap 'err_report $LINENO' ERR

# uncomment line below to echo commands to console
# set -x

# get a mORMot folder name based on this script location
MORMOT2_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"
TARGET="${TARGET:-linux}"
ARCH="${ARCH:-x86_64}"
ARCH_TG="$ARCH-$TARGET"

LIB2="${LIB2:-$MORMOT2_ROOT}"
BIN="${BIN:-/tmp/mormot2}"
STATIC="${STATIC:-$LIB2/static}"

mkdir -p "$BIN/fpc-$ARCH_TG/.dcu"
rm -f "$BIN"/fpc-"$ARCH_TG"/.dcu/*

if [ -f "$LIB2"/test/mormot2tests.cfg ]; then
  mv -f "$LIB2/test/mormot2tests.cfg"  "$LIB2/test/mormot2tests.cfg.bak"
fi

dest_fn=mormot2tests
if [[ $TARGET == win* ]]; then
  dest_fn="$dest_fn.exe"
fi

# suppress warnings
# Warning: (5059) Function result variable does not seem to be initialized
# Warning: (5036) Local variable XXX does not seem to be initialized
# Warning: (5089) Local variable XXX of a managed type does not seem to be initialized
# Warning: (5090) Variable XXX of a managed type does not seem to be initialized
SUPRESS_WARN=-vm11047,6058,5092,5091,5060,5058,5057,5028,5024,5023,4081,4079,4055,3187,3124,3123,5059,5036,5089,5090

set -o pipefail

fpc -MDelphi -Sci -Ci -O3 -g -gl -gw2 -Xg -k'-rpath=$ORIGIN' -k-L$BIN \
  -T$TARGET -P$ARCH \
  -veiq -v-n-h- $SUPRESS_WARN \
  -Fi"$BIN/fpc-$ARCH_TG/.dcu" -Fi"$LIB2/src/core" -Fi"$LIB2/src/db" -Fi"$LIB2/src/rest" \
  -Fl"$STATIC/$ARCH-$TARGET" \
  -Fu"$LIB2/src/core" -Fu"$LIB2/src/db" -Fu"$LIB2/src/rest" -Fu"$LIB2/src/crypt" \
    -Fu"$LIB2/src/app" -Fu"$LIB2/src/net" -Fu"$LIB2/src/lib" -Fu"$LIB2/src/orm" -Fu"$LIB2/src/soa" \
  -FU"$BIN/fpc-$ARCH_TG/.dcu" -FE"$BIN/fpc-$ARCH_TG" -o"$BIN/fpc-$ARCH_TG/$dest_fn" \
  -dFPC_SYNCMEM -dDOPATCHTRTL -dFPCUSEVERSIONINFO1 \
  -B -Se1 "$LIB2/test/mormot2tests.dpr" | grep "[Warning|Error|Fatal]:"

if [ -f "$LIB2/test/mormot2tests.cfg.bak" ]; then
  mv -f "$LIB2/test/mormot2tests.cfg.bak"  "$LIB2/test/mormot2tests.cfg"
fi

script_successful
