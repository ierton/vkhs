#!/bin/sh

VKNEWS=./dist/build/vknews/vknews

die() { echo $@ >&2 ; exit 1 ; }

vknews() {
  local pass=$1
  shift
  $VKNEWS -i 60 -p $pass ierton@gmail.com $@
}

vkstuff() {
  cat pirozhok
}

PIRO=`mktemp`
trap "rm $PIRO" EXIT
echo Temp file is $PIRO >&2

echo -n "Enter vk password: "
read PASS
test -z "$PASS" && die "Nonempty password required"
vknews "$PASS" $@ | while read line ; do
  case $line in
    *UTC)
      echo "Ignoring the date line" >&2
      ;;
    "")
      (
      echo == Sending ==
      cat $PIRO
      ./mainsms.sh 9096722988 "`cat $PIRO`"
      echo =============
      ) >&2
      echo -n > $PIRO
      ;;
    *) echo $line >> $PIRO
      ;;
  esac
done

