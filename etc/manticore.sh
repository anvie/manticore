#!/bin/sh
java -Dfile.encoding=UTF-8 -cp manticore.jar com.ansvia.manticore.Manticore $1 "$2" "$3"
