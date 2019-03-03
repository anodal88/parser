#!/usr/bin/env scala

object HelloWorld extends App {
  println("Hello, world!")
}
HelloWorld.main(args)

source=./src/main/resources/templates/origin/
destination=./src/generated
component=""
#
#
#Get command parameters
while getopts ":s:d:c:" opt; do
  case $opt in
    s) source="$OPTARG"
    ;;
    d) destination="$OPTARG"
    ;;
    c) component="$OPTARG"
    ;;
  esac
done
#printf "Argument source is %s\n" "$source"
#printf "Argument destination is %s\n" "$destination"
#printf "Argument component filename is %s\n" "$component"


#Iterate over all soy files of the given folder

#for filename in ./src/main/resources/templates/origin/*.soy; do
#       "$filename" "Logs/$(basename "$filename" .soy)_Log$i.txt"
#done

scala
println("hello")
