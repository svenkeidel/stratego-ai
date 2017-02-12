#!/bin/sh

strj -i $1.str && ./strj-jar -cp strategoxt.jar -i $1.java && echo $2 | java -jar $1.str
