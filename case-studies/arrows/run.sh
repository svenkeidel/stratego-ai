#!/bin/sh

strj -i test.str && ./strj-jar -cp strategoxt.jar -i test.java && echo $1 | java -jar test.jar
