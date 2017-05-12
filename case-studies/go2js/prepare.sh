#!/bin/sh

sed -f overlay.sed go2js.str > go2js-desugared.str
java -Xms512m -Xmx1024m -Xss16m -jar strategoxt.jar --lib -F -i go2js-desugared.str > go2js.aterm
./pp-stratego -i go2js.aterm -a > go2jspp.str
