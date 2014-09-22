#!/bin/sh
for j in hw2 hw3; do 
  cd /home/bluephoenix47/cs2500-grades/cs2500-grades/$j
  for i in */$j.zip; do echo "A" | unzip $i -d /home/bluephoenix47/cs2500-server/cs2500-server/$j; done
done

cd /home/bluephoenix47/cs2500-scripts

#racket update-grades.rkt exam1 60 
#racket update-grades.rkt honors-exam1 42 
#
#racket update-grades.rkt exam-two 66 
#racket update-grades.rkt honors-exam-two 40 
#
#for i in {honors-,}quiz{5,6,7,8,9,10,11}; do
#  racket update-grades.rkt $i 1 
#done
#
#racket update-grades.rkt whim1 5

cd eli-scripts
./compute-grades
