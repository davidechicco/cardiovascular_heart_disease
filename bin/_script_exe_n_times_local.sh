#!/bin/bash
#
#$ -cwd
#$ -S /bin/bash
#
set -o nounset -o pipefail -o errexit
set -o xtrace

iteTot=10

# 
# ############ CART ############
# 
# i=1
# outputFile=""
# 
# random_numberA=$(shuf -i1-100000 -n1)
# method="cart"
# subdir="../results/"$method"_random"$random_numberA"/"
# mkdir -p $subdir
# 
# for i in $( seq 1 $iteTot )
# do
# 
#   echo $i
#   today=`date +%Y-%m-%d`
#   random_numberB=$(shuf -i1-100000 -n1)
#   jobName=$method"_"$today"_rand"$random_numberB
#   outputFile=$subdir$jobName
#   /usr/bin/Rscript cart.r  > $outputFile 2> $outputFile
# done
# 
# ############ one rule ############
# 
# i=1
# outputFile=""
# 
# random_numberA=$(shuf -i1-100000 -n1)
# method="oneRule"
# subdir="../results/"$method"_random"$random_numberA"/"
# mkdir -p $subdir
# 
# for i in $( seq 1 $iteTot )
# do
# 
#   echo $i
#   today=`date +%Y-%m-%d`
#   random_numberB=$(shuf -i1-100000 -n1)
#   jobName=$method"_"$today"_rand"$random_numberB
#   outputFile=$subdir$jobName
#   /usr/bin/Rscript oner_class.r  > $outputFile 2> $outputFile
# done
# 
# 
# 
# 
# ############ k-nearest neighbors ############
# 
# i=1
# outputFile=""
# 
# random_numberA=$(shuf -i1-100000 -n1)
# method="kNN"
# subdir="../results/"$method"_random"$random_numberA"/"
# mkdir -p $subdir
# 
# for i in $( seq 1 $iteTot )
# do
# 
#   echo $i
#   today=`date +%Y-%m-%d`
#   random_numberB=$(shuf -i1-100000 -n1)
#   jobName=$method"_"$today"_rand"$random_numberB
#   outputFile=$subdir$jobName
#   /usr/bin/Rscript knn_NewDataset.r  > $outputFile 2> $outputFile
# done
# 
# 
# ############ naive bayes ############
# 
# i=1
# outputFile=""
# 
# random_numberA=$(shuf -i1-100000 -n1)
# method="naiveBayes"
# subdir="../results/"$method"_random"$random_numberA"/"
# mkdir -p $subdir
# 
# for i in $( seq 1 $iteTot )
# do
# 
#   echo $i
#   today=`date +%Y-%m-%d`
#   random_numberB=$(shuf -i1-100000 -n1)
#   jobName=$method"_"$today"_rand"$random_numberB
#   outputFile=$subdir$jobName
#   /usr/bin/Rscript naive_bayes.r  > $outputFile 2> $outputFile
# done
# 
# 
# ############ linear regression ############
# 
# i=1
# outputFile=""
# 
# random_numberA=$(shuf -i1-100000 -n1)
# method="linearRegression"
# subdir="../results/"$method"_random"$random_numberA"/"
# mkdir -p $subdir
# 
# for i in $( seq 1 $iteTot )
# do
# 
#   echo $i
#   today=`date +%Y-%m-%d`
#   random_numberB=$(shuf -i1-100000 -n1)
#   jobName=$method"_"$today"_rand"$random_numberB
#   outputFile=$subdir$jobName
#   /usr/bin/Rscript lin_reg_NewDataset.r  > $outputFile 2> $outputFile
# done
# 
# 
# ############ SVM ############
# 
# i=1
# outputFile=""
# 
# random_numberA=$(shuf -i1-100000 -n1)
# method="svm"
# subdir="../results/"$method"_random"$random_numberA"/"
# mkdir -p $subdir
# 
# for i in $( seq 1 $iteTot )
# do
# 
#   echo $i
#   today=`date +%Y-%m-%d`
#   random_numberB=$(shuf -i1-100000 -n1)
#   jobName=$method"_"$today"_rand"$random_numberB
#   outputFile=$subdir$jobName
#   /usr/bin/Rscript svm.r  > $outputFile 2> $outputFile
# done

############ random forest ############

i=1
outputFile=""

random_numberA=$(shuf -i1-100000 -n1)
method="randomForest"
subdir="../results/"$method"_random"$random_numberA"/"
mkdir -p $subdir

for i in $( seq 1 $iteTot )
do

  echo $i
  today=`date +%Y-%m-%d`
  random_numberB=$(shuf -i1-100000 -n1)
  jobName=$method"_"$today"_rand"$random_numberB
  outputFile=$subdir$jobName
  /usr/bin/Rscript random_forest_class_NewDataset.r  > $outputFile 2> $outputFile
done
