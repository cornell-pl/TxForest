#!/bin/bash

# Without Problems

names=(
  "aaa17"
  "bcd97"
  "fcq62"
  "hvh01"
  "jd753"
  "jnf27"
  "pab93"
  "wfx20"
  "yt497"
  "zzz23"
)

hws=5
maxScore=100

for ((hw=1; hw<=hws; hw++));
do 
  mkdir -p "hw$hw";
  pushd "hw$hw";
  echo "$maxScore" -n > max;
  for name in "${names[@]}" ;
  do 
    score=$((RANDOM%maxScore))
    echo -n "$score" > $name;
  done 
  popd;
done

# With Problems
# names=(
#   "aaa17"
#   "bcd97"
#   "fcq62"
#   "hvh01"
#   "jd753"
#   "jnf27"
#   "pab93"
#   "wfx20"
#   "yt497"
#   "zzz23"
# )

# hws=5
# maxProblems=10
# maxScore=10

# for ((hw=1; hw<=hws; hw++));
# do 
#   mkdir -p "hw$hw";
#   pushd "hw$hw";
#   numProblems=$((RANDOM%(maxProblems-3) + 3));
#   for name in "${names[@]}" ;
#   do 
#     mkdir -p "$name";
#     pushd "$name";
#     for ((prob=1; prob<=numProblems; prob++));
#     do
#       exists=$((RANDOM%2))
#       if [ $exists -eq 0 ];
#       then
#         score=$((RANDOM%maxScore))
#         echo "$score/$maxScore" > $prob;
#       fi
#     done
#     popd;
#   done 
#   popd;
# done
