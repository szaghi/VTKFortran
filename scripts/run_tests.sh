#!/bin/bash

all_passed () {
  local array="$1[@]"
  local ok=1
  for element in "${!array}"; do
    if [ "$element" == 'F' ]; then
      ok=0
      break
    fi
  done
  echo $ok
}

echo "Run all tests"
declare -a tests_executed
for e in $( find ./exe/ -type f -executable -print ); do
  is_passed=`$e | grep -i "Are all tests passed? " | awk '{print $5}'`
  tests_executed=("${tests_executed[@]}" "$is_passed")
  echo "  run test $e, is passed? $is_passed"
  if [ "$is_passed" == 'F' ]; then
    echo
    echo "Test failed"
    ./exe/$e
  fi
done
passed=$(all_passed tests_executed)
echo "Number of tests executed ${#tests_executed[@]}"
if [ $passed -eq 1 ]; then
  echo "All tests passed"
  exit 0
else
  echo "Some tests failed"
  exit 1
fi
