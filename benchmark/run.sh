#!/bin/bash

echo "note: SBCL does x100 more loops"

ros -L sbcl    run.ros &> $(git rev-parse HEAD).sbcl.log &
ros -L ccl-bin run.ros &> $(git rev-parse HEAD).ccl.log  &
wait
