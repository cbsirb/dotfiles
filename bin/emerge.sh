#!/bin/bash

ancestor=$1
theirs=$2
yours=$3
merge_result=$4

$EMACS_CLIENT -e "(ediff-merge-files-with-ancestor \"${yours}\" \"${theirs}\" \"${ancestor}\" () \"${merge_result}\")"
$EMACS_CLIENT "${merge_result}"
