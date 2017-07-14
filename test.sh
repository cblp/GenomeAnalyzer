#!/bin/bash
set -eux -o pipefail

# stack test --pedantic
stack exec -- time GenomeAnalyzer-exe > result_.txt
diff --brief result.txt result_.txt
