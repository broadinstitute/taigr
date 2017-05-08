set -ex

echo "fn <- devtools::build() ; writeLines(fn, '.generated-release')" | R --vanilla --no-save 
scp `cat .generated-release` datasci@datasci-dev:~/private_html/R-packages/src/contrib
ssh datasci@datasci-dev.broadinstitute.org "cd ~/private_html/R-packages ; use R-3.0 ; ./mkindex"

