#!/bin/sh
# usage: tools/ol_compare.sh "<native command prefix>"   e.g.  tools/ol_compare.sh /tmp/ocb/ol
# Runs three open-loop scenarios on the JS reference and on a native driver and diffs the traces.
cd "$(dirname "$0")/.."
NATIVE="$1"; rc=0
for s in "0 3600" "1 3600" "2 3600" "2 5400 220"; do
  node tools/ol_node.js $s > /tmp/ol_ref.txt
  $NATIVE $s > /tmp/ol_got.txt 2>/tmp/ol_err.txt
  if cmp -s /tmp/ol_ref.txt /tmp/ol_got.txt; then echo "  scenario [$s]: IDENTICAL ($(wc -l < /tmp/ol_ref.txt) samples)"
  else echo "  scenario [$s]: DIFFERENT"; diff /tmp/ol_ref.txt /tmp/ol_got.txt | head -4; head -3 /tmp/ol_err.txt; rc=1; fi
done
exit $rc
