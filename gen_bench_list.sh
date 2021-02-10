#!/bin/sh

HOGE=$(find dist -type f -name "*.txt" )

for f in $HOGE; do
	echo "\\lstinputlisting[caption="$(echo $f | sed -e "s/_/\\\\_/g")"]{"$f"}"
done
