#!/bin/sh

HOGE=$(find -type f -name "*.ml" | grep -v "_build")

for f in $HOGE; do
	echo "\\lstinputlisting[caption="$(echo $f | sed -e "s/_/\\\\_/g")"]{"$f"}"
done
