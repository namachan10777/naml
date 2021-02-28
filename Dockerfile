FROM ocaml/ocaml:alpine

RUN apk add --no-cache make

ENTRYPOINT [ "/bin/sh" ]
