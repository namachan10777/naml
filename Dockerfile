FROM ocaml:alpine-ocaml-4.12

RUN apk add --no-cache make

ENTRYPOINT [ "/bin/sh" ]
