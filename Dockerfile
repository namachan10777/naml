FROM ocaml/ocaml:alpine

RUN apk add --no-cache make
RUN adduser -h /home/naml -s /bin/sh -D naml

USER naml

ENTRYPOINT [ "/bin/sh" ]
