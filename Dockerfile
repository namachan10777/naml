FROM ocaml/opam:alpine

USER root
RUN apk add --no-cache make m4

USER opam
RUN opam switch create 4.12.0 && \
	opam install ocamlfind

ENTRYPOINT [ "/bin/sh" ]
