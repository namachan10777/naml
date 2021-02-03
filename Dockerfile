FROM ubuntu:20.10

RUN apt-get update && \
	apt-get upgrade -y && \
	apt-get install -y wget make m4 gcc patch unzip git bzip2 pkg-config gzip

RUN wget https://github.com/ocaml/opam/releases/download/2.0.7/opam-2.0.7-x86_64-linux -O /usr/local/bin/opam && \
	chmod +x /usr/local/bin/opam

RUN useradd coins -m
USER coins

RUN opam init --compiler=4.11.1 --disable-sandboxing
RUN opam repository add satysfi-external https://github.com/gfngfn/satysfi-external-repo.git && \
	opam repository add satyrographos https://github.com/na4zagin3/satyrographos-repo.git && \
	opam update

RUN opam install -y opam-depext && \
	opam depext satysfi satysfi-dist satyrographos && \
	opam pin add -y ocamlformat 0.15.0 && \
	opam install -y satysfi satysfi-dist satyrographos \
					dune ppx_deriving sedlex menhir core oUnit ocamlfind ocamlformat

RUN opam install -y \
	satysfi-bibyfi satysfi-easytable satysfi-enumitem satysfi-base satysfi-uline satysfi-class-slydifi \
	satysfi-fonts-noto-sans satysfi-fonts-noto-sans-cjk-jp && \
	eval $(opam env) && \
	satyrographos install

RUN echo "eval $(opam env)" >> ~/.bashrc

WORKDIR "/home/coins"
ENTRYPOINT [ "/bin/bash" ]
