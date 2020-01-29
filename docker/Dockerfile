FROM ocaml/opam2:4.07

RUN sudo apt-get update
RUN sudo apt-get install -y libgmp-dev perl pkg-config m4 zlib1g-dev

RUN git clone https://gitlab.com/dailambda/scaml.git

WORKDIR scaml
RUN git checkout pyramid
RUN opam update
RUN opam install -y vendors/*/*.opam src/scaml.opam
RUN eval $(opam env) && which scamlc

WORKDIR /scaml-work
