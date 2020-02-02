FROM ubuntu:18.04
MAINTAINER Jun FURUSE <jun.furuse@dailambda.jp>

RUN apt-get clean

# Replacing the source

RUN sed -e 's|/archive\.ubuntu\.com/ubuntu|/ftp.jaist.ac.jp/pub/Linux/ubuntu|g' /etc/apt/sources.list > /tmp/sources.list
RUN mv /tmp/sources.list /etc/apt/sources.list
RUN apt-get update

RUN apt-get install -y --no-install-recommends curl git ca-certificates make patch sudo netbase jq

# OCaml and OPAM

RUN apt install -y --no-install-recommends pkg-config libgmp-dev libev-dev libhidapi-dev m4 unzip bubblewrap ocaml g++
USER root
WORKDIR /
RUN git clone https://github.com/ocaml/opam
WORKDIR opam
RUN git checkout 2.0.3
RUN ./configure
RUN make lib-ext && make && make install
RUN opam init -a --bare --disable-sandboxing # bwrap does not work...

# SCaml
# 
WORKDIR /
RUN git clone https://gitlab.com/dailambda/scaml.git
WORKDIR /scaml
RUN git checkout ${scaml-commit:-master}
RUN opam update
RUN opam switch create . ocaml-base-compiler.4.07.1
RUN opam update

# required for install local opams
RUN apt install -y --no-install-recommends rsync

RUN eval $(opam env) && env && opam install -y vendors/*/*.opam src/scaml.opam
RUN eval $(opam env) && which scamlc

RUN eval $(opam env) && scamlc src/tests/app_vote.ml && echo OK

# Cleanup
WORKDIR /
RUN du -sh /usr /scaml /root \
    && apt remove -y ocaml g++ \
    && apt autoremove -y \
    && apt-get clean \
    && rm -rf /opam \
    && (cd /scaml; opam clean) \
    && du -sh /usr /scaml /root

WORKDIR /scaml
RUN eval $(opam env) && scamlc src/tests/app_vote.ml && echo OK

RUN opam clean
RUN rm -rf /scaml/_opam/.opam-switch
RUN du -sh /usr /scaml /root

# 
# RUN du -sh /usr /home /tezos /root \
#     && rm -rf /tezos/_build \
#     && (tar zcf /tmp/tezos.tgz /tezos/_opam/lib/stublibs /tezos/_opam/lib/ocaml/stublibs /tezos/tezos-client /tezos/tezos-node /tezos/scripts /tezos/active_protocol_versions /tezos/.template) \
#     && rm -rf /tezos \
#     && (cd /; tar zxvf /tmp/tezos.tgz) \
#     && rm -rf /.opam \
#     && rm -rf /.git \
#     && rm -rf /_opam/.opam-switch \
#     && rm -rf /_opam/.npm \
#     && rm -rf /_opam/.ocp \
#     && du -sh /usr /home /tezos /root

# That's all
