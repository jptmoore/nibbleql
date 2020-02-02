FROM ocaml/opam2:alpine

RUN sudo apk add m4

RUN opam update \
&& opam install -y reason yojson linenoise cohttp-lwt cohttp-lwt-unix menhir \
&& opam depext -i tls ssl

ADD src src
RUN sudo chown -R opam:nogroup src
RUN cd src && opam config exec -- dune build --profile release ./main.exe

FROM alpine

RUN adduser nibble --disabled-password

WORKDIR /home/nibble
COPY --from=0 /home/opam/opam-repository/src/_build/default/main.exe ./nibbleql

RUN apk update && apk add gmp libressl zlib openssl

USER nibble

ENTRYPOINT ["/home/nibble/nibbleql"]



