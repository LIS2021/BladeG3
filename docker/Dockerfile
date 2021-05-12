### start from current ubuntu image
FROM ubuntu:groovy
MAINTAINER lorev
### install base programming tools
RUN apt update && apt install -y build-essential ocaml opam clang bc
### install opam llvm dependencies
RUN apt -y install cmake python2 llvm 
### fix user
RUN useradd -ms /bin/bash bladeg3user
USER bladeg3user
WORKDIR /home/bladeg3user
### install opam packages
RUN opam init --disable-sandboxing
RUN eval $(opam env)
RUN opam install -y ocamlbuild llvm
RUN echo "\neval $(opam env)" >> .bashrc

