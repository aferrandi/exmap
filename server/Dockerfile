# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker

FROM fpco/stack-build:lts-13.27 as build
RUN mkdir /opt/build
WORKDIR /opt/build

COPY . /opt/build
RUN stack build --system-ghc
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# Base image for stack build so compiled artifact from previous
# stage should run
FROM alpine
RUN mkdir -p /opt/executable
WORKDIR /opt/executable

COPY --from=build /opt/build/bin .
COPY ./webclient /opt/executable/webclient
EXPOSE 3000
CMD ["/opt/executable/exmap", "3000"]


VOLUME /opt/executable/prjroot