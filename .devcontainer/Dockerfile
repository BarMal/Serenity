FROM mcr.microsoft.com/devcontainers/java:1-21-bookworm

ENV COURSIER_BIN_DIR=/usr/local/bin

RUN rm -f /etc/apt/sources.list.d/yarn.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends curl gzip zip \
    && curl -fsSL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz -o /tmp/cs.gz \
    && gunzip /tmp/cs.gz \
    && chmod +x /tmp/cs \
    && /tmp/cs install sbt --install-dir "${COURSIER_BIN_DIR}" \
    && rm -f /tmp/cs \
    && rm -rf /var/lib/apt/lists/*
