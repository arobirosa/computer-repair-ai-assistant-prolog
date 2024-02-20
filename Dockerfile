FROM swipl:stable
COPY . .
ENTRYPOINT ["swipl", "start.pl"]
EXPOSE 8080
LABEL org.opencontainers.image.source=https://github.com/arobirosa/computer-repair-ai-assistant-prolog
LABEL org.opencontainers.image.description="AI Computer Assistant. Proof of concept to evaluate the use of knowledge representation and reasoning (KRR) systems "
LABEL org.opencontainers.image.licenses=Apache-2.0
