from fpco/stack-build-small:latest

WORKDIR /app

COPY . /app

RUN echo "echo \"Use stack run -- -[e|d] -c [..] to run the binary.\nstack test --coverage runs tests.\nDon't forget to create input files in /app with echo input > in.txt, etc.\"" >> ~/.bashrc

RUN stack build

RUN stack test

CMD ["/bin/bash"]
