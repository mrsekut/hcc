# hcc

- C Compiler implementation in Haskell.

## Features

- [x] Four arithmetic operations
- [x] local variable
- [x] if statement
- [x] while statement
- [x] for statement
- [ ] function

## Example of effective flow

- (on host):
  - `$ stack build`
  - `$ docker build -t hcc .`
  - `$ stack exec hcc-exe "(3 + 4) - 3 * 5 / 4" > tmp.s`
- (on container):
  - `# gcc -g main.c tmp.s -o tmp`
  - `# ./tmp`
  - `# echo $?`

## Docker

- build: `$ docker build -t hcc .`
- run: `$ docker run -v "$PWD"/.:/home --rm -ti hcc`

## Assemble and Run

- `# gcc -g main.c tmp.s -o tmp`
- `# ./tmp`
