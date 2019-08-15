# hcc

## Example of effective flow

- (on host):
  - `$ stack build`
  - `$ docker build -t hcc .`
  - `$ stack exec hcc-exe "(3 + 4) - 3 * 5 / 4" > tmp.s`
- (on container):
  - `# gcc -g main.c tmp.s -o tmp`
  - `# ./tmp`
  - `# echo $?`

## Build

- `$ stack build`

## Compile Example

- `$ stack exec hcc-exe "(3 + 4) - 3 * 5 / 4" > tmp.s`

## Docker

### Build

- `$ docker build -t hcc .`

### Run

- `$ docker run -v "$PWD"/.:/home --rm -ti hcc`

## Assemble and Run

- `# gcc -g main.c tmp.s -o tmp`

## make Asm file

- `# ./tmp`

## Test

- `$ stack test`

## TODO

- [ ] lexer error
