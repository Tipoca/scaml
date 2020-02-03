# Build

```
./build.sh <commit>
```

should build an image `dailambda/scaml:<commit>`

# Basic use

```
$ docker run dailambda/scaml:<commit> scamlc <args>
```

For example,

```
$ ls xxx.ml
xxx.ml
$ docker run -v `pwd`:/root dailambda/scaml:1.0.3 scamlc xxx.ml
```
