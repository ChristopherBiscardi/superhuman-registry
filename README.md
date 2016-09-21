# The Superhuman Registry

The Superhuman Registry is a
[Docker Registry HTTP API V2][registry-v2] compatible registry
implemented in Haskell with [Servant][servant].

You may be interested in the
[in-progress](https://github.com/ChristopherBiscardi/christopherbiscardi.github.com/blob/leo/data/building-a-docker-registry/index.post)
blog post about this work.

## Logging

Logging is implemented via [Katip][katip]

## Deployment

TODO: Docker Image

# Dumping the Routes

```shell
stack build
stack exec sr-layout-exe
```

Will will dump the route structure similar to the following.

```
/
└─ v2/
   ├─•
   ┆
   ┆
   ├─ <capture>/
   │  ├─ blobs/
   │  │  ├─ <capture>/
   │  │  │  ├─•
   │  │  │  ┆
   │  │  │  ├─•
   │  │  │  ┆
   │  │  │  └─•
   │  │  ┆
   │  │  └─ uploads/
   │  │     ├─•
   │  │     ┆
   │  │     ┆
   │  │     └─ <capture>/
   │  │        ├─•
   │  │        ┆
   │  │        ├─•
   │  │        ┆
   │  │        ├─•
   │  │        ┆
   │  │        └─•
   │  ├─ manifests/
   │  │  └─ <capture>/
   │  │     ├─•
   │  │     ┆
   │  │     ├─•
   │  │     ┆
   │  │     ├─•
   │  │     ┆
   │  │     └─•
   │  └─ tags/
   │     └─ list/
   │        └─•
   ┆
   └─ _catalog/
      └─•
```
[servant]: http://haskell-servant.readthedocs.io/en/stable/
[registry-v2]: https://github.com/docker/distribution/blob/bfa0a9c0973b5026d2e942dec29115c120e7f731/docs/spec/api.md
[katip]: https://hackage.haskell.org/package/katip
