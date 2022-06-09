# haskell-library-template

Haskell library template used at Freckle.

## Create your repo

```sh
gh repo create --template freckle/haskell-library-template
```

## Rename your package

```sh
sed -i s/haskell-library-template/my-name/ **/*
```

## Enable release

When you are ready to release your library, simply remove the conditional from
the release workflow.

```diff
--      - if: false # Remove when ready to release
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
