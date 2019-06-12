## The `grab` package

A _grab_:

  1. Consumes some portion (none, part, or all) of its input _bag_;

  2. Returns a _Result_:

      * A _residue_ consisting of the unconsumed input;

      * Some monoidal _log_ e.g. a list of error messages;

      * Some _desideratum_ (the object of desire) produced from
        the consumed input, or _Nothing_ if the grab failed.

```haskell
newtype Grab bag residue log desideratum =
  Grab
    (bag -> (residue, log, Maybe desideratum))
```

Read more in the [`Control.Grab`](grab/src/Control/Grab.hs) module.

## The `grab-form` package

Utilizes the `grab` package to consume typical HTTP form submission data.

Read more in the [tutorial](grab-form/test/Test/Tutorial.hs) test module.
