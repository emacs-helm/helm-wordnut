# helm-wordnut

This package is merely a combination of two packages already available:

- [helm-wordnet](https://github.com/raghavgautam/helm-wordnet)
- [wordnut](https://github.com/gromnitsky/wordnut)

Both packages seem hardly maintained these days, so I took what I deemed useful
and created one package that combines the searchability of `helm-wordnet` with the
nice-looking output of `wordnut`.

## Getting started

First, install WordNet following the instructions [here](https://wordnet.princeton.edu/download). For example, on
Ubuntu/Debian systems it is simply a matter of:

``` bash
$ sudo apt-get install wordnet
```

`helm-wordnut` is not listed on ELPA nor MELPA. If you want to try it, you can use
[`straight.el`](https://github.com/raxod502/straight.el) to install it:

```elisp
(straight-use-package
  '(helm-wordnut :host github :repo "emacs-helm/helm-wordnut"))
```

Before using <kbd>M-x helm-wordnut</kbd>, ensure `helm-wordnut-wordnet-location` and
`helm-wordnut-prog` are set according to your WordNet installation.

## Acknowledgements

`helm-wordnut` would not have been possible without the work of others in the
Emacs community. So it’s only fair to say “thank you!” to:

- [Thierry Volpiatto](https://github.com/thierryvolpiatto), for [Helm](https://github.com/emacs-helm)
- [Raghav Kumar Gautam](https://github.com/raghavgautam), for [helm-wordnet](https://github.com/raghavgautam/helm-wordnet)
- [Alexander Gromnitsky](https://github.com/gromnitsky), for [wordnut](https://github.com/gromnitsky/wordnut)
