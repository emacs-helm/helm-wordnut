# helm-wordnut

This package is merely a combination of two packages already available:

- [helm-wordnet](https://github.com/raghavgautam/helm-wordnet)
- [wordnut](https://github.com/gromnitsky/wordnut)

I only took what I deemed useful from both packages and adapted it to my needs,
which means I am using it on Linux and with an Emacs version built from `master`.

## Getting started

First, install WordNet following the instructions [here](https://wordnet.princeton.edu/download).

Since I am not providing support for it, `helm-wordnut` is not listed on MELPA. If
you want to try it, you can use [`straight.el`](https://github.com/raxod502/straight.el) to install it:

```elisp
(straight-use-package
  '(helm-wordnut :host github :repo "manuel-uberti/helm-wordnut"))
```

Before using <kbd>M-x helm-wordnut</kbd>, ensure `helm-wordnut-wordnet-location` and
`helm-wordnut-prog` are set as you expect.

## Acknowledgements

`helm-wordnut` would not have been possible without the work of others in the
Emacs community. So it’s only fair to say “thank you!” to:

- [Thierry Volpiatto](https://github.com/thierryvolpiatto), for [Helm](https://github.com/emacs-helm)
- [Raghav Kumar Gautam](https://github.com/raghavgautam), for [helm-wordnet](https://github.com/raghavgautam/helm-wordnet)
- [Alexander Gromnitsky](https://github.com/gromnitsky), for [wordnut](https://github.com/gromnitsky/wordnut)
