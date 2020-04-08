# helm-wordnut

This package is merely a combination of two packages already available:

- [helm-wordnet](https://github.com/raghavgautam/helm-wordnet)
- [wordnut](https://github.com/gromnitsky/wordnut)

I only took what I deemed useful from both packages and adapted it to my needs,
which means I am using it on Linux and with an Emacs version built from the
`master` branch.

## Getting started

First, install WordNet following the instructions [here](https://wordnet.princeton.edu/download).

Since I am not providing support for it, `helm-wordnut` is not listed on MELPA. If
you want to try it, you can use [`straight.el`](https://github.com/raxod502/straight.el) to install it:

```elisp
(straight-use-package
  '(helm-wordnut :host github :repo "manuel-uberti/helm-wordnut"))
```

Once available in your Emacs, you can use <kbd>M-x helm-wordnut</kbd>.

## Acknowledgements

`helm-wordnut` would not have been possible without the work of others in the
Emacs community. So it’s only fair to say “thank you!” to:

- [Thierry Volpiatto](https://github.com/thierryvolpiatto), for [Helm](https://github.com/emacs-helm)
- [Raghav Kumar Gautam](https://github.com/raghavgautam/helm-wordnet), for [helm-wordnet](https://github.com/raghavgautam/helm-wordnet)
- [Alexander Gromnitsky](https://github.com/gromnitsky/wordnut), for [wordnut](https://github.com/gromnitsky/wordnut)
