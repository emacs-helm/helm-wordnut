# helm-wordnut

This package is merely a combination of two packages already available:

- [helm-wordnet](https://github.com/raghavgautam/helm-wordnet)
- [wordnut](https://github.com/gromnitsky/wordnut)

I only took what I deemed useful from both packages and adapted it to my needs.

## Getting started

First, install WordNet following the instructions [here](https://wordnet.princeton.edu/download).

`helm-wordnut` is not listed on MELPA, but you can use [`straight.el`](https://github.com/raxod502/straight.el) to install it:

```elisp
(straight-use-package
  '(helm-wordnut :host github :repo "manuel-uberti/helm-wordnut"))
```

Once available in your Emacs, you can use <kbd>M-x helm-wordnut</kbd>.
