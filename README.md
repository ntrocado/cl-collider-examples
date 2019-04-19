# Cl-collider examples
Supercollider code from various instructive sources translated to Common Lisp using the [cl-collider](https://github.com/byulparan/cl-collider) library.

## The SuperCollider Book
Example code in [The SuperCollider Book](http://supercolliderbook.net/).

Examples where the focus is sclang's features are normally not included.

Status: It's obviously a very large undertaking to include all the code in the book. I've been working on the bits that interest me most, and that are not specific to SuperCollider itself. See [TODO](file:the-supercollider-book/todo.org) for an overview of what's included and what's still missing.

## SuperCollider tutorial by Nick Collins
Example code in [SuperCollider tutorial](https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html).

Many examples involve setting up a graphical user interface. This is accomplished in Common Lisp through CommonQt/Qtools. I use some personalized components, compiled in [this library](https://github.com/ntrocado/qtools-elements). Clone it and, if needed, load with `(ql:quickload :qtools-elements)`. See the sources for more instructions.