# math

A simple math language.

Exploring the ways in which we need to adapt our `lalrpop` grammar to still support precedence or associativity even when passing everything through a `Located<T>` generic to keep track of locations in the source code, so that we may provide nice error messages.
