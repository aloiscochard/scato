Scato
=====

[![Join the chat at https://gitter.im/aloiscochard/scato](https://badges.gitter.im/aloiscochard/scato.svg)](https://gitter.im/aloiscochard/scato?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

In it's current form, it is a typeclass system encoding for Scala that have the particularity of using natural transformation instead of subtyping to encode the hierarchy of the classes.

It is accompanied with a basic typeclass hierarchy and some examples to show the added benefits over conventional and currently used encodings. 

## Modules

- `typeclass`
	- the typeclass system along with the primitive structures required for the encoding (natural transformations, equality, ...).
- `base`
	- the base library containing core types and classes along with instances for the standard library.
- `prelude`
	- the default prelude which combine the syntax of the core types
- `profunctors`
	- *Enterprise Grade Functors*
- `transformers` 
	- *Composable Monadic Vehicles* 

## Motivation

Working with *scalaz7* can be sometimes difficult, it can be even more embarrassing when trying to explain the issues to a newcomers getting started with the library.

Those problems are mainly related to the use of subtyping in the library to encode the typeclass hierarchy.

Here is a concrete example which is given from a *scalaz7* user point of view:
https://github.com/aloiscochard/scato/blob/master/examples/src/main/scala/ConflictingSyntax.scala

You can also look at this comparison with a traditional encoding:
https://github.com/aloiscochard/scato/blob/master/examples/src/main/scala/ConflictingInstances.scala 

## Related projects
* https://github.com/scalaz/scalaz
* https://hackage.haskell.org/package/transformers
* https://hackage.haskell.org/package/profunctors
* https://www.haskell.org
