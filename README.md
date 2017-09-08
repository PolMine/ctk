Welcome to the corpus toolkit (ctk)
===================================

This package offers classes and wrappers for preparing larger, linguistically
and structurally annotated corpora. Preparing corpora to be imported into the
Corpus Workbench (CWB) as a system for corpus management is particularly
supported, but the usefulness of the package is intended to go well beyond that.

First of all, the package offers the 'pipeDir' class to manage data during 
corpus preparation in a succinct and structured manner. The class is used to define a 
directory with relevant subdirectory for the various tasks corpus preparation 
may involve (such as tokenisation etc.). A dirApply method supplements the class
to assist piping many files in parallel in a verbose and flexible manner. The 
aim is to assist the preparation of large-scale corpora, i.e. processes that are
time-consuming and error-prone.

Second, the package includes a set of wrappers to make established tools for
corpus preparation accessible from R, such as the treetagger, Stanford CoreNLP,
or NLTK. Of course, there are specialized packages for that already (such as
koRpus, or coreNLP); the wrappers included here are developed such that data is
processed on file-by-file, or by-line manner to make the process robust against
errors, and to avoid consuming RAM excessively. Parallization is supported wherever
possible.

Third, a set of convenience tools complement the core of the ctk package, to
assist handling re-occurring problems such as murky encodings etc.

An important aim of the package is to offer a standardized workflow for corpus
preparation, thus making corpus preparation more comfortable when data grows
larger, and to contribute to the ideal of reproducible research at the stage of
corpus preparation.