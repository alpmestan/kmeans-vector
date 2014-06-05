# k-means clustering algorithm in Haskell

kmeans-vector is a Haskell library for performing the k-means clustering algorithm. It is based on the very efficient 'vector' library.

[![Build Status](https://secure.travis-ci.org/alpmestan/kmeans-vector.png?branch=master)](http://travis-ci.org/alpmestan/kmeans-vector)

You can find examples in the `examples/` directory.

Feel free to contribute, may it be features, performance improvements, etc.

Performances (0.3 version)
--------------------------

The library has completely been rewritten and beats the 0.2 or the `kmeans-par` package
hands down. I also plan to work on an implementation of the *Streaming k-means* technique soon, but that'll be for the next version.

Note that this version also features some improvements in the usability.

Performances (0.2 version)
--------------------------

In addition to contributing a feature, Ville Tirronen also claimed to make the library about 50% faster, which was confirmed after running the algorithm on a dataset I had lying around.

Performances (0.1 version)
--------------------------

*kmeans-vector* is much faster than the existing [kmeans](http://hackage.haskell.org/package/kmeans) package. For example, the kmeans package performs k-means on 10000 2D points with k=5 in 21.099s while kmeans-vector does the same in 6.605s. For 50000 3D points, with k=5 still, kmeans-vector performs in 47.853s while I Ctrl+C'd the kmeans version after 6 minutes and a half.

Authors
-------

This library is written and maintained by Alp Mestanogullari,
<alpmestan@gmail.com>. Ville Tirronen contributed code to the 0.2 version.