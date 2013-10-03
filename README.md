inflate.hs
==========

An implementation of Inflate (and gunzip) in Haskell, mainly for the
purpose of understanding Haskell and Deflate. For actual production
use, consider Deflate.Compression.Inflate.

Files
-----

One major bottleneck is the backbuffer.

The file `inflate.recSlowdown.hs` gives a purely functional
implementation, using a data structure for the backbuffer that employs
recursive slowdown.

The file `inflate.runST.hs` uses runST to resolve backreferences, but
is not purely functional anymore then.

Still, both are too slow. Please comment or send me E-Mails with
suggestions on how to make the code faster and more readable.

Links
-----

 - [Data.Compression.Inflate](http://hackage.haskell.org/package/MissingH-1.2.0.2/docs/src/Data-Compression-Inflate.html)
 - [RFC 1951 - Deflate](http://tools.ietf.org/html/rfc1951)
 - [RFC 1952 - GZip](http://tools.ietf.org/html/rfc1952)
 - [My academic website](http://www2.tcs.ifi.lmu.de/~senjak/)
 - [My private website](https://uxul.de/)