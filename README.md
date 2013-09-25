inflate.hs
==========

An implementation of Inflate (and gunzip) in Haskell, mainly for the
purpose of understanding Haskell and Deflate

Files
-----

One major bottleneck is the backbuffer.

The file `inflate.recSlowdown.hs` gives a purely functional
implementation, using a data structure for the backbuffer that employs
recursive slowdown.

The file `inflate.runST.hs` uses runST to resolve backreferences.

Still, both are too slow. Please comment or send me E-Mails with
suggestions on how to make the code faster and more readable.

Links
-----

 - [RFC 1951 - Deflate](http://tools.ietf.org/html/rfc1951)
 - [RFC 1952 - GZip](http://tools.ietf.org/html/rfc1952)
 - [My academic website](http://www2.tcs.ifi.lmu.de/~senjak/)
 - [My private website](https://uxul.de/)