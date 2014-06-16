icemorph
========

Code for Icemorph


== 2014-06-16

To run the HMM + restricted Viterbi Perl script go to the ./bin folder and execute:

perl -I . hmm-rv.pl

The "-I ." instructs Perl to use our modified version of the Algorithm/Viterbi.pm Perl module. You can specify the test_type like this:

perl -I . hmm-rv.pl -test_type (all|first20|last20)

where "all" is the default option (i.e. in-sample testing).
