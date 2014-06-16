icemorph
========

Code for Icemorph


== HMM + RESTRICTED VITERBI

To run the HMM + restricted Viterbi Perl script go to the ./bin folder and execute:

perl -I . hmm-rv.pl

The "-I ." instructs Perl to use our modified version of the Algorithm/Viterbi.pm Perl module. You can specify the test_type like this:

perl -I . hmm-rv.pl -test_type (all|first20|last20)

where "all" is the default option (i.e. in-sample testing).


== CRF

To create training and test files for CRF++:

perl create_crf_data_files.pl

The CRF++ exec.sh file contains the following lines:

../../crf_learn -e 0.01 -f 5 -a CRF-L1 -p 7 template_small train-1.data model2
../../crf_test -m model2 test-1.data
