mkdir tbin
rm ebin/*.beam
rm tbin/*.beam
rm doc/*
erlc -I include -o ebin src/*.erl
erlc -I include -o tbin test/*.erl
erl -noshell -pa ebin -pa tbin -s test_all test -s init stop
./gen_doc.sh
zip dist/erl8583_0.1.0.zip src/* include/* doc/* ebin/* make.sh

