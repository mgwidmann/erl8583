rm ebin/*.beam
erlc -I include -o ebin src/*.erl
erl -noshell -pa ebin -s test_all test -s init stop
./gen_doc.sh
zip dist/erl8583_0.1.0.zip src/* include/* doc/* ebin/* make.sh

