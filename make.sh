rm ebin/*.beam
erlc -I include -o ebin src/*.erl
erl -noshell -pa ebin -s test_all test -s init stop
./gen_doc.sh

