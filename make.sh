mkdir tbin
rm ebin/*.beam
rm tbin/*.beam
rm doc/*.html
echo "Compiling source"
erlc -I include -o ebin src/*.erl
echo "Compiling tests"
erlc -I include -o tbin test/*.erl
erl -noshell -pa ebin -pa tbin -s test_all test -s init stop
./gen_doc.sh
echo "Creating archive"
mkdir dist
rm dist/*.zip
zip -q dist/erl8583_0.1.0.zip src/* test/* include/* doc/* ebin/* make.sh gen_doc.sh

