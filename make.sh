mkdir tbin
rm ebin/*.beam
rm tbin/*.beam
echo "Compiling source"
erlc -I include -o ebin src/*.erl
echo "Compiling tests"
erlc -I include -o tbin test/*.erl
erl -noshell -pa ebin -pa tbin -s test_all test -s init stop
rm -r erl8583
mkdir erl8583
mkdir erl8583/doc
cp overview.edoc erl8583/doc
./gen_doc.sh
echo "Creating archive"
rsync -p -r --exclude=".*" src erl8583
rsync -p -r --exclude=".*" test erl8583
rsync -p -r --exclude=".*" gen_doc.sh erl8583
rsync -p -r --exclude=".*" include erl8583
rsync -p -r --exclude=".*" ebin erl8583
cp make.sh erl8583
cp gen_doc.sh erl8583
cp overview.edoc erl8583
zip -q erl8583_0.1.0.zip -r erl8583

