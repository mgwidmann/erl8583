mkdir erl8583
mkdir erl8583/doc
mkdir erl8583/ebin
mkdir erl8583/tbin
echo "Compiling source"
erlc -I include -o erl8583/ebin src/*.erl
echo "Compiling tests"
erlc -I include -o erl8583/tbin test/*.erl
erl -noshell -pa erl8583/ebin -pa erl8583/tbin -s test_all test erl8583/tbin -s init stop
rm -r erl8583/tbin
cp overview.edoc erl8583/doc
./gen_doc.sh
echo "Creating archive"
rsync -p -r --exclude=".*" src erl8583
rsync -p -r --exclude=".*" test erl8583
rsync -p -r --exclude=".*" include erl8583
cp make.sh erl8583
cp gen_doc.sh erl8583
cp overview.edoc erl8583
rm erl8583_0.1.0.zip
zip -q erl8583_0.1.0.zip -r erl8583
rm -r erl8583

