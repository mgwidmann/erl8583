ERL8583_BUILD=erl8583-0.3.5
mkdir $ERL8583_BUILD
mkdir $ERL8583_BUILD/doc
mkdir $ERL8583_BUILD/ebin
mkdir $ERL8583_BUILD/tbin
echo "Compiling source"
erlc -I include -o $ERL8583_BUILD/ebin src/*.erl
cp src/erl8583.app $ERL8583_BUILD/ebin
echo "Compiling tests"
erlc -I include -o $ERL8583_BUILD/tbin test/*.erl
erl -noshell -pa $ERL8583_BUILD/ebin -pa $ERL8583_BUILD/tbin -s test_all test $ERL8583_BUILD/tbin -s init stop
rm -r $ERL8583_BUILD/tbin
cp overview.edoc $ERL8583_BUILD/doc
./gen_doc.sh $ERL8583_BUILD/doc
echo "Creating archive"
rsync -p -r --exclude=".*" src $ERL8583_BUILD
rsync -p -r --exclude=".*" test $ERL8583_BUILD
rsync -p -r --exclude=".*" include $ERL8583_BUILD
rsync -p -r --exclude=".*" src_examples $ERL8583_BUILD
cp make.sh $ERL8583_BUILD
cp gen_doc.sh $ERL8583_BUILD
cp overview.edoc $ERL8583_BUILD
if [ -f $ERL8583_BUILD.zip ]; then
    rm $ERL8583_BUILD.zip
fi
zip -q $ERL8583_BUILD.zip -r $ERL8583_BUILD
rm -r $ERL8583_BUILD

