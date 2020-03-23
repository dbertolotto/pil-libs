tests=`find . | grep "test-.*\.l$"`
currdir=`pwd`
for path in $tests; do
  dir=`dirname $path`
  file=`basename $path`
  echo running $path ...
  cd $dir
  pil $file -tests -bye
  cd $currdir
done
