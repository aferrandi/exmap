EXDIR=/opt/executable

docker run -v ~/prj/exmap/server/systemdirtest:$EXDIR/prjroot  -p 3000:3000 aferrandi/exmap $EXDIR/exmap 3000 $EXDIR/webclient $EXDIR/prjroot
