TAG=$1
IMGID=$2
docker login -u aferrandi
docker tag $IMGID aferrandi/exmap:$TAG
docker push aferrandi/exmap