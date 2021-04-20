
rm -f ./*.log

./eschecs \
/home/roland/Documents/echecs/sources/ct800/142/source/application-uci/output/CT800_V1.42 \
--position='rnbqk1nr/1ppp1ppp/p3p3/8/1bB1P3/2N5/PPPP1PPP/R1BQK1NR w HAha - 2 4' \
--autoplay=true \
--upsidedown=false \
--chessboard=simple \
--time=500 \
--font=montreal \
--language=french \
--size=60 \
--white=FFFF00FF \
--black=FFA500FF \
--green=60C00080 \
--red=C0000080 \
--volume=10 \
2>> error.log

exit 0

./eschecs \
/home/roland/Documents/echecs/sources/ct800/142/source/application-uci/output/CT800_V1.42 \
--position='rnbqk1nr/1ppp1ppp/p3p3/8/1bB1P3/2N5/PPPP1PPP/R1BQK1NR w HAha - 2 4' \
--autoplay=true \
--upsidedown=false \
--chessboard=marblecustom \
--marblecolors=FFFFFFFF,0080B3FF,0066FFFF,0047B3FF \
--time=500 \
--font=montreal \
--language=french \
--size=60 \
--green=60C00080 \
--red=C0000080 \
--volume=10 \
2>> error.log

exit 0

./eschecs \
/home/roland/Documents/echecs/sources/ct800/142/source/application-uci/output/CT800_V1.42 \
-p 'rnbqk1nr/1ppp1ppp/p3p3/8/1bB1P3/2N5/PPPP1PPP/R1BQK1NR w HAha - 2 4' \
-a true \
-u false \
-c marblecustom \
-m FFFFFFFF,0080B3FF,0066FFFF,0047B3FF \
-t 1001 \
-f montreal \
-l french \
-s 60 \
-g 60C00080 \
-r C0000080 \
-v 10 \
2>> error.log

exit 0

./eschecs \
/home/roland/Documents/echecs/sources/ct800/142/source/application-uci/output/CT800_V1.42 \
-p 'rnbqk1nr/1ppp1ppp/p3p3/8/1bB1P3/2N5/PPPP1PPP/R1BQK1NR w HAha - 2 4' \
-a true \
-u false \
-c marble \
-t 1001 \
-f montreal \
-l french \
-s 60 \
-g 60C00080 \
-r C0000080 \
-v 10 \
2>> error.log

#2>> error.log | tee eschecs.log
