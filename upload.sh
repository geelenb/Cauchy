sed s/index.js/index.min.js/g -i sed_backup index.html
terser index.js -o index.min.js
rsync -vr * esat:public_html/cauchy/
rm index.min.js
sed s/index.min.js/index.js/g -i sed_backup index.html
