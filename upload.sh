sed -i .sed_backup s/index.js/index.min.js/g index.html
terser index.js -c passes=2 --toplevel -m reserved=\[abs,arg,coefff,Polynomial\] -o index.min.js
rsync -r \
  --exclude=upload.sh \
  --exclude=index.sh \
  --exclude=node_modules \
  --exclude-from=.gitignore \
  * esat:public_html/cauchy
rm index.min.js
sed -i .2.sed_backup s/index.min.js/index.js/g index.html

