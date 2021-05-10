# You'll need leiningen (http://leiningen.org/) above v.2.3.2 to build this project.
# allow snapshots in release (some versions can use snapshots)
LEIN_SNAPSHOTS_IN_RELEASE=true export LEIN_SNAPSHOTS_IN_RELEASE

# clear bins
rm -R ../target/*
rm -R ../resources/public/js/out/*
rm ../resources/public/css/prod.css
rm ../resources/public/js/prod.js

# minimize css
java -jar closure-stylesheets.jar  --allowed-unrecognized-property -moz-osx-font-smoothing --allowed-unrecognized-property user-select --allowed-non-standard-function progid:DXImageTransform.Microsoft.Alpha --allowed-non-standard-function progid:DXImageTransform.Microsoft.BasicImage --allowed-non-standard-function radial-gradient  --output-file ../resources/public/css/prod.css ../resources/public/css/base.css ../resources/public/css/layout.css ../resources/public/css/skeleton.css ../resources/public/css/font-awesome.css ../resources/public/css/promo.css ../resources/public/css/helpdesk.css

# compile client-side js
lein cljsbuild once prod

# compile server-side jar
lein uberjar

