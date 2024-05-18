#build purescript code with spago
@spagobuild :
    spago build

# bundle with esbuild
@bundle : spagobuild
    esbuild \
    ./output/Main/index.js \
    --bundle \
    --outfile=public/index.js \
    --platform=browser \
    --format=esm \
    --external:@emurgo/cardano-serialization-lib-browser

# build 
@build : bundle
     npx vite build

# serve 
@serve PORT : build
     http-server ./dist -p {{PORT}}