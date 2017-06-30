# This has been tested for hoogle version 5

Assuming we want to host the hoogle at http://reflex-frp.org/hoogle
and haddocks at http://reflex-frp.org/haddock/

On the machine where all the docs are present do the following steps

```
mkdir static
cd static
```

1. Execute the `getHaddockDocs` script for every package required in
   hoogle and haddock. (Edit this script to specify the proper URL of haddock)
   
   This script will copy all the "html" directories of all the packages to the static
   folder, and modify the `package.txt` file to include the URL of haddock.
   
```
  ./getHaddockDocs reflex
```

2. Create hoogle database from this

```
   cd ..;
   hoogle generate --local=./static  --database=hoogledb.hoo
```

   This will take a while and create a file `hoogledb.hoo`

Copy the `static` directory and `hoogledb.hoo` on the server
and start the hoogle server using this db.

   `hoogle server -p 8080 --database=hoogledb.hoo`

and the file hosting server to serve the `static` folder.

Done!
