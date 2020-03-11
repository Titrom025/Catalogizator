# <h1 align=center> Catalogizator</h1>
--- 
#### To build and run Catalogizator app with cabal use `cabal install --force-reinstalls` and `cabal run` in project directory  
---  
#### To run executable file Catalogizator app just put it into directory, which you would like to scan, and start it. 
---  
## <h2 align=center>Catalogizator can:</h2>

1. **Force rescan** working directory for further actions (for example, if you add file to directory while Catalogizator was working) 
2. **Print tree for working directory** (output list of files and directories in descriptive view)
3. **Print tree for given directory** (like 2, but for given directory)
4. **Find doubles by name** (find files with identical names, but the content can be different)
5. **FInd doubles by content** (find files with identical content, but names can be different)
6. **Stop Catalogizator** (delete all created files and exit the program)
---

## <h2 align=center> Explanation of symbols </h2>

    └──┬ dist-newstyle
       ├──┬ packagedb
       │  └──┬ ghc-8.8.1
       │     ├─── package.cache
       │     ├──< tmp
       │     └─── package.cache.lock
       
## <h2 align=center>In this example: </h2>
- `└──┬ dist-newstyle` - directory with content (last in directory, where ***dist-newstyle*** located)
- `├──┬ packagedb` - directory with content (not last in directory, where ***dist-newstyle*** located)
- `├─── package.cache` - file in directory ***ghc-8.8.1*** (not last)
- `└─── package.cache.lock` - last file in directory ***ghc-8.8.1***
- `├──< tmp` - em
