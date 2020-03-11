# Catalogizator
---
#### To run Catalogizator app use `cabal install --force-reinstalls` and `cabal run` in project directory
---
### Catalogizator can:

1. **Force rescan** working directory for further actions (for example, if you add file to directory while Catalogizator was working) 
2. **Print tree for working directory** (output list of files and directories in descriptive view)
3. **Print tree for given directory** (like 2, but for given directory)
4. **Find doubles by name** (find files with identical names, but the content can be different)
5. **FInd doubles by content** (find files with identical content, but names can be different)
6. **Stop Catalogizator** (delete all created files and exit the program)
---

# Explanation of symbols

    └──┬ dist-newstyle
       ├──┬ packagedb
       │  └──┬ ghc-8.8.1
       │     ├─── package.cache
       │     ├──< tmp
       │     └─── package.cache.lock
       
## In this example we can see:
- `└──┬ dist-newstyle` - directory with content (last in directory, where ***dist-newstyle*** located)
- `├──┬ packagedb` - directory with content (not last in directory, where ***dist-newstyle*** located)
- `├─── package.cache` - file in directory ***ghc-8.8.1*** (not last)
- `└─── package.cache.lock` - last file in directory ***ghc-8.8.1***
- `├──< tmp` - em
