[![Haskell CI](https://github.com/Miezhiko/Haku/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/Haku/actions/workflows/haskell.yml)

# Haku
Portage helper, not replacing portage itself but partially replacing `eix`, `gentoolkit` and more maybe.

<img src="https://cdn.discordapp.com/attachments/740144639050383412/1039095540064981023/Screenshot_from_2022-11-07_12-34-02.png">

# Usage

you should have `PORTDIR` and optionally `PORTDIR_OVERLAY` defined in `make.conf` for this to work.

```bash
Haku v0.0.19

get         Merge one or more variants.             
delete      Delete one or more variants.            
u, update   Update world                            
upgrade     Upgrade world                           
clean       Clean world                             
f, find     Find some Atom in main tree and overlays
b, belongs  Find owner-package for some file        
```

# Design

Pretty fast by itself because it's lazy and [haskell thunks](https://wiki.haskell.org/Thunk) give own benefits.
Currently using binary serialization of parsed config / tree / overlays, will be updated if config or tree changes (has synergy with [Shelter](https://github.com/Miezhiko/Shelter)).
Just using `emerge` for many commands and I don't have plans to recode everything, only some stuff to be more handy for myself.

# WORK IN PROGRESS

many possible things to add and to improve
