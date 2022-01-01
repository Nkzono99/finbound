# finbound
Internal boundary processing library for Fortran.

# Install
```
git clone https://github.com/Nkzono99/finbound.git
cd finbound
vim Makefile  # modify fortran compiler(FC) and etc.
make
```

# Linker flags
```
LFLAGS = -Lfinbound/build/lib/ -Ifinbound/build/include/ -lfinbound

Including dependencies (after installing futils in the same way):
LFLAGS = -Lfutils/build/lib/ -Ifutils/build/include/ -lfutils -Lfinbound/build/lib/ -Ifinbound/build/include/ -lfinbound
```

# Dependencies
futils: https://github.com/Nkzono99/futils
