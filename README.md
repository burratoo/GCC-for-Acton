# GCC for Acton

GCC for Acton is a fork of GCC for the Acton project. The fork focuses on GCC's Ada front-end: modifying GNAT to implement the Cyclic Task syntax proposed in the PhD thesis *[Reducing the Cost of Real-Time Software through a Cyclic Task Abstraction for Ada](https://doi.org/10.25911/5d74e77b72869)*, and to adapt the front-end to work with the Ada tasking runtime implemented in Acton.

The repository includes the GMP, MPC, and MPFR libraries to allow building GCC without having to hunt for GCC's dependences.

The compiler can be built for arm targets using the following command:

	  $ ../gcc/configure --prefix=/usr/local/arm-acton-gnat --enable-languages=c,ada --disable-nls \
        --without-libiconv-prefix --disable-libmudflap --disable-libffi --disable-libstdcxx-pch    \
        --disable-libada --disable-libssp --target=arm-burratoo-acton --disable-gdbtk \
        --with-multilib-list=armv6-m,armv7-m,armv7e-m,armv7-r

The project is not currently being developed and is posted more for general interest.
