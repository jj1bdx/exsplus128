# exsplus128: xorshift128plus PRNG for Erlang

xorshift128plus is a 64-bit PRNG of (2^127-1) period.

See <http://xorshift.di.unimi.it/> for the further details.

Version: 0.2.0

Note: this project is succeeded by [exsplus116](https://github.com/jj1bdx/exsplus116/).

## LICENSE

MIT License.

(The code under `c-example` directory is licensed CC0/public domain.)

## Note on changing the project name

This project was formerly called `exsplus`, but the name is changed to
avoid confusion with OTP's `exsplus` alrorithm, which is actually Xorshift116+
(to meet the 58-bit short integer limitation in Erlang), in the `rand` module.

# Tested platforms

* FreeBSD/amd64 10.1-STABLE with Erlang/OTP 17.5.2 and HiPE
* OS X x86\_64 10.10.3 with Erlang/OTP 17.5.2 and HiPE
* HiPE is not a requirement but recommended to be enabled.
* This code will run on 18.0.

A preliminary test shows the `exsplus` functions takes *less* execution time
than twice of `random` module on a x86\_64 or amd64 architecture environment.

## Make options (of erlang.mk)

* `Makefile` works on both BSD/GNU make
* `Makefile.[module_name]` is the real GNU make file; edit this file for modification
* Building: `make`
* Documentation: `make docs`
* Testing: `make tests`
* Execution speed benchmark: `make speed`
* See also [erlang.mk](https://github.com/extend/erlang.mk) for the details

## Authors

Algorithm by Sebastiano Vigna, and he made it public domain.

Programmed by Kenji Rikitake.

## Contributors

Dan Gudmundsson

