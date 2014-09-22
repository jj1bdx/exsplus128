# exsplus: xorshift128plus PRNG for Erlang

xorshift128plus is a 64-bit PRNG of (2^127-1) period.

See <http://xorshift.di.unimi.it/> for the further details.

A preliminary test shows the `exsplus` functions takes *less* execution time
than twice of `random` module on a x86\_64 or amd64 architecture environment.

## Author

Algorithm by Sebastiano Vigna, made public domain.

Programmed by Kenji Rikitake.

## LICENSE

MIT License.

(The code under `c-example` directory is licensed CC0/public domain.)
