# Bluespec Classic Testbenches

Bluespec Classic (`.bs`) testbenches that consume the shared qrisc32 test
vectors from `../../tests/` through build-local padded hex images.

The public `smoke`, `flags`, `isa`, and `axi` targets instantiate RTL from
`../rtl/`.  `Model*` benches keep the older architectural model available under
`model-*` targets.
