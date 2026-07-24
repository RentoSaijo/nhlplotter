# Draw an NHL rink

`draw_rink()` creates a regulation NHL rink that can be extended with
`ggplot2` layers. The centered coordinate system matches the coordinate
fields returned by `nhlscraper`.

## Usage

``` r
draw_rink(view = "full", orientation = "horizontal")
```

## Arguments

- view:

  character rink view: `'full'` (`'f'`), `'offensive-half'` (`'oh'`),
  `'defensive-half'` (`'dh'`), `'offensive-zone'` (`'oz'`),
  `'defensive-zone'` (`'dz'`), or `'neutral-zone'` (`'nz'`)

- orientation:

  character orientation: `'horizontal'` (`'h'`) or `'vertical'` (`'v'`)

## Value

A `ggplot2` plot.

## Details

The offensive end is on the right for a horizontal rink and at the top
for a vertical rink. Use `xCoordNorm` and `yCoordNorm` from
[`nhlscraper::gc_pbp()`](https://rentosaijo.github.io/nhlscraper/reference/gc_play_by_play.html)
when every event should attack toward that end. Raw play-by-play and
replay coordinates remain spatially aligned but retain their physical
direction unless normalized by the caller.

Both orientations use the same layer mappings: map longitudinal
coordinates to `x` and lateral coordinates to `y`. On a vertical rink,
positive lateral coordinates appear on the right.

## References

[National Hockey League Official Rules
2025-2026](https://media.nhl.com/site/asset/public/ext/2025-26/2025-26Rules.pdf)

## Examples

``` r
draw_rink()

draw_rink(view = 'oz', orientation = 'v')

```
