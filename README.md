Generates customized fantasy baseball dollar values.

This code is a re-write of the now defunct Last Player Picked (LPP) web site.
Thanks and credit should should go to that author (pseudonym Mays Copeland),
who made the source code available at https://github.com/mayscopeland/priceguide.

## Installation

```s
install.packages('devtools')
library('devtools')
install_github("lastplayerpicked", "couthcommander")
```

## Usage

```s
library(lastplayerpicked)
?league
l <- league()
ls <- league(source='steamer14')
lo <- league(source='oliver14')
lz <- league(source='zips14')
exportData(ls, 'steamer_proj')
exportData(lo, 'oliver_proj')
exportData(lz, 'zips_proj')
```

## Beta

This package is in early development.  Many bugs need to fixed and features added.  Feel free to make suggestions.
