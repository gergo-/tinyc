# Tiny-C in Prolog

This is an implementation of the "Tiny-C" language from
<https://www.iro.umontreal.ca/~felipe/IFT2030-Automne2002/Complements/tinyc.c>
in Prolog.

The original is sparsely documented and terse to the point of obfuscation.
In keeping with the "spirit" of the original, this Prolog version shares
these weaknesses. Neither version is suitable "as a pedagogical tool for
learning about compilers" (the stated goal of the original).

This code is very lightly tested with SWI-Prolog 7.6.4.

## Examples

```
$ echo "a=b=c=2<3;" | swipl -q -s tinyc.pl
a = 1
b = 1
c = 1
$ echo "{ i=1; while (i<100) i=i+i; }" | swipl -q -s tinyc.pl
i = 128
$ echo "{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }" | swipl -q -s tinyc.pl
i = 25
j = 25
$ echo "{ i=1; do i=i+10; while (i<50); }" | swipl -q -s tinyc.pl
i = 51
$ echo "{ i=1; while ((i=i+10)<50) ; }" | swipl -q -s tinyc.pl
i = 51
$ echo "{ i=7; if (i<5) x=1; if (i<10) y=2; }" | swipl -q -s tinyc.pl
i = 7
y = 2
```
