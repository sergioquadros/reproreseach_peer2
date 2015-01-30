---
title: "My REGEX's summary from R Documentation with CTRL + C, X and V"
author: "Sérgio Quadros"
date: "28-01-2015"
output: html_document
---

## regex {base}: my summary from R Documentation.

### Regular Expressions as used in R

The regular expression patterns supported by *grep* and related functions: *grepl, regexpr, gregexpr, sub, gsub,  strsplit.*

A *‘regular expression’*: a pattern that describes a set of strings.  
Two types:

*  Extended regular expressions (the default): fixed = TRUE which can be considered

*  Perl-like regular expressions: perl = TRUE. 

Patterns are described here as they would be printed by cat: (do remember that backslashes need to be doubled when entering R character strings, e.g. from the keyboard). About this "old line\n new line" they are talking? No, metacharacter versus non-metacharacter...


```r
cat("(")
```

```
## (
```

```r
cat("\\")
```

```
## \
```

```r
# cat("\(")
## Erro: '\('  é uma seqüência de escape não reconhecida na cadeia de caracteres começando com ""\("
cat("a\nb\nc")
```

```
## a
## b
## c
```

```r
cat(paste(letters, 100* 1:26), fill = TRUE, labels = paste0("{", 1:10, "}:"))
```

```
## {1}: a 100 b 200 c 300 d 400 e 500 f 600 g 700 h 800 i 900 j 1000 k 1100 
## {2}: l 1200 m 1300 n 1400 o 1500 p 1600 q 1700 r 1800 s 1900 t 2000 u 2100 
## {3}: v 2200 w 2300 x 2400 y 2500 z 2600
```

Do not assume that **long** regular expressions will be accepted: the POSIX standard only requires up to 256 bytes. Maybe a cause for my grep's problems?

### Extended Regular Expressions (ERE)

We have the regular expressions allowed in the default mode of _grep, regexpr, gregexpr, sub, gsub and strsplit._ They use an implementation of the POSIX 1003.2 standard.

They are similar to arithmetic expressions: various operators to combine smaller expressions. The whole expression matches zero or more characters (read ‘character’ as ‘byte’ if useBytes = TRUE).

The *fundamental building blocks* are the regular expressions that match *a single character*. 

Most characters, including all letters and digits, are regular expressions that match themselves. 

Any metacharacter with special meaning may be quoted by preceding it with a backslash. The metacharacters in EREs are: **. \ | ( ) [ { ^ $ * + ?**, but note that whether these have _a special meaning depends on the context_.

Escaping non-metacharacters with a backslash is implementation-dependent. The current implementation interprets \\a as BEL, \\e as ESC, \\f as FF, \\n as LF, \\r as CR and \\t as TAB. (Note that these will be interpreted by R's parser in literal character strings.)

Metacharacter: 

*  _Character class_: [*list of characters*] which matches _any single character in that list_

*  The caret ^_character_: which matches at first

*  The dollar $ in _character_$: which matches at last

*  _First char_ - _Last char_: hyphen in range of chars: [A-z]=[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz] Their interpretation is locale; [2-7]; usw.


```r
a <- c("123","234","345","456")
grep("^[2-3]",a)
```

```
## [1] 2 3
```

```r
grep("^[5-8]",a)
```

```
## integer(0)
```

```r
grep("[1-3]$",a)
```

```
## [1] 1
```

*  Certain *named classes* of characters are predefined. Their interpretation depends on the locale (see locales); the interpretation below is that of the POSIX locale:


     *  Alphanumeric characters: [:alnum:] = [:alpha:] and [:digit:]; [[:alnum:]] that is independent of locale and character encoding, otherwise [0-9A-Za-z] depends upon the locale and the character encoding

     *  Alphabetic characters: [:alpha:]

     *  Digits: 0 1 2 3 4 5 6 7 8 9 = [:digit:]

     *  Blank characters: space and tab, and possibly other locale-dependent characters such as non-breaking space = [:blank:]

     *  Lower and uppercase letters in current locale: [:lower:] and [:upper:]


     *  Control characters. In ASCII, these characters have octal codes 000 through 037, and 177 (DEL). In another character set, these are the equivalent characters, if any: [:cntrl:]

     *  Punctuation characters: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~ = [:punct:]

     *  Graphical characters: [:graph:] = [:alnum:] and [:punct:]

     *  Printable characters: [:print:] = [:alnum:]+[:punct:]+space

     *  Space characters: [:space:] = tab, newline, vertical tab, form feed, carriage return, space and possibly other locale-dependent characters.

     *  Hexadecimal digits: 0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f = [:xdigit:]

Most metacharacters lose their special meaning inside a character class. To include a literal ], place it first in the list. Similarly, to include a literal ^, place it anywhere but first. Finally, to include a literal -, place it first or last (or, for perl = TRUE only, precede it by a backslash). (Only ^ - \ ] are special inside character classes.)

*  The period . matches any single character.

*  The symbol \\w matches a ‘word’ character (a synonym for [[:alnum:]_]) and \\W is its negation. 

*  Symbols \\d, \\s, \\D and \\S denote the digit and space classes and their negations.

*  The caret ^ and the dollar sign $ are metacharacters that respectively match the empty string at the beginning and end of a line. 

*  The symbols \\< and \\> match the empty string at the beginning and end of a word.

*  The symbol \\b matches the empty string at either edge of a word, and \\B matches the empty string provided it is not at an edge of a word. 

_The interpretation of ‘word’ depends on the locale and implementation._

*  The repetition quantifiers:

     *  *?* is optional and will be matched at most once.

     *  \* will be matched zero or more times.

     *  \+ will be matched one or more times.

     *  {n} is matched exactly n times.

     *  {n,} is matched n or more times.

     *  {n,m} is matched at least n times, but not more than m times.

By default repetition is greedy, so the maximal possible number of repeats is used. This can be changed to ‘minimal’ by appending ? to the quantifier. (There are further quantifiers that allow approximate matching: see the TRE documentation.)

Regular expressions may be concatenated; the resulting regular expression matches any string formed by concatenating the substrings that match the concatenated subexpressions.

Two regular expressions may be joined by the infix operator |; the resulting regular expression matches any string matching either subexpression. For example, abba|cde matches either the string abba or the string cde. Note that alternation does not work inside character classes, where | has its literal meaning.

Repetition takes precedence over concatenation, which in turn takes precedence over alternation. A whole subexpression may be enclosed in parentheses to override these precedence rules.

The backreference \N, where N = 1 ... 9, matches the substring previously matched by the Nth parenthesized subexpression of the regular expression. (This is an extension for extended regular expressions: POSIX defines them only for basic ones.)

### Perl-like Regular Expressions

The perl = TRUE argument to grep, regexpr, gregexpr, sub, gsub and strsplit switches to the PCRE library that implements regular expression pattern matching using the same syntax and semantics as Perl 5.10, with just a few differences.

For complete details please consult the man pages for PCRE, especially man pcrepattern and man pcreapi), on your system or from the sources at http://www.pcre.org. If PCRE support was compiled from the sources within R, the PCRE version is 8.12 as described here.

Perl regular expressions can be computed byte-by-byte or (UTF-8) character-by-character: the latter is used in all multibyte locales and if any of the inputs are marked as UTF-8 (see Encoding).

All the regular expressions described for extended regular expressions are accepted except \< and \>: in Perl all backslashed metacharacters are alphanumeric and backslashed symbols always are interpreted as a literal character. { is not special if it would be the start of an invalid interval specification. There can be more than 9 backreferences (but the replacement in sub can only refer to the first 9).

Character ranges are interpreted in the numerical order of the characters, either as bytes in a single-byte locale or as Unicode code points in UTF-8 mode. So in either case [A-Za-z] specifies the set of ASCII letters.

In UTF-8 mode the named character classes only match ASCII characters: see \p below for an alternative.

The construct (?...) is used for Perl extensions in a variety of ways depending on what immediately follows the ?.

Perl-like matching can work in several modes, set by the options (?i) (caseless, equivalent to Perl's /i), (?m) (multiline, equivalent to Perl's /m), (?s) (single line, so a dot matches all characters, even new lines: equivalent to Perl's /s) and (?x) (extended, whitespace data characters are ignored unless escaped and comments are allowed: equivalent to Perl's /x). These can be concatenated, so for example, (?im) sets caseless multiline matching. It is also possible to unset these options by preceding the letter with a hyphen, and to combine setting and unsetting such as (?im-sx). These settings can be applied within patterns, and then apply to the remainder of the pattern. Additional options not in Perl include (?U) to set ‘ungreedy’ mode (so matching is minimal unless ? is used as part of the repetition quantifier, when it is greedy). Initially none of these options are set.

If you want to remove the special meaning from a sequence of characters, you can do so by putting them between \Q and \E. This is different from Perl in that $ and @ are handled as literals in \Q...\E sequences in PCRE, whereas in Perl, $ and @ cause variable interpolation.

The escape sequences \d, \s and \w represent any decimal digit, space character and ‘word’ character (letter, digit or underscore in the current locale: in UTF-8 mode only ASCII letters and digits are considered) respectively, and their upper-case versions represent their negation. Vertical tab is not regarded as a space character before PCRE 8.34 (included in R 3.0.3). Sequences \h, \v, \H and \V match horizontal and vertical space or the negation. (In UTF-8 mode, these do match non-ASCII Unicode code points.)

There are additional escape sequences: \cx is cntrl-x for any x, \ddd is the octal character (for up to three digits unless interpretable as a backreference, as \1 to \7 always are), and \xhh specifies a character by two hex digits. In a UTF-8 locale, \x{h...} specifies a Unicode code point by one or more hex digits. (Note that some of these will be interpreted by R's parser in literal character strings.)

Outside a character class, \A matches at the start of a subject (even in multiline mode, unlike ^), \Z matches at the end of a subject or before a newline at the end, \z matches only at end of a subject. and \G matches at first matching position in a subject (which is subtly different from Perl's end of the previous match). \C matches a single byte, including a newline, but its use is warned against. In UTF-8 mode, \R matches any Unicode newline character (not just CR), and \X matches any number of Unicode characters that form an extended Unicode sequence.

In UTF-8 mode, some Unicode properties are supported via \p{xx} and \P{xx} which match characters with and without property xx respectively. For a list of supported properties see the PCRE documentation, but for example Lu is ‘upper case letter’ and Sc is ‘currency symbol’.

The sequence (?# marks the start of a comment which continues up to the next closing parenthesis. Nested parentheses are not permitted. The characters that make up a comment play no part at all in the pattern matching.

If the extended option is set, an unescaped # character outside a character class introduces a comment that continues up to the next newline character in the pattern.

The pattern (?:...) groups characters just as parentheses do but does not make a backreference.

Patterns (?=...) and (?!...) are zero-width positive and negative lookahead assertions: they match if an attempt to match the ... forward from the current position would succeed (or not), but use up no characters in the string being processed. Patterns (?<=...) and (?<!...) are the lookbehind equivalents: they do not allow repetition quantifiers nor \C in ....

regexpr and gregexpr support ‘named capture’. If groups are named, e.g., "(?<first>[A-Z][a-z]+)" then the positions of the matches are also returned by name. (Named backreferences are not supported by sub.)

Atomic grouping, possessive qualifiers and conditional and recursive patterns are not covered here.

Author(s)

This help page is based on the documentation of GNU grep 2.4.2, the TRE documentation and the POSIX standard, and the pcrepattern man page from PCRE 8.0.

See Also

grep, apropos, browseEnv, glob2rx, help.search, list.files, ls and strsplit.

The TRE documentation at http://laurikari.net/tre/documentation/regex-syntax/).

The POSIX 1003.2 standard at http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html#tag_09

The pcrepattern can be found as part of http://www.pcre.org/pcre.txt, and details of Perl's own implementation at http://perldoc.perl.org/perlre.html.

