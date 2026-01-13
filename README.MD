# Type on Latin keyboard, get Hindi text

At first, refer to the given tables to get the characters you want. After a time this will become automatic.

Try live at https://www.eludev.fr/hindi

# Programming language
The application is written with Elm, which allows for:
- easy and clean development
- very small application size (33 kb for the generated javascript)
- easy communication with javascript
- simple, consistent and efficient tooling

# Rationale

We tried to use a logical transcription close to Davanagari logic:

## _ prefixed characters:
We use _ prefix only in _( to get a regular ( since we use ( for candrabindu  ँ

We use two suffixes:
- ' mainly for aspirated consonants (k' g' c' j' T' D' p' b' to get ख घ छ झ ठ ढ फ भ),
but also for ञ as n', श as s', anusvara ं as (' and visarga ः as :'
- and " used for ग़ : g" and  ष : s"

To use ' or " as quotation marks, insert a space before them.

## vowels

The choice between the two sets of vowels ("full" and "matra" vowels) is made automatically based on the context.

## anusvara ं or candrabindu ँ

The choice between anusvara and candrabindu is not automatic and should be made by the user. It is made easier by the fact that candrabindu is represented by ( to which you just add a ' to get anusvara (' .

To get a regular ( type _(

## ligatures

The basic consonant forms include the 'inherent vowel' a. Devanagari script uses ligatures when there is no vowel between two consonants. For example:
nd is translated as नद, which also stands for nada
To represent the nd sound as in "Hindi", you must mark a virama (transcribed as '-', the minus sign) between n and d:
n-d is translated as न्द, as in hin-dî हिन्दी

## font:

We use the Google Noto Sans Devanagari font.

For an introduction to Google Noto fonts, see [https://en.wikipedia.org/wiki/Noto_fonts#External_links](https://en.wikipedia.org/wiki/Noto_fonts#External_links)

To learn how to use Noto fonts, read [https://design.google/library/hot-type-always-fresh](https://design.google/library/hot-type-always-fresh) and [https://developers.google.com/fonts/docs/getting_started](https://developers.google.com/fonts/docs/getting_started)

For our hindi script program, this very simply translates to:
- 3 lines in index.html (just after the <head> tag):
```
  <link rel="preconnect" href="https://fonts.googleapis.com">
  <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  <link href="https://fonts.googleapis.com/css2?family=Noto+Sans+Devanagari:wght@100..900&display=swap" rel="stylesheet">
```

- and the following in the style.css stylesheet:
```
#convert{
  font-family: "Noto Sans Devanagari", sans-serif;
  font-optical-sizing: auto;
  font-weight: 400; /* range 100-900 */
  font-style: normal;
  font-variation-settings: "wdth" 100;
  font-size: 1.5em;
}
```
with `sans-serif` being the fallback font in the font-family specifiation.

# Unrecognized characters

Any character not recognized is rendered unmodified.


# License

MIT
