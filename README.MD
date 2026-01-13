# Type on Latin keyboard, get Bangla text

At first, refer to the given tables to get the characters you want. After a time this will become automatic.

Try live at https://www.eludev.fr/bangla

# Programming language
The application is written with Elm, which allows for:
- easy and clean development
- very small application size (33 kb for the generated javascript)
- easy communication with javascript
- simple, consistent and efficient tooling

# Rationale

We tried to use a logical transcription close to Bangla script logic:

## _ prefixed characters:
We use _ prefix only in _( to get a regular ( since we use ( for candrabindu  ँ

We use two suffixes:
- ' mainly for aspirated consonants (k' g' c' j' T' D' p' b' to get খ ঘ ছ ঝ ঠ ঢ থ ধ ফ ভ),
but also for ঞ as n', শ as s', anusvara ং as (' and visarga ः as :'
- and " used for ষ : s"

To use ' or " as quotation marks, insert a space before them.

## vowels

The choice between the two sets of vowels ("full" and "matra" vowels) is made automatically based on the context.


## ligatures

The basic consonant forms include the 'inherent vowel' a. Bangla script uses ligatures when there is no vowel between two consonants. For example:
nd is translated as নদ , which also stands for nada
To represent the nd sound as in "andar", you must mark a virama (transcribed as '-', the minus sign) between n and d:
n-d is translated as ন্দ, as in an-dr : অন্দর


# Unrecognized characters

Any character not recognized is rendered unmodified.


# License

MIT
