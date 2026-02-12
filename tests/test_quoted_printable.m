%-----------------------------------------------------------------------------%

:- module test_quoted_printable.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module quoted_printable.

%-----------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test_make_quoted_printable, test_cases, !IO).

:- pred test_make_quoted_printable(string::in, io::di, io::uo) is det.

test_make_quoted_printable(Text, !IO) :-
    make_quoted_printable(utf8, Text, QuotedText),
    io.write_string(QuotedText, !IO),
    io.write_string("\n--------\n", !IO).

:- func test_cases = list(string).

test_cases = [
    "This is a simple short line in pure ASCII.",
    "Thïs ïs a lïne contaïnïng non-ASCII characters.",
    "A long line (i.e. longer than 76 characters allowed in RFC 5321) that needs to be broken across multiple lines when it is encoded using quoted-printable Content-Transfer-Encoding.",
    "Another long line, thïs tïme containing non-ASCII characters that take up more code points when they are encoded.",
    "",     % An empty line
    " ",    % A line containing only whitespace
    "\t",   % Another line containing only whitespace
    "A line containing trailing whitespace:            ",
    "A line containing trailing whitespace that breaks across two lines:            ",
    "A line containing trailing whitespace that breaks across two lines:            (but does not end in whitespace)",
    "A line ending with an explicit newline character.\n",
    "Trailing whitespace, comfortably fitting the line:     ",
    "Trailing whitespace, going up to the character limit:                     ",
    "Trailing whitespace, going beyond the character limit (case 1):            ",
    "Trailing whitespace, going beyond the character limit (case 2):             ",
    "Trailing whitespace, going beyond the character limit (case 3):              ",
    "A line ending with an explicit newline character.\n\nAnd an additional trailing line.",
    "Trailing whitespace, comfortably fitting the line:     \nAnd an additional trailing line.",
    "Trailing whitespace, going up to the character limit:                     \nAnd an additional trailing line.",
    "Trailing whitespace, going beyond the character limit (case 1):            \nAnd an additional trailing line.",
    "Trailing whitespace, going beyond the character limit (case 2):             \nAnd an additional trailing line.",
    "Trailing whitespace, going beyond the character limit (case 3):              \nAnd an additional trailing line.",
    "UTF-8 [äëïöü], whitespace, comfortable:       ",
    "UTF-8 [ëï], whitespace, up to the character limit:              ",
    "UTF-8 [ëï], whitespace, beyond the character limit (case 1):     ",
    "UTF-8 [ëï], whitespace, beyond the character limit (case 2):      ",
    "UTF-8 [ëï], whitespace, beyond the character limit (case 3):       ",
    "UTF-8 non-ASCII characters at the line breaking boundary: äëïöü",
    "UTF-8 non-ASCII characters at the line breaking boundary: _ äëïöü",
    "UTF-8 non-ASCII characters at the line breaking boundary: __ äëïöü",
    "UTF-8 non-ASCII characters at the line breaking boundary: __ äë__ïöü",
    "UTF-8 non-ASCII characters at the line breaking boundary: __ äë___ïöü",
    "END"
].


%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
