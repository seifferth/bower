% Bower - a frontend for the Notmuch email system
% Copyright (C) 2026 Frank Seifferth, Peter Wang
%
% A quoted-printable encoder following RFC 2045, except that it uses
% UNIX newline (LF) rather than DOS newline (CRLF).

:- module quoted_printable.
:- interface.

:- type charset
    --->    utf8.

:- pred make_quoted_printable(charset::in, string::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

make_quoted_printable(Charset, Text, EncodedText) :-
    Charset = utf8,
    encode_bytes(Text, 0, count_code_units(Text),
        [], RevChars0, 0, LineLength0),
    encode_rev_trailing_whitespace(RevChars0, RevChars, LineLength0),
    string.from_rev_char_list(RevChars, EncodedText).

:- pred encode_bytes(string::in, int::in, int::in,
    list(char)::in, list(char)::out, int::in, int::out) is det.

encode_bytes(String, Index0, EndIndex, !RevChars, !LineLength) :-
    ( Index0 < EndIndex ->
        string.unsafe_index_code_unit(String, Index0, Byte),
        encode_byte(Byte, !RevChars, !LineLength),
        encode_bytes(String, Index0 + 1, EndIndex, !RevChars, !LineLength)
    ;
        true
    ).

:- pred encode_byte(int::in, list(char)::in, list(char)::out,
    int::in, int::out) is det.

encode_byte(Byte, !RevChars, !LineLength) :-
    ( Byte = 10 ->  % LF
        encode_rev_trailing_whitespace(!RevChars, !.LineLength),
        !:RevChars = ['\n' | !.RevChars],
        !:LineLength = 0
    ; is_literal_or_whitespace_byte(Byte) ->
        char.det_from_int(Byte, Char),
        add_literal_char(Char, !RevChars, !LineLength)
    ;
        add_quoted_byte(Byte, !RevChars, !LineLength)
    ).

:- pred is_literal_or_whitespace_byte(int::in) is semidet.

is_literal_or_whitespace_byte(Byte) :-
    ( Byte = 9                  % Tab
    ; Byte = 32                 % Space
    ; Byte >= 33, Byte =< 60    % ! .. <
    ; Byte >= 62, Byte =< 126   % > .. ~
    ).

:- pred add_literal_char(char::in, list(char)::in, list(char)::out,
    int::in, int::out) is det.

add_literal_char(Char, !RevChars, !LineLength) :-
    CharLen = 1,
    maybe_soft_line_break(CharLen, !RevChars, !LineLength),
    !:RevChars = [Char | !.RevChars],
    !:LineLength = !.LineLength + CharLen.

:- pred add_quoted_byte(int::in, list(char)::in, list(char)::out,
    int::in, int::out) is det.

add_quoted_byte(Byte, !RevChars, !LineLength) :-
    Hi = (Byte /\ 0xf0) >> 4,
    Lo = (Byte /\ 0x0f),
    (
        char.int_to_hex_char(Hi, HiChar),
        char.int_to_hex_char(Lo, LoChar)
    ->
        QuotedLength = 3,
        maybe_soft_line_break(QuotedLength, !RevChars, !LineLength),
        !:RevChars = [LoChar, HiChar, '=' | !.RevChars],
        !:LineLength = !.LineLength + QuotedLength
    ;
        require.unexpected($module, $pred, "char.int_to_hex_char failed")
    ).

:- pred maybe_soft_line_break(int::in, list(char)::in, list(char)::out,
    int::in, int::out) is det.

maybe_soft_line_break(AddLen, !RevChars, !LineLength) :-
    (
        !.LineLength + AddLen + 1 > max_qp_line_length
        % Line needs folding; +1 is the soft line break via '='
    ->
        !:RevChars = ['\n', '=' | !.RevChars],
        !:LineLength = 0
    ;
        true
    ).

:- pred encode_rev_trailing_whitespace(list(char)::in, list(char)::out,
    int::in) is det.

encode_rev_trailing_whitespace(RevChars0, RevChars, LineLength0) :-
    (
        RevChars0 = [Head | Tail],
        ( Head = ' ', Quoted = ['0', '2', '=']
        ; Head = '\t', Quoted = ['9', '0', '=']
        )
    ->
        ( LineLength0 + 2 > max_qp_line_length ->
            RevChars = Quoted ++ ['\n', '='] ++ Tail
        ;
            RevChars = Quoted ++ Tail
        )
    ;
        RevChars = RevChars0
    ).

:- func max_qp_line_length = int.

max_qp_line_length = 76.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
