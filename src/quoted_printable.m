% Bower - a frontend for the Notmuch email system
% Copyright (C) 2026 Frank Seifferth
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

:- type encoder_state
    --->    encoder_state(list(char), int).

%-----------------------------------------------------------------------------%

make_quoted_printable(Charset, Text, EncodedText) :-
    Charset = utf8,
    string.to_utf8_code_unit_list(Text, Bytes0),
    reverse(Bytes0, Bytes),
    foldr(encode_bytes, Bytes, encoder_state([], 0), EncoderState1),
    EncoderState1 = encoder_state(Chars0, LineLength0),
    encode_rev_trailing_whitespace(Chars0, LineLength0, Chars1, _LineLength1),
    reverse(Chars1, Chars),
    string.from_char_list(Chars, EncodedText).

:- pred encode_bytes(int::in, encoder_state::in, encoder_state::out) is det.

encode_bytes(Byte, EncoderState0, EncoderState) :-
    EncoderState0 = encoder_state(Chars0, LineLength0),
    (
        ( Byte >= 33, Byte =< 60
        ; Byte >= 62, Byte =< 126
        ; Byte = 9
        ; Byte = 32 ),
        char.from_int(Byte, Char)
    ->
        QuotedChar = [Char], QuotedLength = 1
    ;
        Byte = 10   % UNIX newline
    ->
        QuotedChar = ['\n'], QuotedLength = 2 % will be CRLF during transport
    ;
        Hi = (Byte /\ 0xf0) >> 4,
        Lo = (Byte /\ 0x0f),
        (
            char.int_to_hex_char(Hi, HiChar),
            char.int_to_hex_char(Lo, LoChar)
        ->
            QuotedChar = [LoChar, HiChar, '='], QuotedLength = 3
        ;
            require.unexpected($module, $pred, "char.int_to_hex_char failed")
        )
    ),
    (
        QuotedChar = ['\n']
    ->
        encode_rev_trailing_whitespace(Chars0, LineLength0, Chars1,
            _LineLength1),
        Chars = QuotedChar ++ Chars1,
        LineLength = 0
    ;
        LineLength0 + QuotedLength + 1 > 76     % Line needs folding; +1 is
                                                % the soft line break via '='
    ->
        Chars = QuotedChar ++ ['\n', '='] ++ Chars0,
        LineLength = QuotedLength
    ;
        Chars = QuotedChar ++ Chars0,
        LineLength = LineLength0 + QuotedLength
    ),
    EncoderState = encoder_state(Chars, LineLength).

:- pred encode_rev_trailing_whitespace(list(char)::in, int::in,
    list(char)::out, int::out) is det.

encode_rev_trailing_whitespace(Chars0, LineLength0, Chars, LineLength) :-
    (
        Head = head(Chars0), Tail = tail(Chars0),
        (
            Head = ' ', Quoted = ['0', '2', '=']
        ;
            Head = '\t', Quoted = ['9', '0', '=']
        )
    ->
        (
            LineLength0 + 2 > 76
        ->
            Chars = Quoted ++ ['\n', '='] ++ Tail,
            LineLength = 3
        ;
            Chars = Quoted ++ Tail,
            LineLength = LineLength0 + 3
        )
    ;
        Chars = Chars0, LineLength = LineLength0
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
