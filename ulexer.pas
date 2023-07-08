unit ulexer;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ulist, uerror;

type TLPI_Lexer = class(TLPI_ErrorMessage)
       public
         function execute(input_string: AnsiString): TLightList;
     end;

implementation

uses uconstants, utypes, uutils;

function TLPI_Lexer.execute(input_string: AnsiString): TLightList;
var c: Char;
    line: Integer;

// preview next character
function next_ch: Char;
begin
  Result := c;
end;

// return buffer and fill it with the next character
function get_ch: Char;
begin
  if c = #10 then inc(line); // detect line feeds in Unix AND Windows

  Result := c;

  if input_string = '' then c := #0 // #0 is the internal symbol used for end of file / input
  else
  begin
    c := input_string[1];
    Delete(input_string, 1, 1);
  end;
end;

procedure skip_whitespace;
begin
  while (lpi_is_whitespace(next_ch)) do get_ch;
end;

// recognizes and reads a token (read = also deletes the token from the input stream)
procedure lpi_lex_read_token(tokenlist: TLightList);
var s: AnsiString;

function match_and_get_ch(ch: Char): Boolean;
begin
  Result := false;
  if UpperCase(next_ch) = ch then
  begin
    Result := true;
    s := s + get_ch;
  end;
end;

var id: Byte;
    token: TToken;
    nest_depth, current_line: Integer;
begin
  s := get_ch;

  if (lpi_is_alpha(s[1])) or (s[1] = '_') then // Identifier: keywords, variables, functions
  begin
    id := CtokenIdentifier;
    while ((lpi_is_alpha(next_ch)) or (lpi_is_numeric(next_ch)) or (next_ch = '_')) do s := s + get_ch;
  end
  else
  if (s[1] = '/') and (next_ch = '/') then // single line comment: // example comment
  begin
    id := CtokenComment;
    get_ch; // skip second /
    current_line := line;
    // skip until next line or end of file
    while (line = current_line) and (next_ch <> #0) do get_ch;
  end
  else
  if lpi_is_operator(s[1]) then // Operator: + - * / > < = :
  begin
    id := CtokenOperator;
    // only accumulate stuff like := <> >= etc, not -+ +- -- ++!
    if not lpi_is_math_operator(s[1]) then
    while (lpi_is_operator(next_ch)) do s := s + get_ch;
  end
  else
  if s[1] = '''' then // String: 'It''s working!'
  begin
    id := CtokenString;
    s := ''; // clear string, we don't want the leading ' char in our internal string

    while ((next_ch <> #0)) do
    begin
      case next_ch of
        '''': begin
                get_ch; // skip the closing ' and break unless we have the '' exception to continue
                if next_ch <> '''' then Break;
              end;
        #10: LogError('String not terminated', line, Cunknown_operation);
      end;

      s := s + get_ch;
    end;
  end
  else
  if s[1] = '$' then // Hex Number: AA, F0 etc.
  begin
    id := CtokenSingle; // it might be a single $ instead

    while (next_ch in ['0' .. '9', 'A' .. 'F']) do
    begin
      id := CtokenNumber; // found hex numbers, change type
      s := s + get_ch;
    end;
  end
  else
  if lpi_is_numeric(s[1]) then // Number: 78 or 0.234 leading + and - are ignored and evaluated later
  begin
    id := CtokenNumber;
    while (lpi_is_numeric(next_ch) or (next_ch = '.')) do s := s + get_ch;

    // check for exponent notation e.g. 1.234E4
    if match_and_get_ch('E') then
    begin
      // check for negative exponent or additional +
      if not match_and_get_ch('-') then match_and_get_ch('+');
      while lpi_is_numeric(next_ch) do s := s + get_ch;
    end;
  end
  else
  if s[1] = '{' then // multi line comment
  begin
    id := CtokenComment;
    nest_depth := 1;

    while ((nest_depth > 0) and (next_ch <> #0)) do
    begin
      get_ch;

      if next_ch = '{' then inc(nest_depth);
      if next_ch = '}' then dec(nest_depth);
    end;

    get_ch; // get ending }
  end
  else
  if (s[1] = '(') and (next_ch = '*') then // "classic" multi line comment
  begin
    id := CtokenComment;
    nest_depth := 1;

    while ((nest_depth > 0) and (next_ch <> #0)) do
    begin
      s := get_ch;

      if (s[1] = '(') and (next_ch = '*') then inc(nest_depth);
      if (s[1] = '*') and (next_ch = ')') then dec(nest_depth);
    end;

    get_ch; // get ending )
  end
  else id := CtokenSingle; // single token: ( ) ;

  // ignore comments, add everything else
  if id <> CtokenComment then
  begin
    token := TToken.Create;
    token.id := id;
    token.s := s;
    token.line := line;

    tokenlist.Add(token);
  end;
  skip_whitespace; // skips any whitespace between two commands, before or after a linebreak doesn't matter
end;

var i: Integer;
    tokenlist: TLightList;
    token: TToken;
begin
  Result := nil;

  if ClpiDebugMode then messages.Add('*** Lexer ***');

  tokenlist := TLightList.Create;
  line := 1; // start at line 1 of file or string

  get_ch; // fill "next_ch" buffer for first time use

  skip_whitespace; // skip the initial whitespace in the file, if any
  while (next_ch <> #0) and (not isError) do lpi_lex_read_token(tokenlist); // #0 is our internal symbol for end of file / input, it is NOT allowed as character in the sourcecode!

  if isError then
  begin
    // delete the token list...
    for i := 0 to tokenlist.Count - 1 do
    begin
      token := TToken(tokenlist.Items(i));
      FreeAndNil(token);
    end;

    FreeAndNil(tokenlist);
  end
  else
  begin
    // Debug: output our list of token...
    if ClpiDebugMode then
    for i := 0 to tokenlist.Count - 1 do
    begin
      with TToken(tokenlist.Items(i)) do
        messages.Add(IntToStr(line) + ' ' + tokentostr(id) + ': ' + s);
    end;
  end;

  Result := tokenlist;
end;

end.

