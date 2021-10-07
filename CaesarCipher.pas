program CeasarCipher;
uses
   SysUtils;

function encrypt(shift: integer; text: string): string;
var
   cypher: string = '';
   msg: string;
   c, i: integer;

begin
   msg := UpCase(text);
   for i := 1 to Length(msg) do
      begin
         c := byte(msg[i]);
         if not(msg[i] = ' ') then
            begin
            c := c + (shift mod 26);
            if (c > byte('Z')) then
               c := c - 26;
            end;
         cypher := cypher + chr(c);
      end;
      encrypt := cypher;
end;

function decrypt(shift: integer; text: string): string;
var
   cypher: string = '';
   msg: string;
   c, i: integer;

begin
   msg := UpCase(text);
   for i := 1 to Length(msg) do
      begin
         c := byte(msg[i]);
         if not(msg[i] = ' ') then
            begin
            c := c - (shift mod 26);
            if (c < byte('A')) then
               c := c + 26;
            end;
         cypher := cypher + chr(c);
      end;
      decrypt := cypher;
end;

function solve(maxVal: integer; text: string): string;
var
   cypher, msg: string;
   c, i, n: integer;

begin
   msg := text;
   cypher := '';
   for n := 0 to maxVal DO
      begin
         cypher := '';
         for i := 1 to Length(msg) do
            begin
               c := byte(msg[i]);
               if not(msg[i] = ' ') then
                  begin
                  c := c + (n mod 26);
                  if (c > byte('Z')) then
                     c := c - 26;
                  end;
               cypher := cypher + chr(c);
             end;
             writeln('Caesar ' + IntToStr(n) + ': ' + cypher);
   end;
end;

function main(): boolean;
var
   product1, product2: string;

begin
   product1 := encrypt(8, 'This is a test string from Alans');
   writeln(product1);
   product2 := decrypt(8, product1);
   writeln(product2);
   solve(26, 'HAL');
end;

begin
   main();
end.
