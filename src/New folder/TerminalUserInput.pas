unit TerminalUserInput;

interface

///
/// Displays a prompt to the user, and reads in the string
/// they reply with. The string entered is then returned.
///
function ReadString(prompt: String): String;

///
/// Displays a prompt to the user, and reads in the number (whole)
/// they reply with. The function ensures that the user's entry
/// is a valid Integer. The Integer entered is then returned.
///
function ReadInteger(prompt: String): Integer;

///
/// Displays a prompt to the user, and reads in the number (real)
/// they reply with. The function ensures that the user's entry
/// is a valid Double. The Double entered is then returned.
///
function ReadDouble(prompt: String): Double;
///
/// Displays a prompt to the user, and reads in the number (whole)
/// they reply with. The function ensures that the user's entry
/// is in the a valid Integer range. The Integer entered is then returned.
///
function ReadIntegerRange(prompt: String; min, max: Integer): Integer;

implementation
uses SysUtils;

function ReadString(prompt: String): String;
begin
	Write(prompt);
	ReadLn(result);
end;

function ReadInteger(prompt: String): Integer;
var
	line: String;
begin
	line := ReadString(prompt);
	while not TryStrToInt(line, result) do
	begin
		WriteLn(line, ' is not an integer.');
		line := ReadString(prompt);
	end;
end;

function ReadIntegerRange(prompt: String; min, max: Integer): Integer;
begin
	result := ReadInteger(prompt);
	while (result < min) or (result > max) do
	begin
		WriteLn(' Please enter a number between ', min, ' and ', max);
		result := ReadInteger(prompt);
	end;
end;

function ReadDouble(prompt: String): Double;
var
	line: String;
begin
	line := ReadString(prompt);
	while not TryStrToFloat(line, result) do
	begin
		WriteLn(line, ' is not a double.');
		line := ReadString(prompt);
	end;
end;

end.