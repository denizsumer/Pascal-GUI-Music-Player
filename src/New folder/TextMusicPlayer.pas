program TextMusicPlayer;
uses TerminalUserInput;

type
  Genre = (Null, Pop, Rock, Jazz, Classic);

  Track = record
    trackName : String;
    trackNo : Integer;
    trackLoc : String;
  end;

  Playlist = record
    albumName : String;
    artistName : String;
    genre : Genre;
    yearRel : Integer;
    trackData : Array of Track;
    trackCount : Integer;
    albumId: Integer;
  end;

function SetNumber():Integer;
begin
  result := ReadInteger('Enter the number of albums : ');
end;

function ReadGenre(): Integer;
begin
  WriteLn('  Genres : ');
  WriteLn('    1. : ', genre(1));
  WriteLn('    2. : ', genre(2));
  WriteLn('    3. : ', genre(3));
  WriteLn('    4. : ', genre(4));
  result := ReadIntegerRange('  Select a genre (1 - 4) : ', 1, 4);
end;

function ReadAlbum(): Playlist;
var
  i: Integer;
begin;
  result.albumName := ReadString('  Name of the album : ');
  result.artistName := ReadString('  Name of the artist : ');
  result.genre := Genre(ReadGenre());
  result.yearRel := ReadInteger('  Release year : ');
  result.trackCount :=  ReadInteger('  Number of tracks : ');
  SetLength(result.trackdata, result.trackCount);

  for i := 0 to High(result.trackData) do
  begin
    WriteLn('___ Track No ', i+1, ' ___');
    result.trackData[i].trackName := ReadString('        Name of the track : ');
    result.trackData[i].trackLoc := ReadString('        Folder location : ');
  end;
end;

procedure WriteAlbum(const album: Playlist);
var
  i: Integer;
begin
  WriteLn('    Name: ', album.albumName);
  WriteLn('    Artist: ', album.artistName);
  WriteLn('    Genre : ', album.genre);
  WriteLn('    Release Year : ', album.yearRel);
  WriteLn('    Number of Tracks : ', album.trackCount);

  for i := 0 to High(album.trackData) do
  begin
    WriteLn('        Track #', i+1, ' Name : ', album.trackData[i].trackName);
    WriteLn('        Track #', i+1, ' Location : ', album.trackData[i].trackLoc);
  end;
  ReadLn();
end;

procedure ReadAlbums(var album: Array of Playlist);
var
  i: Integer;
begin
    for i := 0 to High(album) do
    begin
      WriteLn();
      WriteLn('_____ Album No ', i+1,' _____');
      album[i] := ReadAlbum();
    end;
end;

procedure WriteAlbums(var album: Array of Playlist);
var
  i: Integer;
begin
    for i := 0 to High(album) do
    begin
      WriteLn();
      WriteLn('Album #', i+1, ' : ');
      WriteAlbum(album[i])
    end;
end;

procedure PlayText(const album: Array of Playlist);
var
  selAlbum: Integer;
  selTrack: Integer;
begin
  WriteLn('Enter Album Number (1 to ', Ord(High(album)+1), ') : ');
  selAlbum := (ReadIntegerRange(' ', 1, Ord(High(album)+1))) - 1;

  WriteLn('Enter Track Number (1 to ', Ord(High(album[selAlbum].trackData)+1), ') : ');
  selTrack := (ReadIntegerRange(' ', 1, Ord(High(album[selAlbum].trackData)+1))) - 1;

  WriteLn();
  WriteLn('The track you selected is ', album[selAlbum].trackData[selTrack].trackName);
  WriteLn('          from the album ', album[selAlbum].albumName);
  WriteLn('          is now playing from file location ', album[selAlbum].trackData[selTrack].trackLoc);
  WriteLn();
  WriteLn('Press Enter to exit...');
  ReadLn();
end;



procedure Main();
var
  album: Array of Playlist;
begin
  WriteLn('____________ Welcome to TEXT MUSIC PLAYER ____________');
  WriteLn();
  SetLength(album, SetNumber());
  ReadAlbums(album);
  WriteAlbums(album);
  PlayText(album);
end;

begin
	Main();
end.
