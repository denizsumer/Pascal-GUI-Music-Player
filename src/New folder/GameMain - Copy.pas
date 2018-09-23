program GameMain;
uses SwinGame, sgTypes, SysUtils;

type
	Track = record
		trackName : String;
		trackNo : Integer;
		trackLoc : String;
  end;

	Album = record
		albumCover : String;
		albumName : String;
		artistName : String;
		yearRel : Integer;
		trackCount : Integer;
		trackData : Array of Track;
	end;

	Playlist = record
		albumData : Array [0..3] of Album;
	end;

procedure LoadTrackData(var trackData : Track; var playlistData : TextFile);
begin
	ReadLn(playlistData, trackData.trackNo);
	ReadLn(playlistData, trackData.trackName);
	ReadLn(playlistData, trackData.trackLoc);
end;

procedure LoadAlbumData(var playlistData : TextFile; var albums : Album);
var
	i : Integer;
begin
	i := 0;
	ReadLn(playlistData, albums.albumCover);
	ReadLn(playlistData, albums.albumName);
	ReadLn(playlistData, albums.artistName);
	ReadLn(playlistData, albums.yearRel);
	ReadLn(playlistData, albums.trackCount);
	for i := 0 to albums.trackCount - 1 do
	begin
		LoadTrackData(albums.trackData[i], playlistData);
	end;
end;

procedure LoadData(var playlistData : TextFile; var AlbumData : Array of Album);
var
	i : Integer;
begin
	for i := 0 to High(AlbumData) do
	begin
		LoadAlbumData(playlistData, AlbumData[i]);
	end;
end;

procedure InitialiseScreen(var albumData : Array of Album);
begin
	ClearScreen(colorBlue);
	LoadBitmapNamed('album1', albumData[0].albumCover);
	LoadBitmapNamed('album2', albumData[1].albumCover);
	LoadBitmapNamed('album3', albumData[2].albumCover);
	LoadBitmapNamed('album4', albumData[3].albumCover);
	DrawBitmap('album1', 50, 50);
    DrawBitmap('album2', 325, 50);
    DrawBitmap('album3', 50, 325);
    DrawBitmap('album4', 325, 325);
end;

procedure Main();
var
	playData : Playlist;
	playlistData : TextFile;

begin
OpenGraphicsWindow('Music Player CT2.1', 800, 600);
//ShowSwinGameSplashScreen();
AssignFile(playlistData, 'playlistData.dat');
Reset(playlistData);
LoadData(playlistData, playData.albumData);
Close(playlistData);
//InitialiseScreen(playData.albumData);

repeat
ProcessEvents();
DrawText(playData.albumData[0].albumName, colorBlack, 'arial.ttf', 14, 600, 50);
WriteLn(playData.albumData[1].albumName);

DrawFramerate(0,0);
RefreshScreen(60);

until WindowCloseRequested();

end;

begin
  Main();
end.
