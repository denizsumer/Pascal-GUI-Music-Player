//  Author:			Deniz Sumer
//  Student ID:		101527131
//  Program:		Swingame Music Player
//  Task:			CT2.1
//  Date:			23/04/2017
//  Description:	GUI music player, loads album data automatically from text file.
//					Includes 4 albums and unlimited number of tracks for each album.
//					Allows user to change background. User able to explore albums
//					at the same time while selected track is playing. Indicates
//					playing track with a rectangle on it and writes album and artist
//					name at the bottom of the screen.
//					Functions : Play, Pause, Stop, Volume Up/Down and a Track Timer.
//					Keyboard Shortcuts : Z: Play / X: Pause / C: Stop
//										Up: Volume Up / Down: Volume Down

program Swingame_Music_Player_DS7131;
uses SwinGame, sgTypes, SysUtils;

type
	Track = record
		trackNo : Integer;
		trackName : String;
		trackLoc : String;
  end;

	Album = record
		albumCover : String;
		albumName : String;
		artistName : String;
		yearRel : Integer;
		trackCount : Integer;
		trackData : Array of Track;
		trackPlaying : Integer;
	end;

	Player = record
		albumData : Array [0..3] of Album;
		clr : Color;
		albumOnScreen : Integer;
		albumPlaying : Integer;
		timer : Timer;
		volume : Single;
	end;

function ButtonClicked(buttonX, buttonY, buttonW, buttonH: Integer) : Boolean;
var
	mX, mY: Single;
	width, height: Integer;

begin
	mX := MouseX();
	mY := MouseY();
	width := buttonX + buttonW;
	height := buttonY + buttonH;

	if MouseClicked(LeftButton) and (mX <= width) and (mX > buttonX) and (mY <= height) and (mY > buttonY) then
			result := true
			else result := false;
end;

function TimerMMSSFormat(timer: Integer) : String;
var
	seconds : Integer;
	mm : Integer;
	ss : Integer;
begin
	seconds := Trunc(timer / 1000);
	if seconds < 10 then result := '00:0' + FloattoStr(seconds);
	if (seconds >= 10) and (seconds < 60) then result := '00:' + FloattoStr(seconds);
	if seconds >= 60 then
		begin
		mm := Trunc(seconds / 60);
		ss := Seconds mod 60;
		if ss < 10 then result := '0' + FloatToStr(mm) + ':0' + FloattoStr(ss);
		if (ss >= 10) and (ss < 60) then result := '0' + FloatToStr(mm) + ':' + FloattoStr(ss);
		end;
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
	ReadLn(playlistData, albums.albumCover);
	ReadLn(playlistData, albums.albumName);
	ReadLn(playlistData, albums.artistName);
	ReadLn(playlistData, albums.yearRel);
	ReadLn(playlistData, albums.trackCount);
	SetLength(albums.trackData, albums.trackCount);
	for i := 0 to High(albums.trackData) do
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

procedure LoadResources(var albumData : Array of Album);
begin
	LoadBitmapNamed('album1', albumData[0].albumCover);
	LoadBitmapNamed('album2', albumData[1].albumCover);
	LoadBitmapNamed('album3', albumData[2].albumCover);
	LoadBitmapNamed('album4', albumData[3].albumCover);
	LoadBitmapNamed('play', 'play.png');
	LoadBitmapNamed('play-clicked', 'play-clicked.png');
	LoadBitmapNamed('pause', 'pause.png');
	LoadBitmapNamed('pause-clicked', 'pause-clicked.png');
	LoadBitmapNamed('stop', 'stop.png');
	LoadBitmapNamed('stop-clicked', 'stop-clicked.png');
end;

procedure DrawAlbumCovers(var albumOnScreen: Integer);
begin
	if albumOnScreen = 1 then FillRectangle(ColorBlack, 20, 20, 210, 210);
	if albumOnScreen = 2 then FillRectangle(ColorBlack, 245, 20, 210, 210);
	if albumOnScreen = 3 then FillRectangle(ColorBlack, 20, 245, 210, 210);
	if albumOnScreen = 4 then FillRectangle(ColorBlack, 245, 245, 210, 210);
	DrawBitmap('album1', 25, 25);
    DrawBitmap('album2', 250, 25);
    DrawBitmap('album3', 25, 250);
    DrawBitmap('album4', 250, 250);
    if ButtonClicked(25, 475, 120, 120) then  DrawBitmap('play-clicked', 25, 475) else DrawBitmap('play', 25, 475);
	if ButtonClicked(160, 475, 120, 120) then DrawBitmap('pause-clicked', 160, 475) else DrawBitmap('pause', 160, 475);
	if ButtonClicked(295, 475, 120, 120) then DrawBitmap('stop-clicked', 295, 475) else DrawBitmap('stop', 295, 475);
end;

procedure PlayingMusic(var playerData: Player);
begin
	ReleaseAllMusic();
	LoadMusicNamed('trackPlaying', PathToResource(playerdata.albumData[playerData.albumPlaying-1].trackData[playerdata.albumData[playerData.albumPlaying-1].trackPlaying-1].trackLoc));
	PlayMusic('trackPlaying');
	StartTimer('timer1');
end;

procedure ChooseTrack(var Albums: Album; var playerData: Player);
var
i: Integer;
begin
	if MouseClicked(LeftButton) and (MouseX() > 475) and (MouseX() < 600) then
	begin
		for i := 1 to High(albums.trackData) do
		begin
			if (MouseY() > 125 + i*25) and (MouseY() < 150 + i*25) then
			begin
				albums.trackPlaying := i;
				playerData.albumPlaying := playerData.albumOnScreen;
				PlayingMusic(playerData);
			end;
		end;
	end;
end;

procedure DisplayTrackList(var trackData : Track; var h: Integer);
var
	l : Integer;
begin
	l := trackData.trackNo + 5;
	DrawText('#' + FloattoStr(trackData.trackNo) + ' ' + trackData.trackName, colorBlack, 'arial.ttf', 16, 475, l*h);
end;

procedure DisplayAlbum(var albums : Album; var playerData : Player);
var
h : Integer; //lineheight
i : Integer;
begin
h := 25;
DrawText(albums.albumName, colorBlack, 'arial.ttf', 18, 475, h);
DrawText(albums.artistName, colorBlack, 'arial.ttf', 16, 475, 2*h);
DrawText('Release Year : ' + FloattoStr(albums.yearRel), colorBlack, 'arial.ttf', 14, 475, 3*h);
DrawText('Number of Tracks : ' + FloattoStr(albums.trackCount), colorBlack, 'arial.ttf', 14, 475, 4*h);
	for i := 0 to High(albums.trackData) do
	begin
		DisplayTrackList(albums.trackData[i], h);
	end;
ChooseTrack(albums, playerData);
end;

procedure ReadFromFile(var playlistData: TextFile; var playerdata: Player);
begin
	AssignFile(playlistData, 'playlistData.dat');
	Reset(playlistData);
	LoadData(playlistData, playerData.albumData);
	Close(playlistData);
end;

procedure SelectAlbum(var playerData: Player);
begin
	if ButtonClicked(25, 25, 250, 250) then playerData.albumOnScreen := 1;
	if ButtonClicked(250, 25, 250, 250) then playerData.albumOnScreen := 2;
	if ButtonClicked(25, 250, 250, 250) then playerData.albumOnScreen := 3;
	if ButtonClicked(250, 250, 250, 250) then playerData.albumOnScreen := 4;
	DisplayAlbum(playerData.albumData[playerData.albumOnScreen-1], playerData);
end;

procedure Timer(var playerData: Player);
begin
	if MusicPlaying() = true then
	begin
		DrawText(TimerMMSSFormat(TimerTicks('timer1')), colorBlack, 'arial.ttf', 24, 478, 510);
		DrawText('Now Playing : ',colorBlack, 'arial.ttf', 14, 425, 555);
		DrawText(playerdata.albumdata[playerData.albumPlaying-1].trackdata[playerdata.albumdata[playerData.albumPlaying-1].trackPlaying-1].trackName, colorBlack, 'arial.ttf', 14, 520, 555);
		DrawText('Artist : ', colorBlack, 'arial.ttf', 14, 468, 575);
		DrawText(playerdata.albumdata[playerData.albumPlaying-1].artistName, colorBlack, 'arial.ttf', 14, 520, 575);
	end;
	if MusicPlaying() = false then
	begin
		DrawText('00:00', colorBlack, 'arial.ttf', 24, 478, 510);
		DrawText('Now Playing : ',colorBlack, 'arial.ttf', 14, 425, 555);
		DrawText('Artist : ', colorBlack, 'arial.ttf', 14, 468, 575);
	end;
	if (playerData.albumPlaying = playerData.albumOnScreen) and (MusicPlaying() = true) then
	begin
		DrawRectangle (colorBlack, 470, (125+(playerdata.albumdata[playerData.albumPlaying-1].trackPlaying*25)), 330, 20);
	end;
end;

procedure AudioControls(var playerData: Player);
begin
   	if ButtonClicked(25, 475, 120, 120) or KeyTyped(vk_z) then
   	begin
		ResumeMusic();
   		ResumeTimer('timer1');
   	   	if HasMusic('trackPlaying') = false then
    	begin
   			playerdata.albumPlaying := 1;
   			playerdata.albumdata[0].trackPlaying := 1;
   			PlayingMusic(playerdata);
      	end;
   	end;
	if ButtonClicked(160, 475, 120, 120) or KeyTyped(vk_x) then
	begin
		PauseMusic();
		PauseTimer('timer1');
	end;
	if ButtonClicked(295, 475, 120, 120) or KeyTyped(vk_c) then
	begin
		StopMusic();
		ResetTimer('timer1');
		PauseTimer('timer1');
	end;
	if KeyDown(vk_UP) and (playerData.volume < 1) then playerData.volume := playerData.volume + 0.05;
	if KeyDown(vk_DOWN) and (playerData.volume > 0) then playerData.volume := playerData.volume - 0.05;
	DrawText('Volume : ' + FloattoStr(Trunc(playerData.volume*100)), colorBlack, 'arial.ttf', 14, 455, 535);
end;

procedure ChangeBackground(var playerdata: Player);
begin
	FillRectangle(ColorGrey, 700, 550, 90, 40);
	DrawText('Change', colorBlack, 'arial.ttf', 12, 720, 555);
	DrawText('Background', colorBlack, 'arial.ttf', 12, 710, 570);
	if ButtonClicked(710, 550, 90, 40) then playerData.clr := RandomRGBColor(255);
end;

procedure InitialisePlayer(var playerData: Player; var playlistData : Textfile);
begin
	OpenGraphicsWindow('Music Player CT2.1', 800, 600);
	ShowSwinGameSplashScreen();
	ReadFromFile(playlistData, playerData);
	LoadResources(playerData.albumData);
	playerData.clr := colorWhite;
	playerData.albumOnScreen := 1;
	playerData.albumPlaying := 1;
	playerData.volume := 0.7;
	CreateTimer('timer1');
end;

procedure Main();
var
	playerData : Player;
	playlistData : TextFile;
begin
	InitialisePlayer(playerData, playlistData);
	repeat
		ProcessEvents();
		ClearScreen(playerData.clr);
		DrawAlbumCovers(playerData.albumOnScreen);
		ChangeBackground(playerData);
		SelectAlbum(playerData);
		AudioControls(playerData);
		Timer(playerData);
		SetMusicVolume(playerData.volume);
		RefreshScreen(60);
	until WindowCloseRequested();
	
end;

begin
  Main();
end.
