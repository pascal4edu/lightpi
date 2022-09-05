unit ulist;

{$mode objfpc}

interface

uses Classes;

// lightweight double linked list, similar to TList but minimalistic and customized to our needs

type PLightListElement = ^TLightListElement;
     TLightListElement = record
       item: Pointer;
       prev, next: PLightListElement;
     end;

     TLightList = class(TObject)
       private
         head, tail, current: PLightListElement;
         FCount: LongInt;

         function GetElement(index: Longint): PLightListElement;

       public
         property Count: LongInt read FCount;

         Constructor Create; virtual;
         procedure Add(item: Pointer); virtual;
         procedure AddFirst(item: Pointer); virtual;
         procedure Insert(index: Longint; item: Pointer);
         function Get: Pointer;
         function GetFirst: Pointer;
         function GetSecond: Pointer;
         function GetThird: Pointer;
         function GetFourth: Pointer;
         function Items(index: Longint): Pointer;
         function Remove: Pointer; virtual;
         function RemoveFirst: Pointer; virtual;
         function Delete(index: Longint): Pointer; virtual;
         Destructor Destroy; override;
     end;

implementation

uses math;

// get a certain item, 0 based index, expensive function!
function TLightList.GetElement(index: Longint): PLightListElement;
var i: LongInt;
begin
  if index <= Count div 2 then
  begin
    current := head;
    for i := 0 to index - 1  do current := current^.next;
  end
  else
  begin
    current := tail;
    for i := 0 to Count - index - 2 do current := current^.prev;
  end;

  Result := current;
end;

Constructor TLightList.Create;
begin
  inherited;

  head := nil;
  tail := nil;
  FCount := 0;
end;

// add item to tail
procedure TLightList.Add(item: Pointer);
begin
  new(current);
  current^.item := item;
  current^.prev := tail;
  current^.next := nil;

  if tail <> nil then tail^.next := current;
  if head = nil then head := current;

  tail := current;
  inc(FCount);
end;

// add item to head
procedure TLightList.AddFirst(item: Pointer);
begin
  new(current);
  current^.item := item;
  current^.prev := nil;
  current^.next := head;

  if head <> nil then head^.prev := current;
  if tail = nil then tail := current;

  head := current;
  inc(FCount);
end;

// replaces the item at Position with the new one, shifts the rest up
procedure TLightList.Insert(index: Longint; item: Pointer);
var temp: PLightListElement;
begin
  // range check
  index := min(Count - 1, max(0, index));

  if index = 0 then AddFirst(item)
  else
  if index = Count - 1 then Add(item)
  else
  begin
    temp := GetElement(index);

    new(current);
    current^.item := item;

    current^.prev := temp^.prev;
    current^.prev^.next := current;

    current^.next := temp;
    temp^.prev := current;

    inc(FCount);
  end;
end;

// get item from tail
function TLightList.Get: Pointer;
begin
  if tail <> nil then
    Result := tail^.item
  else
    Result := nil;
end;

// get item from head
function TLightList.GetFirst: Pointer;
begin
  if head <> nil then
    Result := head^.item
  else
    Result := nil;
end;

// optimizations
function TLightList.GetSecond: Pointer;
begin
  if Count < 2 then
    Result := nil
  else
    Result := head^.next^.item;
end;

// optimizations
function TLightList.GetThird: Pointer;
begin
  if Count < 3 then
    Result := nil
  else
    Result := head^.next^.next^.item;
end;

// optimizations
function TLightList.GetFourth: Pointer;
begin
  if Count < 4 then
    Result := nil
  else
    Result := head^.next^.next^.next^.item;
end;

// get a certain item, 0 based index, expensive function!
function TLightList.Items(index: Longint): Pointer;
begin
  if (index < 0) or (index > Count - 1) then
  begin
    Result := nil;
    Exit;
  end;

  // optimizations for most common accessed items
  if index = 0 then
  begin
    Result := head^.item;
    Exit;
  end
  else
  if index = 1 then
  begin
    Result := head^.next^.item;
    Exit;
  end
  else
  if index = 2 then
  begin
    Result := head^.next^.next^.item;
    Exit;
  end
  else
  Result := GetElement(index)^.item;
end;

// remove item from tail
function TLightList.Remove: Pointer;
begin
  if tail = nil then Exit;

  current := tail;
  tail := current^.prev;

  if tail <> nil then
    tail^.next := nil
  else
    head := nil;

  Result := current^.item;
  Dispose(current);
  dec(FCount);
end;

// remove item from head
function TLightList.RemoveFirst: Pointer;
begin
  if head = nil then Exit;

  current := head;
  head := current^.next;

  if head <> nil then
    head^.prev := nil
  else
    tail := nil;

  Result := current^.item;
  Dispose(current);
  dec(FCount);
end;

// deletes the item at index position
function TLightList.Delete(index: Longint): Pointer;
begin
  if (index < 0) or (index > Count - 1) then
  begin
    Result := nil;
    Exit;
  end;

  if index = 0 then Result := RemoveFirst
  else
  if index = Count - 1 then Result := Remove
  else
  begin
    current := GetElement(index);
    current^.next^.prev := current^.prev;
    current^.prev^.next := current^.next;

    Result := current^.item;
    Dispose(current);

    dec(FCount);
  end;
end;

// empty list
Destructor TLightList.Destroy;
begin
  while Count > 0 do RemoveFirst;

  inherited;
end;

end.

