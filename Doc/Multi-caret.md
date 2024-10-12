# SynEdit Multicaret Editing: Programmer and Compatibility notes

## Overview of internals

### New data structures

The two new data strarctures introduces are TSynSelection (single selection):

```pascal
  TSynSelection = record
    Caret: TBufferCoord;
    Start: TBufferCoord;
    Stop: TBufferCoord;
    CaretAtEOL: Boolean;  // used by wordwrap
    procedure Normalize;
    function Normalized: TSynSelection;
    function IsEmpty: Boolean;
    procedure Join(const Sel: TSynSelection);
    function Intersects(const Other: TSynSelection): Boolean;
    function Contains(const BC: TBufferCoord): Boolean;
    constructor Create(const ACaret, AStart, AStop: TBufferCoord; ACaretAtEOL: Boolean = False);
    class operator Equal(a, b: TSynSelection): Boolean;
    class operator NotEqual(a, b: TSynSelection): Boolean;
    class function Invalid: TSynSelection; static;
    function IsValid: Boolean;
  end;
```
and TSynSelections (shown partially)

```pascal
TSynSelections = class
  public
    type
      TKeepSelection = (ksKeepBase, ksKeepActive);
    procedure Clear(KeepSelection: TKeepSelection = ksKeepActive);
    // properties
    // The last selection entered
    // Non-multicursor commands operate on the active selection
    property ActiveSelection: TSynSelection read GetActiveSelection write SetActiveSelection;
    // The selection that is kept when you clear multiple cursors
    // It the first one as in VS Code
    property BaseSelection: TSynSelection read GetBaseSelection write SetBaseSelection;
    property Count: Integer read GetCount;
    property ActiveSelIndex: Integer read FActiveSelIndex write SetActiveSelIndex;
    property IsEmpty: Boolean read GetIsEmpty;
    property Selection[Index: Integer]: TSynSelection read GetSelection; default;
  end;
```

TCustomSynedit owns a TSynSelections object exposed with the property ```Selections``` and a TSynSelection record exposed as ```Selection```.

Most of the commands and properties you are likely to have used, such as CaretXY, BeginBlock,
EndBlock, SelAvail, SelStart, SelEnd, SetCaretAndSelection etc operate as in previous versions
on the active selection only and reflected on the Selection property.  

Changes to the active selection are synched with the Selections object in DecPaintLock when
FPaintLock drops to 0.

### New and removed commands

Everything that relates to the old selection mode has been removed including the commands
ecSelectNormal, ecSelectLine and ecSelectColumn.

The Following new commands have been added:
```pascal
  AddKey(ecCancelSelections, SYNEDIT_ESCAPE, []);
  AddKey(ecSelColumnLeft, SYNEDIT_LEFT, [ssShift, ssAlt]);
  AddKey(ecSelColumnRight, SYNEDIT_RIGHT, [ssShift, ssAlt]);
  AddKey(ecSelColumnUp, SYNEDIT_UP, [ssShift, ssAlt]);
  AddKey(ecSelColumnDown, SYNEDIT_DOWN, [ssShift, ssAlt]);
  AddKey(ecSelColumnPageUp, SYNEDIT_PRIOR, [ssShift, ssAlt]);
  AddKey(ecSelColumnPageDown, SYNEDIT_NEXT, [ssShift, ssAlt]);
  AddKey(ecSelMatchingText, Ord('W'), [ssShift, ssCtrl]);
  AddKey(ecCaretsAtLineEnds, SYNEDIT_END, [ssAlt]);
```

In addition the ecSelWord (Ctrl+W) if pressed more than once adds to the selection following 
mathing words.

### New properties

A published property ```CaseSensitive``` has been added which defaults to False.  At runtime
if there is an attached highlighter it returns the corresponding value of the highlighter. 
This property it is used with the ecSelWord and ecMatchingText commands in finding matches.

### Command processing

Each internal command has information attached of the following type:

```pascal
  TSynCommandKind = (ckStandard, ckSingleCaret, ckMultiCaret);
```
Multi-caret commands are executed once per caret/selection.  Single caret commands cancel
multiple carets before executing, keeping the base selection.  Standard commands are executed 
once as before.

TCustomSynEdit.CommandProcessor is responsible for executing the commands according to 
their kind. Note though, that executing commands with TCustomSynEdit.ExecuteCommand only affects
the active selection.

## Compatibility with earlier versions

There should be no major compatibility issues.   Your code using SynEdit should most likely
compile with no changes or just minor changes.  The main incompatibility relates to the 
removal of the old selection mode properties and commands.

However if you need to account for multiple selections you need to bear the following in
mind.

### Storing and restoring selections

Since there may be more than one selections you need to do the following:

```pascal
var
  SelStorage: TSynSelStorage;

  // Store the selections
  Editor.Selections.Store(SelStorage);

  // Restore the selections
  Editor.Selections.Restore(SelStorage);
```

### Search and replace

TCustomSynEdit.SearchReplace works with multiple selections correctly, but if you have 
developed complex search and replace code that uses this routine, you may need to make
adjustments to account for  multiple selections.  The SearchReplaceDemo has been updated
and enhanced to serveas a guide.

TCustomSynEdit.SearchReplace has been significantly refactored and now an overload is 
available:

```pascal
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions): Integer; overload;
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions; const Start, Stop: TBufferCoord): Integer; overload;
```

With the second overload:
- if valid Start and Stop parameters are provided, then search/replace takes place between 
  those coordinates, ignoring the ssoSelection and ssoEntireScope options.  
- if Options include  ssoSelection and you provide a valid Start parameter and 
  Stop = TBufferCoord.Invalid then search/replace take place within the multiple selections, but 
  after/before the Start depending on the option ssoBackwards.

### Printing and previewing 

Printing and previewing does not yet support multiple selections, but they should work with
the active selection as before.