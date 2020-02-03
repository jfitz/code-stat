{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev: 54 $
 * $Date: 2012-03-18 01:02:06 +0000 (Sun, 18 Mar 2012) $
 *
 * Provides OS platform specific information and customisations.
}


unit UPlatform;


interface


uses
  // Delphi
  Forms;


///  <summary>Sets default font of given form and its contained controls to use
///  OSs default system font if necessary.</summary>
procedure SetDefaultFormFont(const AForm: TCustomForm);


implementation


uses
  // Delphi
  SysUtils, Classes, Controls, Windows, Graphics;


type
  ///  <summary>Class helper that provides access to protected Font and
  ///  ParentFont properties of TControl.</summary>
  TControlHelper = class helper for TControl
  public
    ///  <summary>Gets value of TControl.Font property.</summary>
    function GetFont: TFont;
    ///  <summary>Gets value of TControl.ParentFont property.</summary>
    function GetParentFont: Boolean;
  end;

///  <summary>Helper function that gets handle to underlying operating system's
///  default font.</summary>
function GetDefaultFontHandle: HFONT;
var
  LogFont: TLogFont;  // structure storing info about logical font
begin
  if SystemParametersInfo(
    SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0
  ) then
    // system font is same as that used for desktop icon titles
    Result := CreateFontIndirect(LogFont)
  else
    // can't get icon title font: use stock object
    Result := GetStockObject(DEFAULT_GUI_FONT);
end;

procedure SetDefaultFormFont(const AForm: TCustomForm);
var
  DefFont: TFont;   // operating system's default font
  CmpIdx: Integer;  // loops through all form's owned components
  Cmp: TComponent;  // reference to each component owned by form
  Ctrl: TControl;
begin
  // Create default font
  DefFont := TFont.Create;
  try
    DefFont.Handle := GetDefaultFontHandle;
    // Scan through all form's components, looking for TControls
    for CmpIdx := 0 to Pred(AForm.ComponentCount) do
    begin
      Cmp := AForm.Components[CmpIdx];
      if (Cmp is TControl) then
      begin
        Ctrl := Cmp as TControl;
        // Got a TControl. If control's font is not same as form's, but has same
        // name, we change name to default font name. If control's font has same
        // name and size as form, we change both name and size to that of
        // default font
        if not Ctrl.GetParentFont and
          AnsiSameText(Ctrl.GetFont.Name, AForm.Font.Name) then
        begin
          if Ctrl.GetFont.Size = AForm.Font.Size then
            Ctrl.GetFont.Size := DefFont.Size;
          Ctrl.GetFont.Name := DefFont.Name;
        end;
      end;
    end;
    // Finally, change form's font: this affects all controls with ParentFont
    // equal to True
    AForm.Font.Assign(DefFont);
  finally
    FreeAndNil(DefFont);
  end;
end;

{ TControlHelper }

function TControlHelper.GetFont: TFont;
begin
  Result := Self.Font;
end;

function TControlHelper.GetParentFont: Boolean;
begin
  Result := Self.ParentFont;
end;

end.

