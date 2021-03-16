------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

private with Ada.Finalization;

with VSS.Strings.Cursors.Markers;
with VSS.String_Vectors;

package VSS.Regular_Expressions is

   pragma Preelaborate;

   type Pattern_Option is
     (Case_Insensitive,
      Dot_Matches_Everything,
      Multiline,
      Extended_Pattern_Syntax,
      Inverted_Greediness,
      Dont_Capture,
      Dont_Use_Unicode_Properties);

   type Pattern_Options is array (Pattern_Option) of Boolean;

   No_Pattern_Options : constant Pattern_Options := (others => False);

   type Match_Option is
     (Anchored_Match);

   type Match_Options is array (Match_Option) of Boolean;

   No_Match_Options : constant Match_Options := (others => False);

   type Regular_Expression_Match is tagged private;

   ------------------------
   -- Regular_Expression --
   ------------------------

   type Regular_Expression is tagged private;

   overriding function "="
     (Left : Regular_Expression; Right : Regular_Expression) return Boolean;

   function To_Regular_Expression
     (Pattern : VSS.Strings.Virtual_String;
      Options : Pattern_Options := No_Pattern_Options)
      return Regular_Expression;

   function Is_Valid (Self : Regular_Expression'Class) return Boolean;

   function Error_String
     (Self : Regular_Expression'Class) return VSS.Strings.Virtual_String;

   function Pattern
     (Self : Regular_Expression'Class) return VSS.Strings.Virtual_String;

   function Options (Self : Regular_Expression'Class) return Pattern_Options;

   function Capture_Group_Count
     (Self : Regular_Expression'Class) return Natural;

   function Capture_Group_Names
     (Self : Regular_Expression'Class)
      return VSS.String_Vectors.Virtual_String_Vector;

   function Match
     (Self    : Regular_Expression'Class;
      Subject : VSS.Strings.Virtual_String;
      Options : Match_Options := No_Match_Options)
      return Regular_Expression_Match;

   function Match
     (Self    : Regular_Expression'Class;
      Subject : VSS.Strings.Virtual_String;
      From    : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Options : Match_Options := No_Match_Options)
      return Regular_Expression_Match;

   ------------------------------
   -- Regular_Expression_Match --
   ------------------------------

   overriding function "="
     (Left  : Regular_Expression_Match;
      Right : Regular_Expression_Match) return Boolean;

   function Is_Valid (Self : Regular_Expression_Match'Class) return Boolean;

   function Captured
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0) return VSS.Strings.Virtual_String;

   function Captured
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;

   function Captured_Start
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Cursors.Markers.Virtual_Marker;

   function Captured_Start
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Cursors.Markers.Virtual_Marker;

   function Captured_End
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Cursors.Markers.Virtual_Marker;

   function Captured_End
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Cursors.Markers.Virtual_Marker;

   function Captured_Character_Length
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0) return VSS.Strings.Character_Count;

   function Captured_Character_Length
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String) return VSS.Strings.Character_Count;

private

   type Regular_Expression is new Ada.Finalization.Controlled with null record;

   type Regular_Expression_Match is
     new Ada.Finalization.Controlled with null record;

end VSS.Regular_Expressions;
