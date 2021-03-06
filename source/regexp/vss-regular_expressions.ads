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

limited private with VSS.Regular_Expressions.Engines;
limited private with VSS.Regular_Expressions.Matches;

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
   --  Return True when object was obtained as result of matching of valid
   --  regular expression.

   function Has_Match (Self : Regular_Expression_Match'Class) return Boolean;
   --  Return True when regular expression matched against the subject string.

   function Captured
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0) return VSS.Strings.Virtual_String;
   --  Return substring caprured by the Index capturing group. If capture group
   --  did not captured substring or there is no such captured group, returned
   --  string is null. Capture group 0 means substring matched by the entire
   --  pattern.

   function Captured
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Return substring captured by the named capture group Name. If capture
   --  group did not captured substring or there is no such named captured
   --  group, returned string is null.

   function Marker
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Cursors.Markers.Segment_Marker;
   --  Return segment marker for capture group Index.

   function Marker
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Cursors.Markers.Segment_Marker;
   --  Return segment marker for named capture group Name.

   function First_Marker
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Cursors.Markers.Character_Marker;
   --  Return marker at first position of the substring captured by capture
   --  group Index.

   function First_Marker
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Cursors.Markers.Character_Marker;
   --  Return marker at first position of the substring captured by named
   --  capture group Name.

   function Last_Marker
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Cursors.Markers.Character_Marker;
   --  Return marker at last position of the substring captured by capture
   --  group Index.

   function Last_Marker
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Cursors.Markers.Character_Marker;
   --  Return marker at last position of the substring captured by named
   --  capture group Name.

private

   type Engine_Access is
     access all VSS.Regular_Expressions.Engines.Engine'Class;

   type Regular_Expression is new Ada.Finalization.Controlled with record
      Data : Engine_Access;
   end record;

   overriding procedure Adjust (Self : in out Regular_Expression);
   overriding procedure Finalize (Self : in out Regular_Expression);

   type Match_Access is access all VSS.Regular_Expressions.Matches.Match;

   type Regular_Expression_Match is new Ada.Finalization.Controlled with record
      Data : Match_Access;
   end record;

   overriding procedure Adjust (Self : in out Regular_Expression_Match);
   overriding procedure Finalize (Self : in out Regular_Expression_Match);

end VSS.Regular_Expressions;
