--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
     (Anchored_Match);  --  Match whole string

   type Match_Options is array (Match_Option) of Boolean;

   No_Match_Options : constant Match_Options := (others => False);

   type Regular_Expression_Match is tagged private;

   ------------------------
   -- Regular_Expression --
   ------------------------

   type Regular_Expression is tagged private;
   pragma Preelaborable_Initialization (Regular_Expression);

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
   --  Perform a regular expression match of the Subject against given regexp
   --  taking Options (if provided) into account.

   function Match
     (Self    : Regular_Expression'Class;
      Subject : VSS.Strings.Virtual_String;
      From    : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Options : Match_Options := No_Match_Options)
      return Regular_Expression_Match;
   --  Perform a regular expression match of the Subject starting from given
   --  position (From) against the regexp. Take Options (if provided) into
   --  account. The beginning of a line assertion (`^`) matches From position
   --  even if Subject has character before it.

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

   function Has_Capture
     (Self  : Regular_Expression_Match'Class;
      Index : Positive) return Boolean;
   --  Check if the capturing group with given Index has been captured.

   function Captured
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0) return VSS.Strings.Virtual_String;
   --  Return substring captured by the Index capturing group. If capture group
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

   type Simple_Assertion_Kind is
     (Start_Of_Line, End_Of_Line, Word_Boundary, No_Word_Boundary);

end VSS.Regular_Expressions;
