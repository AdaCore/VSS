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

with Ada.Unchecked_Deallocation;
with System.Atomic_Counters;

with VSS.Regular_Expressions.Engines;
with VSS.Regular_Expressions.Matches;
with VSS.Regular_Expressions.Pike_Engines;

package body VSS.Regular_Expressions is
   pragma Warnings (Off);

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left : Regular_Expression; Right : Regular_Expression) return Boolean
   is
   begin
      pragma Compile_Time_Warning (Standard.True, """="" unimplemented");
      return raise Program_Error with "Unimplemented function ""=""";
   end "=";

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left : Regular_Expression_Match; Right : Regular_Expression_Match)
      return Boolean
   is
   begin
      pragma Compile_Time_Warning (Standard.True, """="" unimplemented");
      return raise Program_Error with "Unimplemented function ""=""";
   end "=";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Regular_Expression) is
   begin
      if Self.Data /= null then
         Self.Data.Reference;
      end if;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Regular_Expression_Match) is
   begin
      if Self.Data /= null then
         System.Atomic_Counters.Increment (Self.Data.Counter);
      end if;
   end Adjust;

   -------------------------
   -- Capture_Group_Count --
   -------------------------

   function Capture_Group_Count
     (Self : Regular_Expression'Class) return Natural
   is
   begin
      return
        raise Program_Error with "Unimplemented function Capture_Group_Count";
   end Capture_Group_Count;

   -------------------------
   -- Capture_Group_Names --
   -------------------------

   function Capture_Group_Names
     (Self : Regular_Expression'Class)
      return VSS.String_Vectors.Virtual_String_Vector
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Capture_Group_Names unimplemented");
      return
        raise Program_Error with "Unimplemented function Capture_Group_Names";
   end Capture_Group_Names;

   --------------
   -- Captured --
   --------------

   function Captured
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Virtual_String
   is
   begin
      return Self.Data.Subject.Slice
        (From => Self.First_Marker (Index),
         To   => Self.Last_Marker (Index));
   end Captured;

   --------------
   -- Captured --
   --------------

   function Captured
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Virtual_String
   is
   begin
      return Self.Data.Subject.Slice
        (From => Self.First_Marker (Name),
         To   => Self.Last_Marker (Name));
   end Captured;

   ------------------
   -- Error_String --
   ------------------

   function Error_String
     (Self : Regular_Expression'Class) return VSS.Strings.Virtual_String
   is
   begin
      if Self.Data = null then
         return VSS.Strings.Empty_Virtual_String;
      else
         return Self.Data.Error_String;
      end if;
   end Error_String;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Regular_Expression) is
      procedure Free is new Ada.Unchecked_Deallocation
        (VSS.Regular_Expressions.Engines.Engine'Class, Engine_Access);

      Is_Last : Boolean;
   begin
      if Self.Data /= null then
         Self.Data.Unreference (Is_Last);

         if Is_Last then
            Free (Self.Data);
         end if;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Regular_Expression_Match) is
      procedure Free is new Ada.Unchecked_Deallocation
        (VSS.Regular_Expressions.Matches.Match, Match_Access);
   begin
      if Self.Data /= null
        and then System.Atomic_Counters.Decrement (Self.Data.Counter)
      then
         Free (Self.Data);
      end if;
   end Finalize;

   ------------------
   -- First_Marker --
   ------------------

   function First_Marker
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return VSS.Strings.Cursors.Abstract_Cursor'Class
        (Self.Marker (Index)).First_Marker;
   end First_Marker;

   ------------------
   -- First_Marker --
   ------------------

   function First_Marker
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return VSS.Strings.Cursors.Abstract_Cursor'Class
        (Self.Marker (Name)).First_Marker;
   end First_Marker;

   ---------------
   -- Has_Match --
   ---------------

   function Has_Match (Self : Regular_Expression_Match'Class) return Boolean is
   begin
      return Self.Data /= null and then Self.Data.Has_Match;
   end Has_Match;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Regular_Expression'Class) return Boolean is
   begin
      return Self.Data /= null and then Self.Data.Is_Valid;
   end Is_Valid;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Regular_Expression_Match'Class) return Boolean is
   begin
      return Self.Data /= null;
   end Is_Valid;

   -----------------
   -- Last_Marker --
   -----------------

   function Last_Marker
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return VSS.Strings.Cursors.Abstract_Cursor'Class
        (Self.Marker (Index)).Last_Marker;
   end Last_Marker;

   -----------------
   -- Last_Marker --
   -----------------

   function Last_Marker
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Cursors.Markers.Character_Marker is
   begin
      return VSS.Strings.Cursors.Abstract_Cursor'Class
        (Self.Marker (Name)).Last_Marker;
   end Last_Marker;

   ------------
   -- Marker --
   ------------

   function Marker
     (Self  : Regular_Expression_Match'Class;
      Index : Natural := 0)
      return VSS.Strings.Cursors.Markers.Segment_Marker
   is
   begin
      return Self.Data.Markers (Index + 1);
   end Marker;

   ------------
   -- Marker --
   ------------

   function Marker
     (Self : Regular_Expression_Match'Class; Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Cursors.Markers.Segment_Marker
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Marker unimplemented");
      return raise Program_Error with "Unimplemented function Marker";
   end Marker;

   -----------
   -- Match --
   -----------

   function Match
     (Self    : Regular_Expression'Class;
      Subject : VSS.Strings.Virtual_String;
      From    : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Options : Match_Options := No_Match_Options)
      return Regular_Expression_Match
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Match unimplemented");
      return raise Program_Error with "Unimplemented function Match";
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Self    : Regular_Expression'Class;
      Subject : VSS.Strings.Virtual_String;
      Options : Match_Options := No_Match_Options)
      return Regular_Expression_Match
   is
      Data : Match_Access;
   begin
      if Self.Is_Valid then
         Self.Data.Match (Subject, Options, Data);
      end if;

      return (Ada.Finalization.Controlled with Data);
   end Match;

   -------------
   -- Options --
   -------------

   function Options (Self : Regular_Expression'Class) return Pattern_Options is
   begin
      if Self.Data = null then
         return No_Pattern_Options;
      else
         return Self.Data.Options;
      end if;
   end Options;

   -------------
   -- Pattern --
   -------------

   function Pattern
     (Self : Regular_Expression'Class) return VSS.Strings.Virtual_String
   is
   begin
      if Self.Data = null then
         return VSS.Strings.Empty_Virtual_String;
      else
         return Self.Data.Pattern;
      end if;
   end Pattern;

   ---------------------------
   -- To_Regular_Expression --
   ---------------------------

   function To_Regular_Expression
     (Pattern : VSS.Strings.Virtual_String;
      Options : Pattern_Options := No_Pattern_Options)
      return Regular_Expression
   is
      Ok : Boolean;
   begin
      return
        Result : constant Regular_Expression :=
          (Ada.Finalization.Controlled with
           Data => new VSS.Regular_Expressions.Pike_Engines.Engine)
      do
         Result.Data.Parse (Pattern, Options, Ok);

         pragma Assert (Ok);
      end return;
   end To_Regular_Expression;

end VSS.Regular_Expressions;
