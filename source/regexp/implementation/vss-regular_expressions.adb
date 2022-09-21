--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;
with System.Atomic_Counters;

with VSS.Implementation.Referrers;
with VSS.Regular_Expressions.Engines;
with VSS.Regular_Expressions.Matches;
with VSS.Regular_Expressions.Pike_Engines;
with VSS.Strings.Cursors.Internals;
with VSS.Strings.Cursors.Iterators.Characters;
pragma Unreferenced (VSS.Strings.Cursors.Iterators.Characters);
--  GNAT 20220812 doesn't recognize use of this package

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
      return Self.Data.Capture_Group_Count;
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
      return VSS.Strings.Virtual_String is
   begin
      if Self.Is_Valid and then Index + 1 in Self.Data.Markers'Range then
         return
           Self.Data.Get_Owner.Slice
             (From => Self.First_Marker (Index),
              To   => Self.Last_Marker (Index));

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Captured;

   --------------
   -- Captured --
   --------------

   function Captured
     (Self : Regular_Expression_Match'Class;
      Name : VSS.Strings.Virtual_String)
      return VSS.Strings.Virtual_String is
   begin
      if Self.Is_Valid then
         return
           Self.Data.Get_Owner.Slice
             (From => Self.First_Marker (Name),
              To   => Self.Last_Marker (Name));

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
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

   -----------------
   -- Has_Capture --
   -----------------

   function Has_Capture
     (Self  : Regular_Expression_Match'Class;
      Index : Positive) return Boolean is
   begin
      return Self.Marker (Index).Is_Valid;
   end Has_Capture;

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
      use type VSS.Implementation.Referrers.Magic_String_Access;

   begin
      return Self.Data /= null and then Self.Data.Owner /= null;
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
      Data : Match_Access;
   begin
      if Self.Is_Valid and
        VSS.Strings.Cursors.Internals.Is_Owner (From, Subject)
      then
         Self.Data.Match (Subject, From, Options, Data);
      end if;

      return (Ada.Finalization.Controlled with Data);
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Self    : Regular_Expression'Class;
      Subject : VSS.Strings.Virtual_String;
      Options : Match_Options := No_Match_Options)
      return Regular_Expression_Match is
        (Self.Match (Subject, Subject.At_First_Character, Options));

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
