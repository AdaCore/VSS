--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Regular_Expressions.Engines is

   ------------------
   -- Error_String --
   ------------------

   function Error_String
     (Self : Engine'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Error;
   end Error_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Engine'Class;
      Options : VSS.Regular_Expressions.Pattern_Options;
      Error   : VSS.Strings.Virtual_String;
      Pattern : VSS.Strings.Virtual_String) is
   begin
      Self.Options := Options;
      Self.Error := Error;
      Self.Pattern := Pattern;
   end Initialize;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Engine'Class) return Boolean is
   begin
      return Self.Error.Is_Empty;
   end Is_Valid;

   -------------
   -- Options --
   -------------

   function Options (Self : Engine'Class) return Pattern_Options is
   begin
      return Self.Options;
   end Options;

   -------------
   -- Pattern --
   -------------

   function Pattern (Self : Engine'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Pattern;
   end Pattern;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : in out Engine'Class) is
   begin
      System.Atomic_Counters.Increment (Self.Counter);
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out Engine'Class; Last : out Boolean) is
   begin
      if System.Atomic_Counters.Decrement (Self.Counter) then
         Self.On_Destroy;
         Last := True;
      else
         Last := False;
      end if;
   end Unreference;

end VSS.Regular_Expressions.Engines;
