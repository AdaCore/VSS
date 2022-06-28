--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Atomic_Counters;

private
package VSS.Regular_Expressions.Engines is

   pragma Preelaborate;

   type Engine is abstract tagged limited private;

   not overriding procedure Parse
     (Self    : in out Engine;
      Pattern : VSS.Strings.Virtual_String;
      Options : VSS.Regular_Expressions.Pattern_Options;
      Fit     : out Boolean) is abstract;
   --  Parse Patter as a regular expression. If the given patter is not
   --  compatible with the engine return Fit = False.

   not overriding procedure Match
     (Self    : Engine;
      Subject : VSS.Strings.Virtual_String;
      From    : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Options : Match_Options := No_Match_Options;
      Result  : out Match_Access) is abstract
     with Pre'Class => Self.Is_Valid;
   --  Search regexp in the Subject and return Result

   not overriding procedure On_Destroy (Self : in out Engine) is abstract;
   --  This is called before deallocation

   not overriding function Capture_Group_Count
     (Self : Engine) return Natural is abstract;

   procedure Initialize
     (Self    : in out Engine'Class;
      Options : VSS.Regular_Expressions.Pattern_Options;
      Error   : VSS.Strings.Virtual_String;
      Pattern : VSS.Strings.Virtual_String);

   procedure Reference (Self : in out Engine'Class) with Inline;

   procedure Unreference
     (Self : in out Engine'Class;
      Last : out Boolean)
        with Inline;

   function Is_Valid (Self : Engine'Class) return Boolean;

   function Error_String
     (Self : Engine'Class) return VSS.Strings.Virtual_String;

   function Options (Self : Engine'Class) return Pattern_Options;

   function Pattern (Self : Engine'Class) return VSS.Strings.Virtual_String;

private

   type Engine is abstract tagged limited record
      Counter : System.Atomic_Counters.Atomic_Counter;
      Options : VSS.Regular_Expressions.Pattern_Options;
      Error   : VSS.Strings.Virtual_String;
      Pattern : VSS.Strings.Virtual_String;
   end record;

end VSS.Regular_Expressions.Engines;
