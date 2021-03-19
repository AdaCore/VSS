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
      Options : Match_Options := No_Match_Options;
      Result  : out Match_Access) is abstract
     with Pre'Class => Self.Is_Valid;
   --  Search regexp in the Subject and return Result

   not overriding procedure On_Destroy (Self : in out Engine) is abstract;
   --  This is called before deallocation

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
