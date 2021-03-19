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
