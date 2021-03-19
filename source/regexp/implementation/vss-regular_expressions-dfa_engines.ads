------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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
--
--  This regexp engine use DFA to check regular expressions.

with Ada.Containers.Hashed_Maps;
with VSS.Characters;
with VSS.Regular_Expressions.Engines;

private
package VSS.Regular_Expressions.DFA_Engines is

   pragma Preelaborate;

   type Engine is new VSS.Regular_Expressions.Engines.Engine with private;

private

   type Jump is record
      Char  : VSS.Characters.Virtual_Character;
      State : Positive;
   end record;

   function Hash (Value : Jump) return Ada.Containers.Hash_Type;

   package Jump_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Jump,
      Element_Type    => Integer,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Engine is new VSS.Regular_Expressions.Engines.Engine with record
      Jumps : Jump_Maps.Map;
   end record;

   overriding procedure Parse
     (Self    : in out Engine;
      Pattern : VSS.Strings.Virtual_String;
      Options : VSS.Regular_Expressions.Pattern_Options;
      Fit     : out Boolean);

   overriding procedure On_Destroy (Self : in out Engine);

   overriding procedure Match
     (Self    : Engine;
      Subject : VSS.Strings.Virtual_String;
      Options : Match_Options := No_Match_Options;
      Result  : out Match_Access);

end VSS.Regular_Expressions.DFA_Engines;
