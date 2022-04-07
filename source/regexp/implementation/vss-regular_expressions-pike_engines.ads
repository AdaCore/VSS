------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
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
--  This regexp engine use Pike's VM to check regular expressions.

with Ada.Containers.Vectors;
with VSS.Characters;
with VSS.Regular_Expressions.Engines;
with VSS.Regular_Expressions.Name_Sets;

private
package VSS.Regular_Expressions.Pike_Engines is

   pragma Preelaborate;

   type Engine is new VSS.Regular_Expressions.Engines.Engine with private;
   --  The regexp engine uses Pike's Virtual Machine to find a match

private

   type Tag_Number is new Natural;
   --  Tag represents subgroup boundaries
   type Instruction_Offset is new Integer;
   --  Offset between two VM instructions
   subtype Instruction_Address is Instruction_Offset
     range 1 .. Instruction_Offset'Last;
   --  Address of some VM instruction

   type Instruction_Kind is
     (No_Operation,  --  To able accept an empty string
      Split,         --  Create an alternative thread of execution
      Character,     --  Accept one Virtual_Character
      Class,         --  Accept one Virtual_Character from a class
      Category,      --  Accept one Virtual_Character from a general category
      Match,         --  Mark accepted string prefix as a regexp match
      Save);         --  Save subgroup bound
   --  VM instruction kinds

   type Instruction (Kind : Instruction_Kind := Split) is record
      Next : Instruction_Offset;
      --  Instruction address is relative to the current instruction

      case Kind is
         when Split =>
            Fallback : Instruction_Address;
         when Character =>
            Character : VSS.Characters.Virtual_Character;
         when Class =>
            From, To : VSS.Characters.Virtual_Character;
         when Category =>
            Category : Name_Sets.General_Category_Set;
         when Save =>
            Tag : Tag_Number;
         when No_Operation | Match =>
            null;
      end case;
   end record;

   package Instruction_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Instruction_Address,
      Element_Type => Instruction);

   type Engine is new VSS.Regular_Expressions.Engines.Engine with record
      Last_Tag    : Tag_Number;  --  Max tag used in the Program
      Max_Threads : Positive;  --  Maximum number of threads
      Program     : Instruction_Vectors.Vector;
      --  The program executes starting from program address = 1
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

   overriding function Capture_Group_Count (Self : Engine) return Natural;

end VSS.Regular_Expressions.Pike_Engines;
