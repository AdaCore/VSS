--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
      Class,         --  Accept one Virtual_Character from a range
      Category,      --  Accept one Virtual_Character from a general category
      Negate_Class,  --  Accept one Virtual_Character if not in a class/range
      Assertion,     --  Accept one assertion, like ^, $, \b, \B
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
         when Negate_Class =>
            --  Program to define char class starts from the next
            --  instruction after Negate_Class
            null;
         when Assertion =>
            Assertion : Simple_Assertion_Kind;
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
      From    : VSS.Strings.Cursors.Abstract_Cursor'Class;
      Options : Match_Options := No_Match_Options;
      Result  : out Match_Access);

   overriding function Capture_Group_Count (Self : Engine) return Natural;

end VSS.Regular_Expressions.Pike_Engines;
