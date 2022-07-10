--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;

with VSS.IRIs;
with VSS.Strings.Texts;
with VSS.String_Vectors;

package VSS.XML.Implementation.Template_Programs is

   pragma Preelaborate;

   type Address is new Natural;

   Null_Address : constant Address := 0;

   type Instruction_Kind is
     (None,
      Start_Element,
      Attribute,
      End_Element,
      Text,
      Comment,
      Processing_Instruction,
      Location,                --  Location information for debug purposes.
      Condition,
      Content,
      Omit_Tag,
      Repeat,
      Done);

   type Instruction (Kind : Instruction_Kind := None) is record
      case Kind is
         when None =>
            null;

         when Start_Element =>
            URI        : VSS.IRIs.IRI;
            Name       : VSS.Strings.Virtual_String;

         when Attribute =>
            Attribute_URI   : VSS.IRIs.IRI;
            Attribute_Name  : VSS.Strings.Virtual_String;
            Attribute_Value : VSS.Strings.Virtual_String;
            Attribute_Path  : VSS.String_Vectors.Virtual_String_Vector;

         when End_Element =>
            Start_Address : Address;

         when Text =>
            CDATA : Boolean;
            Text  : VSS.Strings.Virtual_String;

         when Comment =>
            Comment : VSS.Strings.Virtual_String;

         when Processing_Instruction =>
            Target : VSS.Strings.Virtual_String;
            Data   : VSS.Strings.Virtual_String;

         when Location =>
            System_Id : VSS.Strings.Virtual_String;
            Line      : VSS.Strings.Texts.Line_Count;
            Column    : VSS.Strings.Character_Count;

         when Condition =>
            Negate         : Boolean;
            Condition_Path : VSS.String_Vectors.Virtual_String_Vector;

         when Content =>
            Is_Text      : Boolean;
            Content_Path : VSS.String_Vectors.Virtual_String_Vector;

         when Omit_Tag =>
            Omit_Tag : Boolean;

         when Repeat =>
            Identifier  : VSS.Strings.Virtual_String;
            Repeat_Path : VSS.String_Vectors.Virtual_String_Vector;

         when Done =>
            null;
      end case;
   end record;

   subtype Program_Address is Address range 1 .. Address'Last;

   package Instruction_Vectors is
     new Ada.Containers.Vectors (Program_Address, Instruction);

end VSS.XML.Implementation.Template_Programs;
