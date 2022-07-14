--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;

with VSS.Strings.Texts;
with VSS.String_Vectors;
with VSS.XML.Content_Handlers;
with VSS.XML.Error_Handlers;
with VSS.XML.Implementation.Error_Handlers;
with VSS.XML.Implementation.Template_Namespaces;
with VSS.XML.Implementation.Template_Programs;
with VSS.XML.Lexical_Handlers;

package VSS.XML.Implementation.Template_Evaluators is

   type Filter_Kind is
     (None, Ignore_Children);

   type State is record
      Element      : VSS.XML.Implementation.Template_Programs.Address;
      Namespace    :
        VSS.XML.Implementation.Template_Namespaces.Namespace_Access;
      Condition    : VSS.String_Vectors.Virtual_String_Vector;
      Negate       : Boolean;
      Iterator     :
        VSS.XML.Implementation.Template_Namespaces.Iterable_Iterator_Access;
      Content      : VSS.String_Vectors.Virtual_String_Vector;
      Text_Content : Boolean;
      Omit_Tag     : Boolean;

      System_Id    : VSS.Strings.Virtual_String;
      Line         : VSS.Strings.Texts.Line_Count;
      Column       : VSS.Strings.Character_Count;
   end record;

   package State_Vectors is
     new Ada.Containers.Vectors (Positive, State);

   type Template_Evaluator is
     limited new VSS.XML.Implementation.Template_Namespaces.Error_Handler with
   record
      Content : VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
      Lexical : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access;
      Error   : VSS.XML.Error_Handlers.SAX_Error_Handler_Access :=
        VSS.XML.Implementation.Error_Handlers.Default'Access;
      Globals : aliased VSS.XML.Implementation.Template_Namespaces.Namespace;

      Current : State;
      Stack   : State_Vectors.Vector;
   end record;

   procedure Evaluate
     (Self    : in out Template_Evaluator'Class;
      Binded  :
        not null VSS.XML.Implementation.Template_Namespaces.Namespace_Access;
      Program :
        VSS.XML.Implementation.Template_Programs.Instruction_Vectors.Vector;
      Success : in out Boolean);

   overriding procedure Report_Error
     (Self    : in out Template_Evaluator;
      Message : VSS.Strings.Virtual_String;
      Success : in out Boolean);

end VSS.XML.Implementation.Template_Evaluators;
