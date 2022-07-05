--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

--  private with VSS.IRIs;
--  with VSS.Strings.Hash;
with VSS.String_Vectors;
--  private with VSS.XML.Attributes;
with VSS.XML.Content_Handlers;
with VSS.XML.Implementation.Template_Namespaces;
with VSS.XML.Implementation.Template_Programs;
with VSS.XML.Lexical_Handlers;
--  with VSS.XML.Templates.Proxies;
--  private with VSS.XML.Locators;

package VSS.XML.Implementation.Template_Evaluators is

   type Filter_Kind is
     (None, Ignore_Children);

   type State is record
      Element   : VSS.XML.Implementation.Template_Programs.Address;
      Namespace : VSS.XML.Implementation.Template_Namespaces.Namespace_Access;
      Iterator  :
        VSS.XML.Implementation.Template_Namespaces.Iterable_Iterator_Access;
      Content   : VSS.String_Vectors.Virtual_String_Vector;
      --  Filter    : Filter_Kind;
   end record;

   package State_Vectors is
     new Ada.Containers.Vectors (Positive, State);

   type Template_Evaluator is tagged limited record
      Content : VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
      Lexical : VSS.XML.Lexical_Handlers.SAX_Lexical_Handler_Access;

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

end VSS.XML.Implementation.Template_Evaluators;
