--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides parser of command line arguments.

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Hashed_Sets;

private with VSS.Strings.Hash;
--  with VSS.String_Vectors;

package VSS.Command_Line.Parsers is

   type Command_Line_Parser is tagged limited private;

   procedure Add_Option
     (Self   : in out Command_Line_Parser'Class;
      Option : Abstract_Option'Class);
   --  Add command line option.

   function Parse
     (Self      : in out Command_Line_Parser'Class;
      Arguments : VSS.String_Vectors.Virtual_String_Vector) return Boolean;
   --  Parse given arguments. Return True on success, otherwise return
   --  False and set error message that can be retrieved with Error_Message
   --  subprogram.

   function Error_Message
     (Self : Command_Line_Parser'Class) return VSS.Strings.Virtual_String;
   --  Return error message if any.

   function Help_Text
     (Self : Command_Line_Parser'Class)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  Construct description of all registered options and returns it.

   function Is_Specified
     (Self   : Command_Line_Parser'Class;
      Option : Abstract_Option'Class) return Boolean;
   --  Return True when given option has been specified on the command line.

   function Value
     (Self   : Command_Line_Parser'Class;
      Option : Positional_Option'Class) return VSS.Strings.Virtual_String;
   --  Return value of the given option.

   function Value
     (Self   : Command_Line_Parser'Class;
      Option : Value_Option'Class) return VSS.Strings.Virtual_String;
   --  Return value of the given option.

   function Values
     (Self   : Command_Line_Parser'Class;
      Option : Value_Option'Class)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  Return all values of the given option specified in the command line.

   function Values
     (Self   : Command_Line_Parser'Class;
      Option : Name_Value_Option'Class) return Name_Value_Vectors.Vector;
   --  Return all name=value pairs of the given option specified in the
   --  command line.

   function Values
     (Self   : Command_Line_Parser'Class;
      Option : Multivalue_Positional_Option'Class)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  Return all values provided for given multivalued positional option.

   function Positional_Arguments
     (Self : Command_Line_Parser'Class)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  Return list of positional arguments.

   function Unknown_Option_Arguments
     (Self : Command_Line_Parser'Class)
      return VSS.String_Vectors.Virtual_String_Vector;
   --  Return list of undefined options found in the list of arguments.

private

   package Name_Sets is
     new Ada.Containers.Hashed_Sets
       (Element_Type        => VSS.Strings.Virtual_String,
        Hash                => VSS.Strings.Hash,
        Equivalent_Elements => VSS.Strings."=",
        "="                 => VSS.Strings."=");

   package Named_Option_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => VSS.Strings.Virtual_String,
        Element_Type    => Named_Option'Class,
        Hash            => VSS.Strings.Hash,
        Equivalent_Keys => VSS.Strings."=",
        "="             => "=");

   package Named_Value_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => VSS.Strings.Virtual_String,
        Element_Type    => VSS.String_Vectors.Virtual_String_Vector,
        Hash            => VSS.Strings.Hash,
        Equivalent_Keys => VSS.Strings."=",
        "="             => VSS.String_Vectors."=");

   package Positional_Option_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Positional_Option'Class);

   package Named_Option_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Named_Option'Class);

   package Multivalue_Positional_Option_Holders is
     new Ada.Containers.Indefinite_Holders (Multivalue_Positional_Option);

   type Command_Line_Parser is tagged limited record
      Defined_Named_Options_List   : Named_Option_Vectors.Vector;
      Defined_Short_Options        : Name_Sets.Set;
      Defined_Long_Options         : Name_Sets.Set;
      Defined_Named_Options        : Named_Option_Maps.Map;
      Defined_Positional_Options   : Positional_Option_Vectors.Vector;
      Defined_Multivalue_Positional_Option :
        Multivalue_Positional_Option_Holders.Holder;

      Error_Message                : VSS.Strings.Virtual_String;
      Only_Positional              : Boolean := False;
      --  All following arguments are positional arguments.
      Known_Named_Options_Values   : Named_Value_Maps.Map;
      Unknown_Named_Options_Values : Named_Value_Maps.Map;
      Positional_Options_Values    : VSS.String_Vectors.Virtual_String_Vector;
   end record;

end VSS.Command_Line.Parsers;
