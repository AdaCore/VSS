--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  JSON Schema types for Draft 4 - Draft 6 JSON Schema specification

with Ada.Containers.Vectors;

with VSS.Strings;
with VSS.String_Vectors;
with VSS.JSON;
with VSS.JSON.Events;

package JSON_Schema is
   pragma Preelaborate;

   type Schema is tagged;
   --  Core schema meta-schema

   type Schema_Access is access all Schema;

   package Definitions is
      package Schema_Vectors is new Ada.Containers.Vectors
        (Positive, Schema_Access);

      type Schema_Array is new Schema_Vectors.Vector with null record;
      --  Min_Items = 1 ???

      type Non_Negative_Integer is new Integer range 0 .. Integer'Last;

      type Non_Negative_Integer_Default_0 is new Non_Negative_Integer
        with Default_Value => 0;

      type Simple_Types is
        (An_Array,
         A_Boolean,
         An_Integer,
         A_Null,
         A_Number,
         An_Object,
         A_String);

      type String_Array is new VSS.String_Vectors.Virtual_String_Vector
        with null record;
      --  uniqueItems = True
   end Definitions;

   type String_Or_Schema (Is_String : Boolean := False) is record
      case Is_String is
         when True =>
            String : VSS.Strings.Virtual_String;
         when False =>
            Schema  : JSON_Schema.Schema_Access;
      end case;
   end record;

   type Property is record
      Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access;
   end record;

   package Property_Vectors is new Ada.Containers.Vectors (Positive, Property);

   package Simple_Type_Vectors is new Ada.Containers.Vectors
     (Positive, Definitions.Simple_Types, Definitions."=");

   package JSON_Event_Vectors is new Ada.Containers.Vectors
     (Positive, VSS.JSON.Events.JSON_Event, VSS.JSON.Events."=");

   type JSON_Value is new JSON_Event_Vectors.Vector with null record;
   --  Any JSON values.

   type JSON_Value_Array is new JSON_Event_Vectors.Vector with null record;
   --  Array of JSON values.

   subtype URI is VSS.Strings.Virtual_String;
   --  An absolute URI (starting with a scheme)

   subtype URI_Reference is VSS.Strings.Virtual_String;
   --  A relative path, fragment, or any other style of URI Reference
   --  (per RFC 3986) is allowable

   type Schema is tagged limited record
      Id                 : URI_Reference;  --  $id (id in Draft 4)
      Schema             : URI;  --  $schema
      Ref                : URI_Reference; --  $ref
      Comment            : VSS.Strings.Virtual_String;  --  $comment (since 7)
      Title              : VSS.Strings.Virtual_String;
      Description        : VSS.Strings.Virtual_String;
      Default            : JSON_Value;
      Read_Only          : Boolean := False;  --  since Draft 7
      Write_Only         : Boolean := False;  --  since Draft 7
      Examples           : JSON_Value_Array;  --  since Draft 6
      Multiple_Of        : VSS.JSON.JSON_Number;
      Maximum            : VSS.JSON.JSON_Number;
      Exclusive_Maximum  : VSS.JSON.JSON_Number;
      Minimum            : VSS.JSON.JSON_Number;
      Exclusive_Minimum  : VSS.JSON.JSON_Number;
      Max_Length         : Definitions.Non_Negative_Integer :=
                            Definitions.Non_Negative_Integer'Last;
      Min_Length         : Definitions.Non_Negative_Integer_Default_0;
      Pattern            : VSS.Strings.Virtual_String;  --  regexp?
      Additional_Items   : Schema_Access;
      Items              : Definitions.Schema_Array;  --  if a single Schema?
      Max_Items          : Definitions.Non_Negative_Integer :=
                            Definitions.Non_Negative_Integer'Last;
      Min_Items          : Definitions.Non_Negative_Integer_Default_0 := 0;
      Unique_Items       : Boolean := False;
      Contains           : Schema_Access;  --  since Draft 6
      Max_Properties     : Definitions.Non_Negative_Integer :=
                            Definitions.Non_Negative_Integer'Last;
      Min_Properties     : Definitions.Non_Negative_Integer_Default_0 := 0;
      Required           : Definitions.String_Array;

      Additional_Properties : Schema_Access;

      Properties         : Property_Vectors.Vector;
      Pattern_Properties : Property_Vectors.Vector;
      --  propertyNames": { "format": "regex" }

      Dependencies       : String_Or_Schema;
      Property_Names     : Schema_Access;  --  since Draft 6
      Const              : JSON_Event_Vectors.Vector;  --  since Draft 6
      Enum               : Definitions.String_Array;  --  May by not a strings?
      Kind               : Simple_Type_Vectors.Vector;  -- "type"
      Format             : VSS.Strings.Virtual_String;
      Content_Media_Type : VSS.Strings.Virtual_String;  --  since Draft 7
      Content_Encoding   : VSS.Strings.Virtual_String;  --  since Draft 7
      If_Schema          : Schema_Access;  --  since Draft 7
      Then_Schema        : Schema_Access;  --  since Draft 7
      Else_Schema        : Schema_Access;  --  since Draft 7
      All_Of             : Definitions.Schema_Array;
      Any_Of             : Definitions.Schema_Array;
      One_Of             : Definitions.Schema_Array;
      Negate             : Schema_Access;
   end record;
   --  Default: `{}` or `True`

   function Is_True (Self : Schema'Class) return Boolean;
   --  Check if given schema is "True". Than means it equals to `{}` JSON.

   function Is_False (Self : Schema'Class) return Boolean;
   --  Check if given schema is "False". Than means it equals to `not {}` JSON.
end JSON_Schema;
