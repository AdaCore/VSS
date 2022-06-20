--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Special implementation of text streams to be used for testing.

with Ada.Streams;

with VSS.Characters;
with VSS.Stream_Element_Vectors;
with VSS.Strings;
with VSS.Text_Streams;

package Tests_Text_Streams is

   ------------------------------
   -- Memory_UTF8_Input_Stream --
   ------------------------------

   type Memory_UTF8_Input_Stream is
   limited new VSS.Text_Streams.Input_Text_Stream with record
      Buffer      : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Current     : Ada.Streams.Stream_Element_Count := 1;
      Skip        : Boolean := False;
      Incremental : Boolean := False;
      Diagnosis   : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Get
     (Self    : in out Memory_UTF8_Input_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding function Is_End_Of_Data
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Is_End_Of_Stream
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Has_Error
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Error_Message
     (Self : Memory_UTF8_Input_Stream) return VSS.Strings.Virtual_String;

   procedure Set_Incremental
     (Self : in out Memory_UTF8_Input_Stream'Class;
      To   : Boolean);

   -------------------------------
   -- Memory_UTF8_Output_Stream --
   -------------------------------

   type Memory_UTF8_Output_Stream is
   limited new VSS.Text_Streams.Output_Text_Stream with record
      Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Limit  : VSS.Strings.Character_Count := VSS.Strings.Character_Count'Last;
      Count  : VSS.Strings.Character_Count := 0;
      --  Count of the processed characters and limiting amount, Put operation
      --  returns failure when this limit has been reached.
   end record;

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   procedure Set_Limit
     (Self : in out Memory_UTF8_Output_Stream'Class;
      To   : VSS.Strings.Character_Count);
   --  Set limiting number of character that can be consumed by the text stream
   --  successfully. After reaching of this limit all subsequential Put
   --  operations will fail.

   --------------------------
   -- String_Output_Stream --
   --------------------------

   type String_Output_Stream is
   limited new VSS.Text_Streams.Output_Text_Stream with record
      Buffer : VSS.Strings.Virtual_String;
      Limit  : VSS.Strings.Character_Count := VSS.Strings.Character_Count'Last;
      --  Limiting amount of accumulated characters, Put operation returns
      --  failure when this limit has been reached.
   end record;

   overriding procedure Put
     (Self    : in out String_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   procedure Set_Limit
     (Self : in out String_Output_Stream'Class;
      To   : VSS.Strings.Character_Count);
   --  Set limiting number of character that can be consumed by the text stream
   --  successfully. After reaching of this limit all subsequential Put
   --  operations will fail.

end Tests_Text_Streams;
