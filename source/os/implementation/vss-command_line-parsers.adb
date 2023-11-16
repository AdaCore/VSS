--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Application;
with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Grapheme_Cluster_Iterators;
with VSS.Strings.Word_Iterators;

package body VSS.Command_Line.Parsers is

   use type VSS.Strings.Grapheme_Cluster_Count;

   procedure Parse_Argument
     (Self      : in out Command_Line_Parser'Class;
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Index     : in out Positive;
      Success   : in out Boolean);
   --  Parse single argument. Index may be incremented when known named
   --  argument requires value.

   procedure Remove_Prefix
     (Item   : in out VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String);

   Short_Prefix : constant VSS.Strings.Virtual_String := "-";
   Long_Prefix  : constant VSS.Strings.Virtual_String := "--";

   Screen_Width      : constant VSS.Strings.Grapheme_Cluster_Count := 80;
   Option_Width      : constant VSS.Strings.Grapheme_Cluster_Count := 28;
   Description_Width : constant VSS.Strings.Grapheme_Cluster_Count :=
     Screen_Width - Option_Width - 2;

   Description_Continuation_Indent : constant := 2;

   function Length
     (Item : VSS.Strings.Virtual_String)
      return VSS.Strings.Grapheme_Cluster_Count;
   --  Computes length of the given string in grapheme clusters.

   function Format_Description
     (Item : VSS.Strings.Virtual_String)
      return VSS.String_Vectors.Virtual_String_Vector;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option
     (Self   : in out Command_Line_Parser'Class;
      Option : Abstract_Option'Class) is
   begin
      if Option in Named_Option'Class then
         Self.Defined_Named_Options_List.Append (Named_Option'Class (Option));

         if not Named_Option'Class (Option).Short_Name.Is_Empty then
            Self.Defined_Short_Options.Insert
              (Named_Option'Class (Option).Short_Name);
            Self.Defined_Named_Options.Insert
              (Named_Option'Class (Option).Short_Name,
               Named_Option'Class (Option));
         end if;

         if not Named_Option'Class (Option).Long_Name.Is_Empty then
            Self.Defined_Long_Options.Insert
              (Named_Option'Class (Option).Long_Name);
            Self.Defined_Named_Options.Insert
              (Named_Option'Class (Option).Long_Name,
               Named_Option'Class (Option));
         end if;

      else
         Self.Defined_Positional_Options.Append
           (Positional_Option'Class (Option));
      end if;
   end Add_Option;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : Command_Line_Parser'Class) return VSS.Strings.Virtual_String is
   begin
      return Self.Error_Message;
   end Error_Message;

   ------------------------
   -- Format_Description --
   ------------------------

   function Format_Description
     (Item : VSS.Strings.Virtual_String)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      function Max_Width return VSS.Strings.Grapheme_Cluster_Count;
      --  Maximun width of the currently constructed line.

      procedure Append_Line;
      --  Appends currently constructed line to the result.

      Result       : VSS.String_Vectors.Virtual_String_Vector;
      Line         : VSS.Strings.Virtual_String;
      Line_Length  : VSS.Strings.Grapheme_Cluster_Count := 0;
      Space        : VSS.Strings.Virtual_String;
      Space_Length : VSS.Strings.Grapheme_Cluster_Count := 0;
      Word         : VSS.Strings.Virtual_String;
      Word_Length  : VSS.Strings.Grapheme_Cluster_Count := 0;
      Iterator     : VSS.Strings.Word_Iterators.Word_Iterator :=
        Item.Before_First_Word;

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line is
      begin
         if not Result.Is_Empty then
            for J in 1 .. Description_Continuation_Indent loop
               Line.Prepend (' ');
            end loop;
         end if;

         Result.Append (Line);
      end Append_Line;

      ---------------
      -- Max_Width --
      ---------------

      function Max_Width return VSS.Strings.Grapheme_Cluster_Count is
      begin
         if Result.Is_Empty then
            return Description_Width;

         else
            return Description_Width - Description_Continuation_Indent;
         end if;
      end Max_Width;

   begin
      while Iterator.Forward loop
         if Iterator.On_Whitespace then
            Space        := Iterator.Element;
            Space_Length := Length (Space);

         else
            Word        := Iterator.Element;
            Word_Length := Length (Word);

            if Line_Length + Space_Length + Word_Length
                 < Max_Width
            then
               Line.Append (Space);
               Line.Append (Word);
               Line_Length := @ + Space_Length + Word_Length;

            else
               Append_Line;

               Line        := Word;
               Line_Length := Word_Length;
            end if;

            Space.Clear;
            Space_Length := 0;
         end if;
      end loop;

      if not Line.Is_Empty then
         Append_Line;
      end if;

      return Result;
   end Format_Description;

   ---------------
   -- Help_Text --
   ---------------

   function Help_Text
     (Self : Command_Line_Parser'Class)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      procedure Append_Option_Description
        (Option_Text : in out VSS.Strings.Virtual_String;
         Description : VSS.Strings.Virtual_String);

      Result      : VSS.String_Vectors.Virtual_String_Vector;
      Option_Text : VSS.Strings.Virtual_String;
      Indent      : VSS.Strings.Virtual_String;

      -------------------------------
      -- Append_Option_Description --
      -------------------------------

      procedure Append_Option_Description
        (Option_Text : in out VSS.Strings.Virtual_String;
         Description : VSS.Strings.Virtual_String)
      is
         Description_Text : VSS.String_Vectors.Virtual_String_Vector :=
           Format_Description (Description);

      begin
         if Length (Option_Text) < Option_Width then
            Option_Text.Append (' ');

            for J in Length (Option_Text) .. Option_Width loop
               Option_Text.Append (' ');
            end loop;

            Option_Text.Append (Description_Text.First_Element);
            Description_Text.Delete_First;
         end if;

         Result.Append (Option_Text);

         for Line of Description_Text loop
            Option_Text := Indent;
            Option_Text.Append (Line);
            Result.Append (Option_Text);
         end loop;
      end Append_Option_Description;

   begin
      for J in 1 .. Option_Width + 1 loop
         Indent.Append (' ');
      end loop;

      declare
         Executable : constant VSS.Strings.Virtual_String :=
           VSS.Application.Application_File.Split
             ('/').Last_Element.Split ('\').Last_Element;
         Usage      : VSS.Strings.Virtual_String := "Usage: ";

      begin
         Usage.Append (Executable);

         if not Self.Defined_Named_Options_List.Is_Empty then
            Usage.Append (" [options]");
         end if;

         for Option of Self.Defined_Positional_Options loop
            Usage.Append (' ');
            Usage.Append (Option.Name);
         end loop;

         Result.Append (Usage);
      end;

      if not Self.Defined_Named_Options_List.Is_Empty then
         Result.Append (VSS.Strings.Empty_Virtual_String);
         Result.Append ("Options:");

         for Option of Self.Defined_Named_Options_List loop
            Option_Text.Clear;
            Option_Text.Append ("  ");

            if not Option.Short_Name.Is_Empty then
               Option_Text.Append (Short_Prefix);
               Option_Text.Append (Option.Short_Name);

               if not Option.Long_Name.Is_Empty then
                  Option_Text.Append (", ");
               end if;
            end if;

            if not Option.Long_Name.Is_Empty then
               Option_Text.Append (Long_Prefix);
               Option_Text.Append (Option.Long_Name);
            end if;

            if Option in Value_Option'Class then
               Option_Text.Append (" <");
               Option_Text.Append (Value_Option'Class (Option).Value_Name);
               Option_Text.Append (">");
            end if;

            Append_Option_Description (Option_Text, Option.Description);
         end loop;
      end if;

      if not Self.Defined_Positional_Options.Is_Empty then
         Result.Append (VSS.Strings.Empty_Virtual_String);
         Result.Append ("Arguments:");

         for Option of Self.Defined_Positional_Options loop
            Option_Text.Clear;
            Option_Text.Append ("  ");
            Option_Text.Append (Option.Name);

            Append_Option_Description (Option_Text, Option.Description);
         end loop;
      end if;

      return Result;
   end Help_Text;

   ------------------
   -- Is_Specified --
   ------------------

   function Is_Specified
     (Self   : Command_Line_Parser'Class;
      Option : Abstract_Option'Class) return Boolean is
   begin
      if Option in Named_Option'Class then
         return
           Self.Known_Named_Options_Values.Contains
             (Named_Option'Class (Option).Unique_Name);

      else
         return
           Self.Defined_Positional_Options.Find_Index
             (Positional_Option'Class (Option))
                <= Self.Positional_Options_Values.Length;
      end if;
   end Is_Specified;

   ------------
   -- Length --
   ------------

   function Length
     (Item : VSS.Strings.Virtual_String)
      return VSS.Strings.Grapheme_Cluster_Count
   is
      Iterator :
        VSS.Strings.Grapheme_Cluster_Iterators.Grapheme_Cluster_Iterator
          := Item.Before_First_Grapheme_Cluster;
      Result   : VSS.Strings.Grapheme_Cluster_Count := 0;

   begin
      while Iterator.Forward loop
         Result := @ + 1;
      end loop;

      return Result;
   end Length;

   -----------
   -- Parse --
   -----------

   function Parse
     (Self      : in out Command_Line_Parser'Class;
      Arguments : VSS.String_Vectors.Virtual_String_Vector) return Boolean
   is
      Index   : Positive := 1;
      Success : Boolean  := True;

   begin
      Self.Error_Message.Clear;
      Self.Known_Named_Options_Values.Clear;
      Self.Unknown_Named_Options_Values.Clear;
      Self.Positional_Options_Values.Clear;
      Self.Only_Positional := False;

      while Index <= Arguments.Length loop
         Self.Parse_Argument (Arguments, Index, Success);

         exit when not Success;

         Index := Index + 1;
      end loop;

      return Success;
   end Parse;

   --------------------
   -- Parse_Argument --
   --------------------

   procedure Parse_Argument
     (Self      : in out Command_Line_Parser'Class;
      Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Index     : in out Positive;
      Success   : in out Boolean)
   is
      use type VSS.Characters.Virtual_Character;
      use type VSS.Strings.Virtual_String;

      procedure Parse_Long_Argument;

      procedure Parse_Short_Argument;

      procedure Append_Named_Argument
        (Name  : VSS.Strings.Virtual_String;
         Value : VSS.Strings.Virtual_String);

      Argument : VSS.Strings.Virtual_String := Arguments (Index);

      ---------------------------
      -- Append_Named_Argument --
      ---------------------------

      procedure Append_Named_Argument
        (Name  : VSS.Strings.Virtual_String;
         Value : VSS.Strings.Virtual_String) is
      begin
         if Name.Is_Empty then
            Self.Error_Message := "empty option name";
            Success            := False;

            return;
         end if;

         if Self.Defined_Named_Options.Contains (Name) then
            declare
               Option : constant Named_Option'Class :=
                 Self.Defined_Named_Options (Name);

            begin
               if Option in Binary_Option'Class then
                  --  Value of the binary option is empty always.

                  pragma Assert (Value.Is_Empty);

               elsif Option in Value_Option'Class then
                  --  Named option with value, may value be empty?

                  null;

               elsif Option in Name_Value_Option'Class then
                  --  Named option of "name=value", may value be empty?

                  null;

               else
                  raise Program_Error;
               end if;

               if not Self.Known_Named_Options_Values.Contains
                 (Option.Unique_Name)
               then
                  Self.Known_Named_Options_Values.Insert
                    (Option.Unique_Name,
                     VSS.String_Vectors.Empty_Virtual_String_Vector);
               end if;

               Self.Known_Named_Options_Values
                 (Option.Unique_Name).Append (Value);
            end;

         else
            if not Self.Unknown_Named_Options_Values.Contains (Name) then
               Self.Unknown_Named_Options_Values.Insert
                 (Name, VSS.String_Vectors.Empty_Virtual_String_Vector);
            end if;

            Self.Unknown_Named_Options_Values (Name).Append (Value);
         end if;
      end Append_Named_Argument;

      -------------------------
      -- Parse_Long_Argument --
      -------------------------

      procedure Parse_Long_Argument is
         Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
           Argument.At_First_Character;

      begin
         while Iterator.Forward loop
            exit when Iterator.Element = VSS.Characters.Latin.Equals_Sign;
         end loop;

         if Iterator.Has_Element then
            if Self.Defined_Named_Options.Contains
              (Argument.Head_Before (Iterator))
            then
               --  Binary option doesn't support '--name=value' format.

               declare
                  Option : constant Named_Option'Class :=
                    Self.Defined_Named_Options
                      (Argument.Head_Before (Iterator));

               begin
                  if Option in Binary_Option'Class then
                     Self.Error_Message := "binary option can't have value";
                     Success            := False;

                     return;
                  end if;
               end;
            end if;

            Append_Named_Argument
              (Argument.Head_Before (Iterator),
               Argument.Tail_After (Iterator));

         elsif Self.Defined_Named_Options.Contains (Argument) then
            declare
               Option : constant Named_Option'Class :=
                 Self.Defined_Named_Options (Argument);

            begin
               if Option in Binary_Option'Class then
                  Append_Named_Argument
                    (Argument, VSS.Strings.Empty_Virtual_String);

               else
                  Index := Index + 1;
                  Append_Named_Argument (Argument, Arguments (Index));
                  --  Out-of-range of Index not need to be checked here,
                  --  Virtual_String_Vector returns empty string when index
                  --  is out of range.
               end if;
            end;

         else
            Append_Named_Argument (Argument, VSS.Strings.Empty_Virtual_String);
         end if;
      end Parse_Long_Argument;

      --------------------------
      -- Parse_Short_Argument --
      --------------------------

      procedure Parse_Short_Argument is
      begin
         while not Argument.Is_Empty loop
            declare
               Name : constant VSS.Strings.Virtual_String :=
                 Argument.Slice (Argument.At_First_Grapheme_Cluster);

            begin
               Remove_Prefix (Argument, Name);

               if not Argument.Is_Empty
                 and then Argument.At_First_Character.Element
                   = VSS.Characters.Latin.Equals_Sign
               then
                  if Self.Defined_Named_Options.Contains (Name) then
                     --  Binary option doesn't support '--N=value' format.

                     declare
                        Option : constant Named_Option'Class :=
                          Self.Defined_Named_Options (Name);

                     begin
                        if Option in Binary_Option'Class then
                           Self.Error_Message :=
                             "binary option can't have value";
                           Success            := False;

                           return;
                        end if;
                     end;
                  end if;

                  Append_Named_Argument
                    (Name, Argument.Tail_After (Argument.At_First_Character));
                  Argument.Clear;

               elsif Self.Defined_Named_Options.Contains (Name) then
                  declare
                     Option : constant Named_Option'Class :=
                       Self.Defined_Named_Options (Name);

                  begin
                     if Option in Binary_Option'Class then
                        Append_Named_Argument
                          (Name, VSS.Strings.Empty_Virtual_String);

                     else
                        if Argument.Is_Empty then
                           Index := Index + 1;
                           Append_Named_Argument (Name, Arguments (Index));
                           --  Out-of-range of Index not need to be checked
                           --  here, Virtual_String_Vector returns empty
                           --  string when index is out of range.

                        else
                           Append_Named_Argument (Name, Argument);
                           Argument.Clear;
                        end if;
                     end if;
                  end;

               else
                  Append_Named_Argument
                    (Name, VSS.Strings.Empty_Virtual_String);
               end if;
            end;
         end loop;
      end Parse_Short_Argument;

   begin
      if not Self.Only_Positional
        and then Argument.Starts_With (Long_Prefix)
      then
         if Argument = Long_Prefix then
            --  '--' is processed as end of switches, only positional
            --  arguments are allowed.

            Self.Only_Positional := True;

         else
            Remove_Prefix (Argument, Long_Prefix);
            Parse_Long_Argument;
         end if;

      elsif not Self.Only_Positional
        and then Argument.Starts_With (Short_Prefix)
        and then Argument /= Short_Prefix
      then
         Remove_Prefix (Argument, Short_Prefix);
         Parse_Short_Argument;

      else
         if Self.Defined_Positional_Options.Is_Empty then
            Self.Error_Message := "unexpected positional argument";
            Success            := False;

            return;
         end if;

         Self.Positional_Options_Values.Append (Argument);
      end if;
   end Parse_Argument;

   --------------------------
   -- Positional_Arguments --
   --------------------------

   function Positional_Arguments
     (Self : Command_Line_Parser'Class)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Self.Positional_Options_Values;
   end Positional_Arguments;

   -------------------
   -- Remove_Prefix --
   -------------------

   procedure Remove_Prefix
     (Item   : in out VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String)
   is
      Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
        Item.Before_First_Character;

   begin
      pragma Assert (Item.Starts_With (Prefix));

      for J in 1 .. Prefix.Character_Length loop
         exit when not Iterator.Forward;
      end loop;

      Item := Item.Tail_After (Iterator);
   end Remove_Prefix;

   ------------------------------
   -- Unknown_Option_Arguments --
   ------------------------------

   function Unknown_Option_Arguments
     (Self : Command_Line_Parser'Class)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for Position in Self.Unknown_Named_Options_Values.Iterate loop
            Result.Append (Named_Value_Maps.Key (Position));
         end loop;
      end return;
   end Unknown_Option_Arguments;

   -----------
   -- Value --
   -----------

   function Value
     (Self   : Command_Line_Parser'Class;
      Option : Positional_Option'Class) return VSS.Strings.Virtual_String is
   begin
      return
        Self.Positional_Options_Values
          (Self.Defined_Positional_Options.Find_Index (Option));
   end Value;

   -----------
   -- Value --
   -----------

   function Value
     (Self   : Command_Line_Parser'Class;
      Option : Value_Option'Class) return VSS.Strings.Virtual_String is
   begin
      if Self.Known_Named_Options_Values.Contains (Option.Unique_Name) then
         return Self.Known_Named_Options_Values (Option.Unique_Name) (1);

      elsif Self.Unknown_Named_Options_Values.Contains
        (Option.Unique_Name)
      then
         return Self.Unknown_Named_Options_Values (Option.Unique_Name) (1);

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Value;

   ------------
   -- Values --
   ------------

   function Values
     (Self   : Command_Line_Parser'Class;
      Option : Value_Option'Class)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Self.Known_Named_Options_Values (Option.Unique_Name);
   end Values;

   ------------
   -- Values --
   ------------

   function Values
     (Self   : Command_Line_Parser'Class;
      Option : Name_Value_Option'Class) return Name_Value_Vectors.Vector
   is
      use type VSS.Characters.Virtual_Character;

      procedure Parse
        (Result : in out Name_Value_Vectors.Vector;
         Map    : VSS.String_Vectors.Virtual_String_Vector);

      -----------
      -- Parse --
      -----------

      procedure Parse
        (Result : in out Name_Value_Vectors.Vector;
         Map    : VSS.String_Vectors.Virtual_String_Vector) is
      begin
         for Image of Map loop
            declare
               Iterator :
               VSS.Strings.Character_Iterators.Character_Iterator :=
                 Image.At_First_Character;

            begin
               while Iterator.Forward loop
                  exit when Iterator.Element
                    = VSS.Characters.Latin.Equals_Sign;
               end loop;

               Result.Append
                 (Name_Value'
                  (Name  => Image.Head_Before (Iterator),
                   Value => Image.Tail_After (Iterator)));
            end;
         end loop;
      end Parse;

   begin
      return Result : Name_Value_Vectors.Vector do
         if Self.Known_Named_Options_Values.Contains (Option.Unique_Name) then
            Parse
              (Result, Self.Known_Named_Options_Values (Option.Unique_Name));

         elsif Self.Unknown_Named_Options_Values.Contains
                 (Option.Unique_Name)
         then
            Parse
              (Result, Self.Unknown_Named_Options_Values (Option.Unique_Name));
         end if;
      end return;
   end Values;

end VSS.Command_Line.Parsers;
