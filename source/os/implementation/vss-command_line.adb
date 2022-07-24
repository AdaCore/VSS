--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Wide_Wide_Text_IO;

with GNAT.OS_Lib;

with VSS.Application;
with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
pragma Warnings
  (Off, "unit ""VSS.Strings.Grapheme_Cluster_Iterators"" is not referenced");
--  GNAT Pro 20220609 reports unreferenced unit, while it is necessary
--  to be able to use grapheme cluster iterator as parameter of the
--  Virtual_String.Slice subprogram.
with VSS.Strings.Grapheme_Cluster_Iterators;
with VSS.Strings.Hash;

package body VSS.Command_Line is

   use type VSS.Characters.Virtual_Character;

   function Parse
     (Arguments : VSS.String_Vectors.Virtual_String_Vector) return Boolean;

   procedure Remove_Prefix
     (Item   : in out VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String);

   procedure Parse_Argument
     (Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Index     : in out Positive;
      Success   : in out Boolean);

   function Unique_Name
     (Self : Named_Option'Class) return VSS.Strings.Virtual_String;

   procedure Output_Error (Message : VSS.Strings.Virtual_String);
   --  Outputs error message to the standard error.

   package Platform is

      procedure Report_Error (Message : VSS.Strings.Virtual_String);
      --  Report error in platform specific way. On POSIX systems, error is
      --  output to standard error stream; on Windows application console is
      --  used when available, otherwise, if standard error stream is defined
      --  the message is output to it, and otherwise message box with the
      --  messase text is displayed.

   end Platform;

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

   Short_Prefix : constant VSS.Strings.Virtual_String := "-";
   Long_Prefix  : constant VSS.Strings.Virtual_String := "--";

   Defined_Short_Options      : Name_Sets.Set;
   Defined_Long_Options       : Name_Sets.Set;
   Defined_Named_Options      : Named_Option_Maps.Map;
   Defined_Positional_Options : Positional_Option_Vectors.Vector;

   Only_Positional : Boolean := False;
   --  All following arguments are positional arguments.
   Error_Message   : VSS.Strings.Virtual_String;

   Named_Options_Values           : Named_Value_Maps.Map;
   Undefined_Named_Options_Values : Named_Value_Maps.Map;
   Positional_Options_Values      : VSS.String_Vectors.Virtual_String_Vector;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Option : Abstract_Option'Class) is
   begin
      if Option in Named_Option'Class then
         if not Named_Option'Class (Option).Short_Name.Is_Empty then
            Defined_Short_Options.Insert
              (Named_Option'Class (Option).Short_Name);
            Defined_Named_Options.Insert
              (Named_Option'Class (Option).Short_Name,
               Named_Option'Class (Option));
         end if;

         if not Named_Option'Class (Option).Long_Name.Is_Empty then
            Defined_Long_Options.Insert
              (Named_Option'Class (Option).Long_Name);
            Defined_Named_Options.Insert
              (Named_Option'Class (Option).Long_Name,
               Named_Option'Class (Option));
         end if;

      else
         Defined_Positional_Options.Append (Positional_Option'Class (Option));
      end if;
   end Add_Option;

   ------------------
   -- Is_Specified --
   ------------------

   function Is_Specified (Option : Abstract_Option'Class) return Boolean is
   begin
      if Option in Named_Option'Class then
         return
           Named_Options_Values.Contains
             (Named_Option'Class (Option).Unique_Name);

      else
         raise Program_Error;
      end if;
   end Is_Specified;

   ------------------
   -- Output_Error --
   ------------------

   procedure Output_Error (Message : VSS.Strings.Virtual_String) is
   begin
      --  ??? VSS doesn't provide IO implementation for standard streams, so
      --  use Ada.Wide_Wide_Text_IO as fallback, while it is known that it may
      --  creates an issues like crash on Windows when nor standard error nor
      --  console is not available, or incorrect encodings on POSIX systems.

      Ada.Wide_Wide_Text_IO.Put_Line
        (Ada.Wide_Wide_Text_IO.Standard_Error,
         VSS.Strings.Conversions.To_Wide_Wide_String (Message));
   end Output_Error;

   -----------
   -- Parse --
   -----------

   function Parse
     (Arguments : VSS.String_Vectors.Virtual_String_Vector) return Boolean
   is
      Index   : Positive := 1;
      Success : Boolean  := True;

   begin
      while Index <= Arguments.Length loop
         Parse_Argument (Arguments, Index, Success);

         exit when not Success;

         Index := Index + 1;
      end loop;

      return Success;
   end Parse;

   --------------------
   -- Parse_Argument --
   --------------------

   procedure Parse_Argument
     (Arguments : VSS.String_Vectors.Virtual_String_Vector;
      Index     : in out Positive;
      Success   : in out Boolean)
   is
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
            Error_Message := "empty option name";
            Success       := False;

            return;
         end if;

         if Defined_Named_Options.Contains (Name) then
            declare
               Option : constant Named_Option'Class :=
                 Defined_Named_Options (Name);

            begin
               if Option in Binary_Option then
                  raise Program_Error;

               elsif Option in Value_Option'Class then
                  --  Named option with value, may value be empty?

                  null;

               elsif Option in Name_Value_Option'Class then
                  --  Named option of "name=value", may value be empty?

                  null;

               else
                  raise Program_Error;
               end if;

               if not Named_Options_Values.Contains (Option.Unique_Name) then
                  Named_Options_Values.Insert
                    (Option.Unique_Name,
                     VSS.String_Vectors.Empty_Virtual_String_Vector);
               end if;

               Named_Options_Values (Option.Unique_Name).Append (Value);
            end;

         else
            if not Undefined_Named_Options_Values.Contains (Name) then
               Undefined_Named_Options_Values.Insert
                 (Name, VSS.String_Vectors.Empty_Virtual_String_Vector);
            end if;

            Undefined_Named_Options_Values (Name).Append (Value);
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
            Append_Named_Argument
              (Argument.Head_Before (Iterator),
               Argument.Tail_After (Iterator));

         elsif Defined_Named_Options.Contains (Argument) then
            declare
               Option : constant Named_Option'Class :=
                 Defined_Named_Options (Argument);

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
                  Append_Named_Argument
                    (Name, Argument.Tail_After (Argument.At_First_Character));
                  Argument.Clear;

               elsif Defined_Named_Options.Contains (Name) then
                  declare
                     Option : constant Named_Option'Class :=
                       Defined_Named_Options (Name);

                  begin
                     if Option in Binary_Option'Class then
                        raise Program_Error;

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
      if not Only_Positional
        and then Argument.Starts_With (Long_Prefix)
      then
         if Argument = Long_Prefix then
            --  '--' is processed as end of switches, only positional
            --  arguments are allowed.

            Only_Positional := True;

         else
            Remove_Prefix (Argument, Long_Prefix);
            Parse_Long_Argument;
         end if;

      elsif  not Only_Positional
        and then Argument.Starts_With (Short_Prefix)
        and then Argument /= Short_Prefix
      then
         Remove_Prefix (Argument, Short_Prefix);
         Parse_Short_Argument;

      else
         if Defined_Positional_Options.Is_Empty then
            Error_Message := "unexpected positional argument";
            Success       := False;

            return;
         end if;

         Positional_Options_Values.Append (Argument);
      end if;
   end Parse_Argument;

   --------------
   -- Platform --
   --------------

   package body Platform is separate;

   --------------------------
   -- Positional_Arguments --
   --------------------------

   function Positional_Arguments
     return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Positional_Options_Values;
   end Positional_Arguments;

   -------------
   -- Process --
   -------------

   procedure Process is
   begin
      if not Parse (VSS.Application.Arguments) then
         Report_Error (Error_Message);
      end if;

      if not Undefined_Named_Options_Values.Is_Empty then
         Report_Error ("unknown option");
      end if;
   end Process;

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

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Message : VSS.Strings.Virtual_String) is
   begin
      Platform.Report_Error (Message);
      GNAT.OS_Lib.OS_Exit (1);
   end Report_Error;

   -----------------
   -- Unique_Name --
   -----------------

   function Unique_Name
     (Self : Named_Option'Class) return VSS.Strings.Virtual_String is
   begin
      if Self.Short_Name.Is_Empty then
         return Self.Long_Name;

      else
         return Self.Short_Name;
      end if;
   end Unique_Name;

   -----------
   -- Value --
   -----------

   function Value
     (Option : Value_Option'Class) return VSS.Strings.Virtual_String is
   begin
      if Named_Options_Values.Contains (Option.Unique_Name) then
         return Named_Options_Values (Option.Unique_Name) (1);

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Value;

   ------------
   -- Values --
   ------------

   function Values
     (Option : Name_Value_Option'Class) return Name_Value_Vectors.Vector is
   begin
      return Result : Name_Value_Vectors.Vector do
         if Named_Options_Values.Contains (Option.Unique_Name) then
            for Image of Named_Options_Values (Option.Unique_Name) loop
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
                    ((Image.Head_Before (Iterator),
                      Image.Tail_After (Iterator)));
               end;
            end loop;
         end if;
      end return;
   end Values;

end VSS.Command_Line;
