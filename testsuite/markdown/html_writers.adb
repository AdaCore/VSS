------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Wide_Wide_Text_IO;
with Ada.Integer_Wide_Wide_Text_IO;

with VSS.Characters;
with VSS.Strings.Conversions;
with VSS.Strings.Cursors.Iterators.Characters;

package body HTML_Writers is

   function "+"
     (Item : VSS.Strings.Virtual_String'Class) return Wide_Wide_String
   renames VSS.Strings.Conversions.To_Wide_Wide_String;

   procedure Close_Tag (Self : in out Writer'Class);

   function Escape
    (Text       : VSS.Strings.Virtual_String;
     Escape_All : Boolean := False) return VSS.Strings.Virtual_String;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Self : in out Writer; Text : VSS.Strings.Virtual_String)
   is
      Value : constant VSS.Strings.Virtual_String :=
        (if Self.CDATA then Text else Escape (Text));
   begin
      Self.Close_Tag;

      Ada.Wide_Wide_Text_IO.Put (+Value);
   end Characters;

   ---------------
   -- Close_Tag --
   ---------------

   procedure Close_Tag (Self : in out Writer'Class) is
   begin
      if not Self.Tag.Is_Empty then
         Ada.Wide_Wide_Text_IO.Put (">");
         Self.Tag.Clear;
      end if;
   end Close_Tag;
   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Self : in out Writer; Local_Name : VSS.Strings.Virtual_String)
   is
      use type VSS.Strings.Virtual_String;
      use type VSS.Strings.Character_Count;
   begin
      if Self.Tag = Local_Name and then
        not (Self.Tag = "code" or else
             Self.Tag = "html" or else
             Self.Tag = "a" or else
             Self.Tag =  "li")
        and then (Self.Tag.Character_Length = 1 or else
                   Self.Tag.Last_Character.Element not in '1' .. '9')
      then
         Ada.Wide_Wide_Text_IO.Put ("/>");
         Self.Tag.Clear;
      else
         Self.Close_Tag;
         Ada.Wide_Wide_Text_IO.Put ("</");
         Ada.Wide_Wide_Text_IO.Put (+Local_Name);
         Ada.Wide_Wide_Text_IO.Put (">");
      end if;

      if Local_Name.Starts_With ("h") or else Local_Name = "p" then
         Ada.Wide_Wide_Text_IO.New_Line;
      end if;
   end End_Element;

   ------------
   -- Escape --
   ------------

   function Escape
    (Text       : VSS.Strings.Virtual_String;
     Escape_All : Boolean := False)
       return VSS.Strings.Virtual_String
   is
      Result : VSS.Strings.Virtual_String;
      Cursor : VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
        Text.First_Character;
   begin
      if Cursor.Has_Element then
         loop
            case Cursor.Element is
               when '&' =>
                  Result.Append ("&amp;");

               when '"' =>
                  if Escape_All then
                     Result.Append ("%22");
                  else
                     Result.Append ("&quot;");
                  end if;

               when '>' =>
                  Result.Append ("&gt;");

               when '<' =>
                  Result.Append ("&lt;");

               when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' |
                  '-' | '_' | '.' | '~' | '/' | '@' | '+' | ',' |
                  '(' | ')' | '#' | '?' | '=' | ':' | '*'
                  =>

                  Result.Append (Cursor.Element);

               when others =>

                  if Escape_All or
                    VSS.Characters.Virtual_Character'Pos (Cursor.Element) in
                      16#1#  .. 16#8#
                    | 16#B#  .. 16#C#
                    | 16#E#  .. 16#1F#
                    | 16#7F#
                  then
                     declare
                        Image : Wide_Wide_String (1 .. 7);  --  -#16#xx#

                     begin
                        Ada.Integer_Wide_Wide_Text_IO.Put
                          (To   => Image,
                           Item => VSS.Characters.Virtual_Character'Pos
                                     (Cursor.Element),
                           Base => 16);
                        Result.Append ("%");
                        Result.Append
                          (VSS.Strings.To_Virtual_String (Image (5 .. 6)));
                     end;
                  else
                     Result.Append (Cursor.Element);
                  end if;
            end case;

            exit when not Cursor.Forward;
         end loop;
      end if;

      return Result;
   end Escape;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Self       : in out Writer; Local_Name : VSS.Strings.Virtual_String;
      Attributes :        HTML_Attributes'Class := No_Attributes)
   is
      use type VSS.Strings.Virtual_String;
   begin
      Self.Close_Tag;
      Ada.Wide_Wide_Text_IO.Put ("<");
      Ada.Wide_Wide_Text_IO.Put (+Local_Name);

      if Local_Name = "hr" then
         Ada.Wide_Wide_Text_IO.Put (" ");
      end if;

      for Attribute of Attributes loop
         Ada.Wide_Wide_Text_IO.Put (" ");
         Ada.Wide_Wide_Text_IO.Put (+Attribute.Name);
         Ada.Wide_Wide_Text_IO.Put ("=""");

         if Attribute.Name = "href" then
            Ada.Wide_Wide_Text_IO.Put (+Escape (Attribute.Value, True));
         elsif Attribute.Name = "class" then
            Ada.Wide_Wide_Text_IO.Put (+Attribute.Value);
         else
            Ada.Wide_Wide_Text_IO.Put (+Escape (Attribute.Value, False));
         end if;

         Ada.Wide_Wide_Text_IO.Put ("""");
      end loop;

      Self.Tag := Local_Name;
   end Start_Element;

end HTML_Writers;
