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

package body VSS.Implementation.Markdown.Paragraphs is

   -----------------
   -- Append_Line --
   -----------------

   overriding procedure Append_Line
     (Self  : in out Paragraph;
      Input : Input_Position;
      CIP   : Can_Interrupt_Paragraph;
      Ok    : in out Boolean)
   is
   begin
      Ok := Input.First.Has_Element and not CIP;

      if Ok then
         Self.Lines.Append (Input.Line.Unexpanded_Tail (Input.First));
      end if;
   end Append_Line;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Input : not null access Input_Position) return Paragraph
   is
   begin
      return Result : Paragraph do
         Result.Lines.Append (Input.Line.Unexpanded_Tail (Input.First));
         --  Shift Input.First to EOL
         while Input.First.Forward loop
            null;
         end loop;
      end return;
   end Create;

   --------------
   -- Detector --
   --------------

   procedure Detector
     (Line : Input_Position;
      Tag  : in out Ada.Tags.Tag;
      CIP  : out Can_Interrupt_Paragraph)
   is
   begin
      if Line.First.Has_Element then  --  FIXME: use Blank_Pattern here
         Tag := Paragraph'Tag;
         CIP := False;
      end if;
   end Detector;

   ----------
   -- Text --
   ----------

   function Text (Self : Paragraph)
     return VSS.Markdown.Annotations.Annotated_Text
   is
      First : Boolean := True;
   begin
      return Result : VSS.Markdown.Annotations.Annotated_Text do
         for Line of Self.Lines loop
            if First then
               First := False;
            else
               Result.Plain_Text.Append (' ');
            end if;

            Result.Plain_Text.Append (Line);
         end loop;
      end return;
   end Text;

end VSS.Implementation.Markdown.Paragraphs;
