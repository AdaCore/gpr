with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNATCOLL.Utils;

package body Test_Helper is

   -----------
   -- Image --
   -----------

   function Image (Command : Argument_List) return String
   is
      Result : Unbounded_String;
      Quote  : constant Character := '"';
   begin
      for Arg of Command loop
         if Length (Result) > 0 then
            Append (Result, " ");
         end if;

         if Ada.Strings.Fixed.Index (Arg, " ") > 0 then
            Append (Result, Quote);
            Append (Result, Arg);
            Append (Result, Quote);
         else
            Append (Result, Arg);
         end if;
      end loop;

      return To_String (Result);
   end Image;

   --------------
   -- Image_RF --
   --------------

   function Image_RF (Command : Argument_List) return String
   is
      Result : Unbounded_String;
      First  : Boolean := True;
   begin
      for Arg of Command loop
         if First then
            declare
               Res     : constant GNATCOLL.Utils.Unbounded_String_Array :=
                           GNATCOLL.Utils.Split
                             (Str => Arg,
                              On  => GNAT.OS_Lib.Directory_Separator);
               RF_Name : constant String := To_String (Res (Res'Last));
            begin
               Append
                 (Result, "Response file @" & RF_Name & " : {");
            end;
            First := False;
         else
            if GNATCOLL.Utils.Ends_With (Arg, "" & ASCII.LF) then
               Append (Result, Arg (Arg'First .. Arg'Last - 1) & "<LF>");
            else
               Append (Result, Arg);
            end if;
         end if;
      end loop;

      if not Command.Is_Empty then
         Append (Result, "}");
      end if;

      return To_String (Result);
   end Image_RF;

   -------------------
   -- New_Test_Case --
   -------------------

   procedure New_Test_Case (Message : String := "") is
   begin
      Ada.Text_IO.Put ("[ Test case" & Test_Cases'Img & " ]");
      if Message /= "" then
         Ada.Text_IO.Put (" - [ " & Message & " ]");
      end if;
      Ada.Text_IO.New_Line;
      Test_Cases := Test_Cases + 1;
   end New_Test_Case;

end Test_Helper;
