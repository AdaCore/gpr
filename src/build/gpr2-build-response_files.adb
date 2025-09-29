--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Response_Files is

   function Format (Arg : String) return String;

   procedure Write
     (FD     : GNATCOLL.OS.FS.File_Descriptor;
      Vector : in out Unbounded_String;
      Buffer : String);

   procedure New_Line
     (FD     : GNATCOLL.OS.FS.File_Descriptor;
      Vector : in out Unbounded_String);

   -----------
   -- Close --
   -----------

   procedure Close (Self : in out Object) is
   begin
      if Self.Has_Primary_Response_File then
         GOF.Close (Self.Primary_FD);
      end if;

      if Self.Has_Secondary_Response_File then
         GOF.Close (Self.Secondary_FD);
      end if;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self      : in out Object;
      Cmd_Line  : in out GPR2.Build.Command_Line.Object) is
   begin
      case Self.Kind is
         when Binder =>
            Self.Create_Binder (Cmd_Line);
         when Compiler =>
            Self.Create_Compiler (Cmd_Line);
         when Linker =>
            Self.Create_Linker (Cmd_Line);
         when Unknown =>
            null;
      end case;

      Self.Close;
   end Create;

   -------------------
   -- Create_Binder --
   -------------------

   procedure Create_Binder
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
   is
      function Format_Binder_Arg (Arg : String) return String;

      function Format_Binder_Arg (Arg : String) return String
      is
         Char          : Character;
         Quotes_Needed : Boolean := False;
      begin
         for Index in Arg'Range loop
            Char := Arg (Index);

            if Char = ' '
              or else Char = ASCII.HT
              or else Char = '"'
            then
               Quotes_Needed := True;
               exit;
            end if;
         end loop;

         if Quotes_Needed then
            declare
               New_Arg : String (1 .. Arg'Length * 2 + 2);
               Offset  : Integer := 0;
            begin
               New_Arg (1) := '"';
               Offset := Offset + 1;

               for Index in Arg'Range loop
                  Char := Arg (Index);
                  New_Arg (Index + Offset) := Char;

                  if Char = '"' then
                     Offset := Offset + 1;
                     New_Arg (Index + Offset) := '"';
                  end if;
               end loop;

               Offset := Offset + 1;
               New_Arg (Arg'Length + Offset) := '"';

               return New_Arg (1 .. Arg'Length + Offset);
            end;
         end if;

         return Arg;
      end Format_Binder_Arg;

      First : Boolean := True;
   begin
      for Arg of Cmd_Line.Argument_List loop
         if First then
            First := False;
         else
            Write
              (Self.Primary_FD, Self.Primary_Content, Format_Binder_Arg (Arg));
            New_Line (Self.Primary_FD, Self.Primary_Content);
         end if;
      end loop;

      --  Recompute the command line with the response file
      Cmd_Line.Recompute_For_Response_File
        (True,
         "@" & Self.Primary_Path.String_Value);
   end Create_Binder;

   ---------------------
   -- Create_Compiler --
   ---------------------

   procedure Create_Compiler
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
   is
      First : Boolean := True;
   begin
      for Arg of Cmd_Line.Argument_List loop
         if First then
            First := False;
         else
            Write
              (Self.Primary_FD, Self.Primary_Content, Format (Arg));
            New_Line (Self.Primary_FD, Self.Primary_Content);
         end if;
      end loop;

      --  Recompute the command line with the response file
      Cmd_Line.Recompute_For_Response_File
        (True,
         "@" & Self.Primary_Path.String_Value);
   end Create_Compiler;

   -------------------
   -- Create_Linker --
   -------------------

   procedure Create_Linker
     (Self      : in out Object;
      Cmd_Line  : in out GPR2.Build.Command_Line.Object)
   is
      GNU_Info_Needed   : constant Boolean := Self.Format in GNU | GCC_GNU;
      GNU_Header        : constant String := "INPUT (";
      GNU_Opening       : constant String := """";
      GNU_Closing       : constant String := GNU_Opening;
      GNU_Footer        : constant String := ")";
      GCC_Info          : constant Boolean :=
                           Self.Format in GCC_Formatting_Required;
      GNU_Archiver_Info : constant Boolean := Self.Format in GNU_Archiver;
      Resp_File_Prefix  : constant String :=
                            (if GCC_Info or else GNU_Archiver_Info
                             then "@"
                             else
                               (if Self.Resp_File_Switches.Is_Empty
                                then ""
                                else Self.Resp_File_Switches.Last_Element));
   begin
      if GNU_Info_Needed then
         Write (Self.Primary_FD, Self.Primary_Content, GNU_Header);
      end if;

      for Obj of Cmd_Line.Argument_List (Build.Command_Line.Obj) loop
         if GNU_Info_Needed then
            Write (Self.Primary_FD, Self.Primary_Content, GNU_Opening);
            Write (Self.Primary_FD, Self.Primary_Content, Format (Obj));
            Write (Self.Primary_FD, Self.Primary_Content, GNU_Closing);

         else
            Write (Self.Primary_FD, Self.Primary_Content, Format (Obj));
         end if;

         New_Line (Self.Primary_FD, Self.Primary_Content);
      end loop;

      if GNU_Info_Needed then
         Write (Self.Primary_FD, Self.Primary_Content, GNU_Footer);
         New_Line (Self.Primary_FD, Self.Primary_Content);
      end if;

      if GCC_Info and then Self.Has_Secondary_Response_File then
         for Switch of Self.Resp_File_Switches loop
            Write (Self.Secondary_FD, Self.Secondary_Content, Switch);

            if Switch /= Self.Resp_File_Switches.Last_Element then
               New_Line (Self.Secondary_FD, Self.Secondary_Content);
            end if;
         end loop;

         Write
           (Self.Secondary_FD,
            Self.Secondary_Content,
            Format (Self.Primary_Path.String_Value));
         New_Line (Self.Secondary_FD, Self.Secondary_Content);

         for Other of Cmd_Line.Argument_List (Build.Command_Line.Other) loop
            Write
              (Self.Secondary_FD,
               Self.Secondary_Content,
               Format (Other));
            New_Line (Self.Secondary_FD, Self.Secondary_Content);
         end loop;
      end if;

      --  Recompute the command line depending on the response file
      Cmd_Line.Recompute_For_Response_File
        (Clear_Other   => Self.Has_Secondary_Response_File,
         Resp_File_Arg =>
           (if not Self.Has_Secondary_Response_File
            then Resp_File_Prefix & Self.Primary_Path.String_Value
            else Resp_File_Prefix & Self.Secondary_Path.String_Value));
   end Create_Linker;

   ------------
   -- Format --
   ------------

   function Format (Arg : String) return String is
      Char    : Character;
      Tmp_Arg : String (1 .. Arg'Length) := Arg (Arg'First .. Arg'Last);
      New_Arg : String (1 .. Arg'Length * 2);
      Offset  : Integer := 0;
   begin
      for Index in Tmp_Arg'Range loop
         Char := Tmp_Arg (Index);

         if Char = ' '
           or else Char = ASCII.HT
           or else Char = '"'
           or else Char = '\'
         then
            New_Arg (Index + Offset) := '\';
            Offset := Offset + 1;
            New_Arg (Index + Offset) := Char;
         else
            New_Arg (Index + Offset) := Char;
         end if;
      end loop;

      return New_Arg (1 .. Arg'Length + Offset);
   end Format;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Object;
      Format     : Response_File_Format;
      Kind       : Response_File_Kind;
      Switches   : Containers.Source_Value_List) is
   begin
      Self.Format              := Format;
      Self.Kind                := Kind;
      for Val of Switches loop
         Self.Resp_File_Switches.Append (Val.Text);
      end loop;
   end Initialize;

   --------------
   -- New_Line --
   --------------

   procedure New_Line
     (FD     : GNATCOLL.OS.FS.File_Descriptor;
      Vector : in out Unbounded_String) is
   begin
      GOF.Write (FD, "" & ASCII.LF);
      Append (Vector, "<LF>");
   end New_Line;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self      : in out Object;
      FD        : GNATCOLL.OS.FS.File_Descriptor;
      Path      : Filename_Type;
      Secondary : Boolean := False) is
   begin
      if Secondary then
         Self.Secondary_FD   := FD;
         Self.Secondary_Path := Path_Name.Create_File (Path);
      else
         Self.Primary_FD   := FD;
         Self.Primary_Path := Path_Name.Create_File (Path);
      end if;
   end Register;

   -----------
   -- Write --
   -----------

   procedure Write
     (FD     : GNATCOLL.OS.FS.File_Descriptor;
      Vector : in out Unbounded_String;
      Buffer : String) is
   begin
      if Buffer'Length > 0 then
         GOF.Write (FD, Buffer);
         Append (Vector, Buffer);
      end if;
   end Write;


end GPR2.Build.Response_Files;
