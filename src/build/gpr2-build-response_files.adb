--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Response_Files is

   function Format (Arg : String) return String;

   procedure Write
     (FD     : GNATCOLL.OS.FS.File_Descriptor;
      Vector : in out GNATCOLL.OS.Process.Argument_List;
      Buffer : String);

   ------------------
   -- Check_Length --
   ------------------

   function Check_Length
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object) return Boolean is
   begin

      --  If we have a length restriction, and the current command line length
      --  exceeds that restriction, we should create a response file.
      if Self.Length_Restriction (Cmd_Line) then
         return True;
      end if;

      Self.Has_Primary_Content   := False;
      Self.Has_Secondary_Content := False;

      return False;
   end Check_Length;

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
         when Compiler =>
            Self.Create_Compiler (Cmd_Line);
         when Linker =>
            Self.Create_Linker (Cmd_Line);
         when Unknown =>
            null;
      end case;

      Self.Close;
   end Create;

   ---------------------
   -- Create_Compiler --
   ---------------------

   procedure Create_Compiler
     (Self     : in out Object;
      Cmd_Line : in out GPR2.Build.Command_Line.Object)
   is
      First : Boolean := True;
   begin
      --  Do not create a response file if it would result in a longer command
      --  line length.
      if not Self.Check_Length (Cmd_Line) then
         return;
      end if;

      for Arg of Cmd_Line.Argument_List loop
         if First then
            First := False;
         else
            Write
              (Self.Primary_FD, Self.Primary_Content, Format (Arg) & ASCII.LF);
         end if;
      end loop;

      --  Recompute the command line with the response file
      Cmd_Line.Recompute_For_Response_File
        (True, "@" & (-Self.Primary_Path), Build.Command_Line.All_Args);
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
      GNU_Closing       : constant String := '"' & ASCII.LF;
      GNU_Footer        : constant String := ')' & ASCII.LF;
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
      --  Do not create a response file if it would result in a longer command
      --  line length.
      if not Self.Check_Length (Cmd_Line) then
         return;
      end if;

      --  Recompute the command line depending on the response file
      Cmd_Line.Recompute_For_Response_File
        (GCC_Info or else GNU_Archiver_Info,
         (if Self.Secondary_FD = GOF.Invalid_FD
          then Resp_File_Prefix & (-Self.Primary_Path)
          else Resp_File_Prefix & (-Self.Secondary_Path)));

      if GNU_Info_Needed then
         Write (Self.Primary_FD, Self.Primary_Content, GNU_Header);
      end if;

      for Obj of Cmd_Line.Argument_List (Build.Command_Line.Obj) loop
         if GNU_Info_Needed then
            Write (Self.Primary_FD, Self.Primary_Content, GNU_Opening);
         end if;

         if GNU_Info_Needed then
            Write (Self.Primary_FD, Self.Primary_Content, Format (Obj));
            Write (Self.Primary_FD, Self.Primary_Content, GNU_Closing);

         elsif GNU_Archiver_Info then
            declare
               First : constant Boolean :=
                         (Obj = Cmd_Line.Argument_List
                            (Build.Command_Line.Obj).First_Element);
            begin
               Write
                 (Self.Primary_FD,
                  Self.Primary_Content,
                  (if First then "" else " ") & Format (Obj));
            end;

         else
            Write
              (Self.Primary_FD, Self.Primary_Content, Format (Obj) & ASCII.LF);
         end if;
      end loop;

      if GNU_Info_Needed then
         Write (Self.Primary_FD, Self.Primary_Content, GNU_Footer);
      end if;

      if GCC_Info and then Self.Secondary_FD /= GOF.Invalid_FD then
         for Switch of Self.Resp_File_Switches loop
            Write (Self.Secondary_FD, Self.Secondary_Content, Switch);

            if Switch /= Self.Resp_File_Switches.Last_Element then
               Write
                 (Self.Secondary_FD, Self.Secondary_Content, "" & ASCII.LF);
            end if;
         end loop;

         Write
           (Self.Secondary_FD,
            Self.Secondary_Content,
            Format (-Self.Primary_Path));
         Write (Self.Secondary_FD, Self.Secondary_Content, "" & ASCII.LF);

         for Other of Cmd_Line.Argument_List (Build.Command_Line.Other) loop
            if not Cmd_Line.Argument_List.Contains (Other) then
               Write
                 (Self.Secondary_FD,
                  Self.Secondary_Content,
                  Format (Other) & ASCII.LF);
            end if;
         end loop;
      end if;
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
      Max_Length : Natural;
      Switches   : Containers.Source_Value_List)
   is

      function Define_Switches return Build.Command_Line.Args_Vector.Vector;

      ---------------------
      -- Define_Switches --
      ---------------------

      function Define_Switches return Build.Command_Line.Args_Vector.Vector
      is
         V : Build.Command_Line.Args_Vector.Vector;
      begin
         for Val of Switches loop
            V.Append (Val.Text);
         end loop;

         return V;
      end Define_Switches;

   begin
      Self.Format              := Format;
      Self.Kind                := Kind;
      Self.Max_Cmd_Line_Length := Max_Length;
      Self.Resp_File_Switches  := Define_Switches;
   end Initialize;

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
         Self.Secondary_Path := +Path;
         Self.Secondary_Content.Append (String (Path));
         Self.Has_Secondary_Content := True;
      else
         Self.Primary_FD   := FD;
         Self.Primary_Path := +Path;
         Self.Primary_Content.Append (String (Path));
         Self.Has_Primary_Content := True;
      end if;
   end Register;

   -----------
   -- Write --
   -----------

   procedure Write
     (FD     : GNATCOLL.OS.FS.File_Descriptor;
      Vector : in out GNATCOLL.OS.Process.Argument_List;
      Buffer : String) is
   begin
      GOF.Write (FD, Buffer);
      Vector.Append (Buffer);
   end Write;


end GPR2.Build.Response_Files;
