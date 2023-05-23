with Ada.Text_IO;
with Ada.Containers.Vectors;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with GPR2.Path_Name;
with GPR2.Build.Tree_Db;
with GPR2.Build.View_Db;
with GPR2.Build.Source.Sets;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Test;
with Objects; use Objects;

package body Scenario is

   use GNATCOLL.VFS;
   use GPR2;
   use GPR2.Build;

   Tok_Load    : constant XString := To_XString ("load");
   --  Load the tree

   Tok_Unload  : constant XString := To_XString ("unload");
   --  Unload the tree

   Tok_Refresh : constant XString := To_XString ("refresh");
   --  Refresh the build db

   Tok_Dump    : constant XString := To_XString ("dump");
   --  Refresh the build db

   Tok_Mkdir       : constant XString := To_XString ("mkdir");
   --  Create a directory

   Tok_Copy        : constant XString := To_XString ("copy");
   --  Copy a file

   Tok_Create_Spec      : constant XString := To_XString ("create_spec");
   Tok_Create_Body      : constant XString := To_XString ("create_body");
   Tok_Create_Separate  : constant XString := To_XString ("create_separate");
   Tok_Create_Proc_Body : constant XString := To_XString ("create_proc_body");

   Tok_Remove  : constant XString := To_XString ("remove");
   --  Remove a file

   Ctx         : Context.Object := Context.Empty;

   package VFS_Vectors is new Ada.Containers.Vectors (Positive, Virtual_File);

   -------------
   -- Execute --
   -------------

   procedure Execute (Path : String)
   is
      function To_File (Token : XString) return Virtual_File;

      -------------
      -- To_File --
      -------------

      function To_File (Token : XString) return Virtual_File
      is
      begin
         return Path_Name.Create_File
           (Filename_Type (Token.To_String)).Virtual_File;
      end To_File;

      Scenario_Path : constant Virtual_File := To_File (To_XString (Path));
      Scenario      : constant XString := Scenario_Path.Read_File;
      Lines         : constant XString_Array := Split (Scenario, ASCII.LF);
      Success       : Boolean;
      Old_Dir       : constant Virtual_File := Get_Current_Dir;
      Artifacts     : VFS_Vectors.Vector;
      Loaded        : Boolean := False;
      Log           : GPR2.Log.Object;

   begin
      Scenario_Path.Get_Parent.Change_Dir;
      Ada.Text_IO.Put_Line ("=======================================");
      Ada.Text_IO.Put_Line ("Executing " & Path);
      Ada.Text_IO.Put_Line ("=======================================");
      Ada.Text_IO.New_Line;

      for L of Lines loop
         declare
            Tokens : constant XString_Array := L.Trim.Split (' ');
            Cmd    : XString;

         begin
            if Tokens'Length > 0 and then not Tokens (1).Is_Empty and then Tokens (1).To_String /= "#" then
               Cmd := Tokens (1).To_Lower;

               if Cmd = Tok_Load then
                  Ada.Text_IO.Put_Line
                    ("--- Loading project " & Tokens (2).To_String);

                  if Loaded then
                     Tree.Unload (True);
                     Loaded := False;
                  end if;

                  begin
                     Project.Tree.Load_Autoconf
                       (Tree,
                        Path_Name.Create_File
                          (Filename_Type (Tokens (2).To_String)),
                        Ctx);
                     Tree.Log_Messages.Output_Messages (Information => False);

                  exception
                     when GPR2.Project_Error =>
                        Tree.Log_Messages.Output_Messages
                          (Information => False);

                        exit;
                  end;

                  Tree.Update_Sources (Messages => Log);
                  Log.Output_Messages (Information => False);
                  Loaded := True;

               elsif Cmd = Tok_Unload then
                  Ada.Text_IO.Put_Line
                    ("--- Unloading project");
                  Tree.Unload (Full => True);
                  Loaded := False;

               elsif Cmd = Tok_Refresh then
                  Ada.Text_IO.Put_Line
                    ("--- Refresh list of sources");
                  Tree.Update_Sources (Messages => Log);
                  Log.Output_Messages (Information => False);

               elsif Cmd = Tok_Dump then
                  Ada.Text_IO.Put_Line ("-----------------------------------");
                  Test.Dump;
                  Ada.Text_IO.Put_Line ("-----------------------------------");

               elsif Cmd = Tok_Mkdir then
                  Ada.Text_IO.Put_Line
                    ("--- mkdir " & Tokens (2).To_String);
                  declare
                     D : constant Virtual_File := To_File (Tokens (2));
                  begin
                     D.Ensure_Directory;
                     D.Make_Dir;
                     Artifacts.Prepend (D);
                  end;

               elsif Cmd = Tok_Copy then
                  Ada.Text_IO.Put_Line
                    ("--- cp " & Tokens (2).To_String & " " &
                       Tokens (3).To_String);

                  declare
                     Src : constant Virtual_File := To_File (Tokens (2));
                     Dest : Virtual_File := To_File (Tokens (3));
                  begin
                     if Dest.Is_Directory then
                        Dest := Dest.Create_From_Dir (Src.Base_Name);
                     end if;

                     Src.Copy (Dest.Full_Name, Success);
                     exit when not Success;
                     Artifacts.Prepend (Dest);
                  end;

               elsif Cmd = Tok_Remove then
                  Ada.Text_IO.Put_Line ("--- rm " & Tokens (2).To_String);

                  declare
                     Path : constant Virtual_File :=
                              To_File (Tokens (2));
                  begin
                     Path.Delete (Success);

                     if Success and then not Artifacts.Is_Empty then
                        for Idx in 1 .. Positive (Artifacts.Last_Index) loop
                           if Artifacts (Idx) = Path then
                              Artifacts.Delete (Idx);
                              exit;
                           end if;
                        end loop;
                     end if;
                  end;

               elsif Cmd = Tok_Create_Spec then
                  Ada.Text_IO.Put_Line ("--- create spec " & Tokens (3).To_String);
                  declare
                     Unit_Name : constant String := Tokens (2).To_String;
                     File      : constant Virtual_File := To_File (Tokens (3));
                     Path      : constant Filesystem_String := File.Full_Name;
                     Fd        : Ada.Text_IO.File_Type;
                     use Ada.Text_IO;
                  begin
                     Ada.Text_IO.Create
                       (Fd,
                        Ada.Text_IO.Out_File,
                        String (Path));
                     Put_Line (Fd, "package " & Unit_Name & " is");
                     Put_Line (Fd, "end " & Unit_Name & ";");
                     Close (Fd);
                     Artifacts.Prepend (File);
                  end;

               elsif Cmd = Tok_Create_Body then
                  Ada.Text_IO.Put_Line ("--- create body " & Tokens (3).To_String);
                  declare
                     Unit_Name : constant String := Tokens (2).To_String;
                     File      : constant Virtual_File := To_File (Tokens (3));
                     Path      : constant Filesystem_String := File.Full_Name;
                     Fd        : Ada.Text_IO.File_Type;
                     use Ada.Text_IO;
                  begin
                     Ada.Text_IO.Create
                       (Fd,
                        Ada.Text_IO.Out_File,
                        String (Path));
                     Put_Line (Fd, "package body " & Unit_Name & " is");
                     Put_Line (Fd, "end " & Unit_Name & ";");
                     Close (Fd);
                     Artifacts.Prepend (File);
                  end;

               elsif Cmd = Tok_Create_Separate then
                  Ada.Text_IO.Put_Line ("--- create separate " & Tokens (4).To_String);
                  declare
                     Unit_Name : constant String := Tokens (2).To_String;
                     Sep_Name  : constant String := Tokens (3).To_String;
                     File      : constant Virtual_File := To_File (Tokens (4));
                     Path      : constant Filesystem_String := File.Full_Name;
                     Fd        : Ada.Text_IO.File_Type;
                     use Ada.Text_IO;
                  begin
                     Ada.Text_IO.Create
                       (Fd,
                        Ada.Text_IO.Out_File,
                        String (Path));
                     Put_Line (Fd, "separate (" & Unit_Name & ")");
                     Put_Line (Fd, "package body " & Sep_Name & " is");
                     Put_Line (Fd, "end " & Sep_Name & ";");
                     Close (Fd);
                     Artifacts.Prepend (File);
                  end;

               elsif Cmd = Tok_Create_Proc_Body then
                  Ada.Text_IO.Put_Line ("--- create procedure body " &
                                          Tokens (3).To_String);
                  declare
                     Unit_Name : constant String := Tokens (2).To_String;
                     File      : constant Virtual_File := To_File (Tokens (3));
                     Path      : constant Filesystem_String := File.Full_Name;
                     Fd        : Ada.Text_IO.File_Type;
                     use Ada.Text_IO;
                  begin
                     Ada.Text_IO.Create
                       (Fd,
                        Ada.Text_IO.Out_File,
                        String (Path));
                     Put_Line (Fd, "procedure " & Unit_Name & " is");
                     Put_Line (Fd, "begin");
                     Put_Line (Fd, "end " & Unit_Name & ";");
                     Close (Fd);
                     Artifacts.Prepend (File);
                  end;

               else
                  Ada.Text_IO.Put_Line
                    ("Unexpected command " & Tokens (1).To_String);
                  exit;
               end if;
            end if;
         end;
      end loop;

      Ada.Text_IO.New_Line;

      while not Artifacts.Is_Empty loop
         declare
            Path : constant Virtual_File := Artifacts.First_Element;
         begin
            Artifacts.Delete_First;
            if Path.Is_Directory then
               Path.Remove_Dir (Recursive => True, Success => Success);
            else
               Path.Delete (Success);
            end if;
         end;
      end loop;

      if Loaded then
         Tree.Unload;
      end if;

      Old_Dir.Change_Dir;
   end Execute;

end Scenario;
