------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2026, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Exceptions;
with Ada.Text_IO;

with GNATCOLL.JSON;

with GPRtools.Options;
with GPRtools.Program_Termination;

with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree.Inspect;
with GPR2.Project.View;
with GPR2.Reporter;
with GPR2.Version;
with GPR2.View_Ids;

procedure GPRinspect.Process
  (Options : in out GPRinspect.GPRinspect_Options)
is

   use Ada;
   use GNATCOLL;
   use GNATCOLL.JSON;
   use GPR2;
   use GPR2.View_Ids;
   use GPRtools.Program_Termination;
   use type GPRtools.Display_Kind;
   use type Project.Registry.Attribute.Value_Kind;

   package PRA renames Project.Registry.Attribute;

   procedure Display_Exception_JSON_Output
     (JSON_Res    : JSON_Value;
      Except      : Ada.Exceptions.Exception_Occurrence);

   procedure Display_Messages_JSON_Output
     (JSON_Res    : JSON_Value;
      Conf_Logs   : GPR2.Log.Object;
      Tree_Logs   : GPR2.Log.Object;
      Only_Errors : Boolean)
   with Pre => JSON_Res.Kind = JSON_Object_Type;
   --  Add a "messages" field to the provided JSON object,
   --  and fill it with the tree messages. If Only_Errors
   --  is true, only errors are added.

   procedure Indent (Level : Natural; Item : String);
   --  Output indented text item. First level of indentation does not
   --  have a prefix, the second one is prefixed with a "*", and
   --  others with a "-".

   procedure Display_Messages_Textual_Output
     (Conf_Logs   : GPR2.Log.Object;
      Tree_Logs   : GPR2.Log.Object;
      Only_Errors : Boolean);
   --  Display tree messages. If Only_Errors is true, only errors
   --  are displayed.

   procedure Inspect_Project_Textual_Output (Tree : Project.Tree.Object);
   --  Inspect project and possibly recursively all imports

   function View_Id (View : Project.View.Object) return String;
   --  Get the View's View_Id image

   function No_View_Restriction
     (Views : Restricted_Scope;
      VName : Name_Type) return Boolean;
   --  Return if the view must be processed and displayed or not.

   procedure Display_Exception_JSON_Output
     (JSON_Res    : JSON_Value;
      Except      : Ada.Exceptions.Exception_Occurrence)
   is
      Except_Obj     : constant JSON_Value := Create_Object;
   begin
      Set_Field (Except_Obj,
                 Ada.Exceptions.Exception_Name (Except),
                 Ada.Exceptions.Exception_Message (Except));
      Set_Field (JSON_Res, "exception", Except_Obj);
   end Display_Exception_JSON_Output;

   ----------------------------------
   -- Display_Messages_JSON_Output --
   ----------------------------------

   procedure Display_Messages_JSON_Output
     (JSON_Res    : JSON_Value;
      Conf_Logs   : GPR2.Log.Object;
      Tree_Logs   : GPR2.Log.Object;
      Only_Errors : Boolean)
   is
      procedure Populate_Array
        (Arr  : in out JSON_Array;
         Logs : GPR2.Log.Object);
      --  Populate a JSON array with Logs messages.

      --------------------
      -- Populate_Array --
      --------------------

      procedure Populate_Array
        (Arr  : in out JSON_Array;
         Logs : GPR2.Log.Object) is
      begin
         for C in Logs.Iterate
           (Hint     => (not Only_Errors) and then Options.Verbose,
            Warning  => not Only_Errors,
            Error    => True,
            Lint     => not Only_Errors,
            Read     => False,
            Unread   => True)
         loop
            declare
               M : constant Message.Object := GPR2.Log.Element (C);
            begin
               Append (Arr, Create (M.Format));
            end;
         end loop;
      end Populate_Array;

      Messages_Obj   : constant JSON_Value := Create_Object;
      Tree_Mes_Array : JSON_Array;
      Conf_Mes_Array : JSON_Array;

   begin
      Populate_Array (Conf_Mes_Array, Conf_Logs);
      Populate_Array (Tree_Mes_Array, Tree_Logs);

      Set_Field (Messages_Obj, "configuration", Conf_Mes_Array);
      Set_Field (Messages_Obj, "tree", Tree_Mes_Array);
      Set_Field (JSON_Res, "messages", Messages_Obj);
   end Display_Messages_JSON_Output;

   -------------------------------------
   -- Display_Messages_Textual_Output --
   -------------------------------------

   procedure Display_Messages_Textual_Output
     (Conf_Logs   : GPR2.Log.Object;
      Tree_Logs   : GPR2.Log.Object;
      Only_Errors : Boolean)
   is
      procedure Display (Logs : GPR2.Log.Object);

      -------------
      -- Display --
      -------------

      procedure Display (Logs : GPR2.Log.Object) is
      begin
         for C in Logs.Iterate
           (Hint    => (not Only_Errors) and then Options.Verbose,
            Warning => not Only_Errors,
            Error   => True,
            Lint    => not Only_Errors,
            Read    => False,
            Unread  => True)
         loop
            Indent (2, GPR2.Log.Element (C).Format);
         end loop;
      end Display;

   begin
      Indent (0, "+--------------------------------------+");
      Indent (0, "|               Messages               |");
      Indent (0, "+--------------------------------------+");

      if Conf_Logs.Is_Defined and then not Conf_Logs.Is_Empty then
         Indent (1, "Configuration:");
         Display (Conf_Logs);
      end if;

      Indent (1, "Tree:");
      Display (Tree_Logs);
      Ada.Text_IO.New_Line;
   end Display_Messages_Textual_Output;

   ------------
   -- Indent --
   ------------

   procedure Indent (Level : Natural; Item : String) is
      Prefix : constant String :=
                 (case Level is
                    when 0      => "",
                    when 1      => "* ",
                    when others => (1 .. (Level - 1) * 3 => ' ') & " - ");

   begin
      Text_IO.Put_Line (Prefix & Item);
   end Indent;

   ------------------------------------
   -- Inspect_Project_Textual_Output --
   ------------------------------------

   procedure Inspect_Project_Textual_Output (Tree : Project.Tree.Object) is

      procedure Print_Infos;
      procedure Print_Projects;
      procedure Print_Tree;

      function Image
        (V : GPR2.Project.View.Object) return String
      is (String (V.Name) & " (" & View_Id (V) & ")");

      -----------------
      -- Print_Infos --
      -----------------

      procedure Print_Infos is
      begin
         Indent (0, "+--------------------------------------+");
         Indent (0, "|         General Information          |");
         Indent (0, "+--------------------------------------+");
         Indent (1, "Generated on : " &
                   Calendar.Formatting.Image (Calendar.Clock));
         Indent (1, "Version      : " &
                   GPR2.Version.Long_Value);
      end Print_Infos;

      --------------------
      -- Print_Projects --
      --------------------

      procedure Print_Projects is
         First_Attr   : Boolean := True;
         Ident_Offset : Natural := 0;

         procedure Print_Attributes
           (View   : GPR2.Project.View.Object;
            Pack   : GPR2.Package_Id;
            Offset : Natural := 0);

         ----------------------
         -- Print_Attributes --
         ----------------------

         procedure Print_Attributes
           (View   : GPR2.Project.View.Object;
            Pack   : GPR2.Package_Id;
            Offset : Natural := 0) is
         begin
            for Attr of View.Attributes
              (Pack, With_Config => Options.Display_Config_Attributes)
            loop
               if First_Attr then
                  --  Actually has attributes to display, print the
                  --  category
                  Indent (2 + Offset, "Attributes        : ");
                  First_Attr := False;
               end if;

               Indent (3 + Offset, Image (Attr.Name.Id) &
                         " [ " & Attr.Kind'Img & " ]");

               if Attr.Has_Index then
                  Indent (4 + Offset, "Index value : """ &
                            Attr.Index.Value & '"');
               end if;

               if Attr.Kind = PRA.Single  then
                  Indent (4 + Offset, "Value : """ & Attr.Value.Text & '"');

               elsif Attr.Kind = PRA.List then
                  Indent (4 + Offset, "Values : ");

                  for V of Attr.Values loop
                     Indent (5 + Offset, '"' & V.Text & '"');
                  end loop;
               end if;
            end loop;
         end Print_Attributes;

      begin
         Indent (0, "+--------------------------------------+");
         Indent (0, "|         Projects Information         |");
         Indent (0, "+--------------------------------------+");

         for V in Tree.Iterate loop
            declare
               View : constant Project.View.Object := Project.Tree.Element (V);

            begin
               if (Options.All_Projects
                   and then No_View_Restriction
                     (Views => Options.Restricted_Views,
                      VName => View.Name))
                 or else (View_Id (View) = View_Id (Tree.Root_Project))
               then
                  Indent (1, Image (View) & " [ " & Image (View.Kind) & " ]");
                  Indent (2, "Project file      : " &
                            View.Path_Name.String_Value);
                  Indent (2, "Project directory : " &
                            String (View.Path_Name.Dir_Name));

                  if View.Kind in With_Object_Dir_Kind then
                     Indent (2, "Object directory  : " &
                               String (View.Object_Directory.Dir_Name));
                  end if;

                  if View.Kind in With_Source_Dirs_Kind then
                     Indent (2, "Source directory  :");

                     for S of View.Source_Directories loop
                        Indent (3, S.String_Value);
                     end loop;
                  end if;

                  if View.Is_Library then
                     Indent (2, "Library name      : " &
                               String (View.Library_Name));
                     Indent (2, "Library file      : " &
                               View.Library_Filename.String_Value);
                     Indent (2, "Library directory : " &
                               String (View.Library_Directory.Dir_Name));
                     Indent (2, "Library ALI dir.  : " &
                               String (View.Library_Ali_Directory.Dir_Name));
                  end if;

                  if View.Is_Extended then
                     Indent (2, "Extended by       : " &
                               Image (View.Extending));
                  end if;

                  if View.Is_Extending then
                     Indent (2, "Extends           : ");

                     for Extended_V of View.Extended loop
                        Indent (3, Image (Extended_V));
                     end loop;
                  end if;

                  declare
                     First_Import : Boolean := True;
                  begin
                     for I_V in Tree.Iterate loop
                        declare
                           I_View : constant Project.View.Object :=
                                      Project.Tree.Element (I_V);

                        begin
                           if I_View.Id /= View.Id
                             and then I_View.Has_Imports
                           then
                              for I of I_View.Imports loop
                                 if I.Id = View.Id then
                                    if First_Import then
                                       Indent (2, "Imported-by        : ");
                                       First_Import := False;
                                    end if;

                                    Indent (3, Image (I_View));
                                 end if;
                              end loop;
                           end if;
                        end;
                     end loop;
                  end;

                  if View.Has_Imports then
                     Indent (2, "Imports           : ");

                     for I of View.Imports loop
                        Indent (3, Image (I));
                     end loop;
                  end if;

                  if View.Qualifier in Aggregate_Kind then
                     Indent (2, "Aggregated        : ");

                     for A of View.Aggregated (Recursive => False) loop
                        Indent (3, Image (A));
                     end loop;
                  end if;

                  if Options.Display_Attributes
                    or else Options.Display_Everything
                  then
                     First_Attr := True;
                     Print_Attributes
                       (View, Project_Level_Scope);
                  end if;

                  if Options.Display_Packages
                    or else Options.Display_Attributes
                    or else Options.Display_Everything
                  then
                     if Options.Display_Packages
                       or else Options.Display_Everything
                     then
                        Ident_Offset := 1;
                        First_Attr := False;
                        Indent (2, "Packages          :");
                     end if;

                     for P of View.Packages
                       (With_Defaults => False,
                        With_Config   => Options.Display_Config_Attributes)
                     loop
                        if Options.Display_Packages
                          or else Options.Display_Everything
                        then
                           Indent (3, Image (P));
                        end if;

                        if Options.Display_Attributes
                          or else Options.Display_Everything
                        then
                           Print_Attributes (View, P, Ident_Offset);
                        end if;
                     end loop;
                  end if;

                  if Options.Display_Variables
                    or else Options.Display_Everything
                  then
                     if not View.Variables.Is_Empty then
                        Indent (2, "Variables         : ");

                        for Var of View.Variables loop
                           Indent (3, String (Var.Name.Text) &
                                     " [ " & Var.Kind'Img & " ]");

                           if Var.Has_Type then
                              Indent (4, "Variable type : """ &
                                        String (Var.Typ.Name.Text) & '"');
                           end if;

                           if Var.Kind = PRA.Single then
                              Indent (4, "Value : """ & Var.Value.Text & '"');

                           elsif Var.Kind = PRA.List then
                              Indent (4, "Values : ");

                              for V of Var.Values loop
                                 Indent (5, '"' & V.Text & '"');
                              end loop;
                           end if;
                        end loop;
                     end if;
                  end if;

                  if Options.Display_Variables
                    or else Options.Display_Everything
                  then
                     if not View.Types.Is_Empty then
                        Indent (2, "Types             : ");

                        for T of View.Types loop
                           Indent (3, String (T.Name.Text));
                           Indent (4, "Values : ");

                           for V of T.Values loop
                              Indent (5, '"' & V.Text & '"');
                           end loop;
                        end loop;
                     end if;
                  end if;

                  Text_IO.New_Line;
               end if;
            end;
         end loop;
      end Print_Projects;

      ----------------
      -- Print_Tree --
      ----------------

      procedure Print_Tree is
      begin
         Indent (0, "+--------------------------------------+");
         Indent (0, "|       Project Tree Information       |");
         Indent (0, "+--------------------------------------+");

         if Tree.Has_Messages then
            declare
               First_Message : Boolean := False;
            begin
               for C in Options.Tree.Log_Messages.Iterate
                 (Hint    => Options.Verbose,
                  Warning => True,
                  Error   => True,
                  Lint    => True,
                  Read    => False,
                  Unread  => True)
               loop
                  if not First_Message then
                     Indent (1, "Messages :");
                     First_Message := True;
                  end if;

                  declare
                     M : constant Message.Object := GPR2.Log.Element (C);
                  begin
                     Indent (2, M.Format);
                  end;
               end loop;
            end;
         end if;

         declare
            Project_Count : Integer := 0;
         begin
            for V in Tree.Iterate loop
               declare
                  View : constant Project.View.Object :=
                           Project.Tree.Element (V);
               begin
                  if (Options.All_Projects
                      and then No_View_Restriction
                        (Views => Options.Restricted_Views,
                         VName => View.Name))
                    or else (View_Id (View) = View_Id (Tree.Root_Project))
                  then
                     Project_Count := Project_Count + 1;
                  end if;
               end;
            end loop;

            Indent (1, "Project count        : " & Project_Count'Img);
         end;

         declare
            First_PPath : Boolean := False;
         begin
            for P of Tree.Project_Search_Paths loop
               if not First_PPath then
                  Indent (1, "Project search paths :");
                  First_PPath := True;
               end if;

               Indent (2, String (P.Dir_Name));
            end loop;
         end;

         declare
            First_SPath : Boolean := False;
         begin
            if Tree.Has_Runtime_Project then
               Indent (1, "Object search paths  :");

               for V in Tree.Iterate loop
                  declare
                     View : constant Project.View.Object :=
                              Project.Tree.Element (V);
                  begin
                     if (Options.All_Projects
                         and then No_View_Restriction
                           (Views => Options.Restricted_Views,
                            VName => View.Name))
                       or else (View_Id (View) = View_Id (Tree.Root_Project))
                     then
                        if View.Kind in With_Object_Dir_Kind then
                           Indent (2, String (View.Object_Directory.Dir_Name));
                        end if;
                     end if;
                  end;
               end loop;

               Indent
                 (2, String (Tree.Runtime_Project.Object_Directory.Dir_Name));

               for V in Tree.Iterate loop
                  declare
                     View : constant Project.View.Object :=
                              Project.Tree.Element (V);
                  begin
                     if (Options.All_Projects
                         and then No_View_Restriction
                           (Views => Options.Restricted_Views,
                            VName => View.Name))
                       or else (View_Id (View) = View_Id (Tree.Root_Project))
                     then
                        if View.Kind in With_Source_Dirs_Kind then
                           for S of View.Source_Directories loop
                              if not First_SPath then
                                 Indent (1, "Source search paths  :");
                                 First_SPath := True;
                              end if;

                              Indent (2, S.String_Value);
                           end loop;
                        end if;
                     end if;
                  end;
               end loop;

               for S of Tree.Runtime_Project.Source_Directories loop
                  if not First_SPath then
                     Indent (1, "Source search paths  :");
                     First_SPath := True;
                  end if;

                  Indent (2, String (S.Dir_Name));
               end loop;
            end if;
         end;

         Indent (1, "Root project :");
         Indent (2, Image (Tree.Root_Project));
      end Print_Tree;

      pragma Unreferenced (Tree);

   begin
      Print_Infos;
      Text_IO.New_Line;
      Print_Tree;
      Text_IO.New_Line;
      Print_Projects;
   end Inspect_Project_Textual_Output;

   ----------------------
   -- View_Restriction --
   ----------------------

   function No_View_Restriction
     (Views : Restricted_Scope;
      VName : Name_Type) return Boolean is
   begin
      return (not Views.Restrict or else (Views.Views.Contains (VName)));
   end No_View_Restriction;

   -------------
   -- View_Id --
   -------------

   function View_Id (View : Project.View.Object) return String is
   begin
      return String (GPR2.View_Ids.Image (View.Id));
   end View_Id;

   Verbosity : GPR2.Reporter.Verbosity_Level;
   Success   : Boolean;

begin
   --  Ensure the reporter used to load the tree is quiet: we need to handle
   --  the messages manually

   --  Save the verbosity that was set according to command line
   Verbosity := Options.Console_Reporter.Verbosity;
   Options.Console_Reporter.Set_Verbosity (GPR2.Reporter.Quiet);

   Success := GPRtools.Options.Load_Project
                (Opt                => Options,
                 Absent_Dir_Error   => No_Error,
                 Handle_Errors      => False);

   --  And restore so that we can use the verbosity to manually handle the
   --  logs.

   Options.Console_Reporter.Set_Verbosity (Verbosity);

   declare
      Conf_Logs : constant GPR2.Log.Object :=
                    (if Options.Tree.Has_Configuration
                     then Options.Tree.Configuration.Log_Messages
                     else GPR2.Log.Undefined);
   begin
      case Options.Kind_Of_Display is
         when GPRtools.K_JSON | GPRtools.K_JSON_Compact =>
            declare
               J_Res : constant JSON_Value := Create_Object;
            begin
               Display_Messages_JSON_Output
                 (JSON_Res    => J_Res,
                  Conf_Logs   => Conf_Logs,
                  Tree_Logs   => Options.Tree.Log_Messages.all,
                  Only_Errors => not Success);

               if Success then
                  GPR2.Project.Tree.Inspect.Inspect_Project_JSON_Output
                    (JSON_Res                  => J_Res,
                     Tree                      => Options.Tree,
                     All_Projects              => Options.All_Projects,
                     Display_Everything        => Options.Display_Everything,
                     Display_Attributes        => Options.Display_Attributes,
                     Display_Config_Attributes =>
                       Options.Display_Config_Attributes,
                     Display_Packages          => Options.Display_Packages,
                     Display_Variables         => Options.Display_Variables);
               end if;

               Text_IO.Put_Line
                 (JSON.Write
                    (J_Res, Compact =>
                         Options.Kind_Of_Display = GPRtools.K_JSON_Compact));
            end;

         when GPRtools.K_Textual_IO =>
            Display_Messages_Textual_Output
              (Conf_Logs   => Conf_Logs,
               Tree_Logs   => Options.Tree.Log_Messages.all,
               Only_Errors => not Success);

            if Success then
               Inspect_Project_Textual_Output (Tree => Options.Tree);
            end if;
      end case;
   end;

   if not Success then
      Handle_Program_Termination
        (Message => '"' & String (Options.Project_File.Name)
         & """ processing failed");
   end if;

exception
   when GPRtools.Program_Termination.E_Program_Termination =>
      --  let it go through
      raise;

   when E : others =>
      case Options.Kind_Of_Display is
         when GPRtools.K_JSON | GPRtools.K_JSON_Compact =>
            declare
               J_Res : constant JSON_Value := Create_Object;
            begin
               Display_Exception_JSON_Output
                 (JSON_Res    => J_Res,
                  Except      => E);
               Text_IO.Put_Line
                 (JSON.Write
                    (J_Res, Compact =>
                         Options.Kind_Of_Display = GPRtools.K_JSON_Compact));
            end;
            Handle_Program_Termination
              (Message => "");

         when others =>
            raise;
      end case;
end GPRinspect.Process;
