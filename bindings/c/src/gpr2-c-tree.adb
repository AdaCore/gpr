------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNATCOLL.JSON;
with GPR2.C.JSON; use GPR2.C.JSON;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source_Reference;

package body GPR2.C.Tree is

   ---------------------------------------
   -- GPR2_Project_Tree_Add_Tool_Prefix --
   ---------------------------------------

   function GPR2_Project_Tree_Add_Tool_Prefix
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Full_Tool_Name : constant Name_Type :=
                            GPR2.Project.Tree.Add_Tool_Prefix
                              (Get_Project_Tree (Request, "tree_id").all,
                               Name_Type (Get_String (Request, "tool_name")));
      begin
         Set_String (Result, "full_tool_name", String (Full_Tool_Name));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Add_Tool_Prefix;

   -------------------------------
   -- GPR2_Project_Tree_Context --
   -------------------------------

   function GPR2_Project_Tree_Context
     (Request : C_Request;
      Answer  : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      -------------
      -- Handler --
      -------------

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree : constant Project_Tree_Access :=
                  Get_Project_Tree (Request, "tree_id");
      begin
         Set_Context (Result, "context", Tree.all.Context);
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Context;

   --------------------------------
   -- GPR2_Project_Tree_Get_File --
   --------------------------------

   function GPR2_Project_Tree_Get_File
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree             : constant Project_Tree_Access :=
                              Get_Project_Tree (Request, "tree_id");
         Base_Name        : constant Simple_Name :=
                              Simple_Name (Get_String (Request, "base_name"));
         View             : constant Project_View_Access :=
                              Get_Optional_Project_View (Request, "view_id");
         Use_Source_Path  : constant Boolean :=
                              Get_Boolean (Request, "use_source_path", True);
         Use_Object_Path  : constant Boolean :=
                              Get_Boolean (Request, "use_object_path", True);
         Predefined_Only  : constant Boolean :=
                              Get_Boolean (Request, "predefined_only", False);
         Return_Ambiguous : constant Boolean :=
                              Get_Boolean (Request, "return_ambiguous", True);
         File             : GPR2.Path_Name.Object;
      begin
         if View = null then
            File := GPR2.Project.Tree.Get_File
              (Self             => Tree.all,
               Base_Name        => Base_Name,
               Use_Source_Path  => Use_Source_Path,
               Use_Object_Path  => Use_Object_Path,
               Predefined_Only  => Predefined_Only,
               Return_Ambiguous => Return_Ambiguous);
         else
            File := GPR2.Project.Tree.Get_File
              (Self             => Tree.all,
               Base_Name        => Base_Name,
               View             => View.all,
               Use_Source_Path  => Use_Source_Path,
               Use_Object_Path  => Use_Object_Path,
               Predefined_Only  => Predefined_Only,
               Return_Ambiguous => Return_Ambiguous);
         end if;

         Set_String (Result, "filename", String (File.Value));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Get_File;

   --------------------------------
   -- GPR2_Project_Tree_Get_View --
   --------------------------------

   function GPR2_Project_Tree_Get_View
      (Request : C_Request;
       Answer  : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      -------------
      -- Handler --
      -------------

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree : constant Project_Tree_Access :=
                  Get_Project_Tree (Request, "tree_id");
         View : Project_View_Access := new GPR2.Project.View.Object;
      begin
         View.all := Tree.Get_View
           (GPR2.Name_Type (Get_String (Request, "unit")));

         Set_Project_View (Result, "view_id", View);
      end Handler;

   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Get_View;

   ------------------------------------------
   -- GPR2_Project_Tree_Invalidate_Sources --
   ------------------------------------------

   function GPR2_Project_Tree_Invalidate_Sources
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         pragma Unreferenced (Result);
         Tree : constant Project_Tree_Access :=
                  Get_Project_Tree (Request, "tree_id");
         View : constant Project_View_Access :=
                  Get_Optional_Project_View (Request, "view_id");
      begin
         if View = null then
            GPR2.Project.Tree.Invalidate_Sources (Self => Tree.all);
         else
            GPR2.Project.Tree.Invalidate_Sources (Self => Tree.all,
                                                  View => View.all);
         end if;

      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Invalidate_Sources;

   -------------------------------------------
   -- GPR2_Project_Tree_Language_Properties --
   -------------------------------------------

   function GPR2_Project_Tree_Language_Properties
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree     : constant Project_Tree_Access :=
                      Get_Project_Tree (Request, "tree_id");
         Language : constant Optional_Name_Type :=
                      Optional_Name_Type (Get_String (Obj => Request,
                                                      Key => "language",
                                                      Default => "ada"));
      begin
         Set_Optional_Name (Result, "runtime", Tree.Runtime (Language));
         Set_Name (Result, "object_suffix", Tree.Object_Suffix (Language));
         Set_Name (Result, "dependency_suffix",
                   Tree.Dependency_Suffix (Language));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Language_Properties;

   ----------------------
   -- GPR2_Project_Load--
   ----------------------

   function GPR2_Project_Tree_Load
      (Request : C_Request;
       Answer  : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree : constant Project_Tree_Access := new GPR2.Project.Tree.Object;

         Filename : constant Path_Name.Object :=
            Get_File_Path (Request, "filename");

         Context : constant GPR2.Context.Object :=
            Get_Context (Request, "context");

         Config : constant GPR2.Project.Configuration.Object :=
            Get_Project_Configuration (Request);

         Build_Path : constant Path_Name.Object :=
            Get_Optional_Dir_Path (Request, "build_path");

         Subdirs : constant Optional_Name_Type :=
            Get_Optional_Name (Request, "subdirs");

         Src_Subdirs : constant Optional_Name_Type :=
            Get_Optional_Name (Request, "src_subdirs");

         Check_Shared_Lib : constant Boolean :=
            Get_Boolean (Request, "check_shared_lib", True);

         Project_Dir : constant Path_Name.Object :=
            Get_Optional_Dir_Path (Request, "project_dir");

         Absent_Dir_Error : constant Boolean :=
            Get_Boolean (Request, "absent_dir_error", False);

         Implicit_With : constant Containers.Name_Set :=
            Get_Name_Set (Request, "implicit_with");

         Target : constant Optional_Name_Type :=
            Get_Optional_Name (Request, "target");

         Language_Runtimes : constant Containers.Name_Value_Map :=
            Get_Name_Value_Map (Request, "language_runtimes");

      begin

         if Config.Is_Defined then
            GPR2.Project.Tree.Load
              (Self             => Tree.all,
               Filename         => Filename,
               Context          => Context,
               Config           => Config,
               Build_Path       => Build_Path,
               Subdirs          => Subdirs,
               Src_Subdirs      => Src_Subdirs,
               Check_Shared_Lib => Check_Shared_Lib,
               Project_Dir      => Project_Dir,
               Absent_Dir_Error => Absent_Dir_Error,
               Implicit_With    => Implicit_With);
         else
            GPR2.Project.Tree.Load_Autoconf
              (Self              => Tree.all,
               Filename          => Filename,
               Context           => Context,
               Build_Path        => Build_Path,
               Subdirs           => Subdirs,
               Src_Subdirs       => Src_Subdirs,
               Check_Shared_Lib  => Check_Shared_Lib,
               Project_Dir       => Project_Dir,
               Absent_Dir_Error  => Absent_Dir_Error,
               Implicit_With     => Implicit_With,
               Target            => Target,
               Language_Runtimes => Language_Runtimes);
         end if;
         Set_Project_Tree (Result, "tree_id", Tree);
      end Handler;

   begin

      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Load;

   ------------------------------------
   -- GPR2_Project_Tree_Log_Messages --
   ------------------------------------

   function GPR2_Project_Tree_Log_Messages
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree                   : constant Project_Tree_Access :=
                                  Get_Project_Tree (Request, "tree_id");
         Information            : constant Boolean :=
                                  Get_Boolean (Request, "information", True);
         Warning                : constant Boolean :=
                                  Get_Boolean (Request, "warning", True);
         Error                  : constant Boolean :=
                                  Get_Boolean (Request, "error", True);
         Read                   : constant Boolean :=
                                  Get_Boolean (Request, "read", True);
         Unread               : constant Boolean :=
                                  Get_Boolean (Request, "unread", True);
         Message_Array : GNATCOLL.JSON.JSON_Array;
         Messages : constant GNATCOLL.JSON.JSON_Value :=
            GNATCOLL.JSON.Create (Message_Array);
      begin
         if Tree.all.Has_Messages then
            for C in Tree.all.Log_Messages.Iterate (Information => Information,
                                                    Warning     => Warning,
                                                    Error       => Error,
                                                    Read        => Read,
                                                    Unread      => Unread)
            loop
               Add_Message (Messages, GPR2.Log.Element (C));
            end loop;
         end if;
         GNATCOLL.JSON.Set_Field (Result, "messages", Messages);
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Log_Messages;

   --------------------------------------------
   -- GPR2_Project_Tree_Project_Search_Paths --
   --------------------------------------------

   function GPR2_Project_Tree_Project_Search_Paths
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
      begin
         Set_Path_Name_Set_Object
           (Obj => Result,
            Key => "project_search_paths",
            Set => GPR2.Project.Tree.Project_Search_Paths
              (Self => Get_Project_Tree (Request, "tree_id").all));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Project_Search_Paths;

   ----------------------------------
   -- GPR2_Project_Tree_Properties --
   ----------------------------------

   function GPR2_Project_Tree_Properties
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree           : constant Project_Tree_Access :=
                            Get_Project_Tree (Request, "tree_id");
      begin
         --  By construction the tree is always defined
         Set_Name (Result, "target", GPR2.Project.Tree.Target (Tree.all));
         Set_Name (Result, "archive_suffix",
                   GPR2.Project.Tree.Archive_Suffix (Tree.all));
         Set_Optional_Name (Result, "src_subdirs",
                            GPR2.Project.Tree.Src_Subdirs (Tree.all));
         Set_Optional_Name (Result, "subdirs",
                            GPR2.Project.Tree.Subdirs (Tree.all));
         Set_Path (Result, "build_path",
                   GPR2.Project.Tree.Build_Path (Tree.all));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Properties;

   ----------------------------------------------------
   -- GPR2_Project_Tree_Register_Project_Search_Path --
   ----------------------------------------------------

   function GPR2_Project_Tree_Register_Project_Search_Path
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         pragma Unreferenced (Result);
      begin
         GPR2.Project.Tree.Register_Project_Search_Path
           (Self => Get_Project_Tree (Request, "tree_id").all,
            Dir  => Get_File_Path (Request, "dir"));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Register_Project_Search_Path;

   ------------------------------------
   -- GPR2_Project_Tree_Root_Project --
   ------------------------------------

   function GPR2_Project_Tree_Root_Project
      (Request : C_Request;
       Answer  : out C_Answer)
      return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree : constant Project_Tree_Access :=
                  Get_Project_Tree (Request, "tree_id");
         View : Project_View_Access := new GPR2.Project.View.Object;
      begin
         View.all := Tree.Root_Project;
         Set_Project_View (Result, "view_id", View);
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Root_Project;

   -----------------------------------
   -- GPR2_Project_Tree_Set_Context --
   -----------------------------------

   function GPR2_Project_Tree_Set_Context
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         pragma Unreferenced (Result);
      begin
         GPR2.Project.Tree.Set_Context
           (Self    => Get_Project_Tree (Request, "tree_id").all,
            Context => Get_Context (Request, "context"));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Set_Context;

   ------------------------------
   -- GPR2_Project_Tree_Unload --
   ------------------------------

   function GPR2_Project_Tree_Unload
      (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         Tree : Project_Tree_Access;
         pragma Unreferenced (Result);

         procedure Free is new Ada.Unchecked_Deallocation
            (GPR2.Project.Tree.Object, Project_Tree_Access);

      begin
         Tree := Get_Project_Tree (Request, "tree_id");
         GPR2.Project.Tree.Unload (Tree.all);
         Free (Tree);
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Unload;

   --------------------------------------
   -- GPR2_Project_Tree_Update_Sources --
   --------------------------------------

   function GPR2_Project_Tree_Update_Sources
     (Request : C_Request; Answer : out C_Answer) return C_Status
   is
      procedure Handler (Request : JSON_Value; Result : JSON_Value);

      procedure Handler (Request : JSON_Value; Result : JSON_Value)
      is
         pragma Unreferenced (Result);
      begin
         GPR2.Project.Tree.Update_Sources
           (Self          => Get_Project_Tree (Request, "tree_id").all,
            Stop_On_Error => Get_Boolean (Request, "stop_on_error", True));
      end Handler;
   begin
      return Bind (Request, Answer, Handler'Unrestricted_Access);
   end GPR2_Project_Tree_Update_Sources;

end GPR2.C.Tree;
